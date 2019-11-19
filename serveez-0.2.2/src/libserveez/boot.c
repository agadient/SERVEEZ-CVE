/*
 * boot.c - configuration and boot functions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include "networking-headers.h"
#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/interface.h"
#include "libserveez/socket.h"
#include "libserveez/icmp-socket.h"
#include "libserveez/pipe-socket.h"
#include "libserveez/server.h"
#include "libserveez/dynload.h"
#include "libserveez/boot.h"
#include "libserveez/server-core.h"
#include "libserveez/codec/codec.h"
#include "misc-macros.h"

/*
 * The one and only...
 */
static int log_verbosity;

/*
 * Library private dynamic state.
 */
svz_private_t *svz_private;

/**
 * Return a list (length saved to @var{count}) of strings
 * representing the features compiled into libserveez.
 */
const char * const *
svz_library_features (size_t *count)
{
  static const char * const features[] = {
#ifdef ENABLE_DEBUG
    "debug",
#endif
#ifdef ENABLE_HEAP_COUNT
    "heap-counters",
#endif
#ifdef ENABLE_IFLIST
    "interface-list",
#endif
#if defined ENABLE_POLL && defined HAVE_POLL
    "poll",
#endif
#if defined ENABLE_SENDFILE && defined HAVE_SENDFILE
    "sendfile",
#endif
#ifdef ENABLE_LOG_MUTEX
    "log-mutex",
#endif
#ifdef ENABLE_FLOOD_PROTECTION
    "flood-protection",
#endif
    "core"
  };

  *count = sizeof (features) / sizeof (char *);

  return features;
}

#ifdef __MINGW32__
SBO void svz_icmp_startup (void);
SBO void svz_icmp_cleanup (void);
#endif

/*
 * This routine has to be called once before you could use any of the
 * serveez core library functions.
 */
static int
net_startup (void)
{
#ifdef __MINGW32__
  WSADATA WSAData;

  /* Call this once before using Winsock API.  */
  if (WSAStartup (WINSOCK_VERSION, &WSAData) == SOCKET_ERROR)
    {
      svz_log_net_error ("WSAStartup");
      WSACleanup ();
      return 0;
    }

  /* Startup IP services.  */
  svz_icmp_startup ();

#endif /* __MINGW32__ */

  return 1;
}

/*
 * Shutdown the serveez core library.
 */
static int
net_cleanup (void)
{
#ifdef __MINGW32__
  /* Shutdown IP services.  */
  svz_icmp_cleanup ();

  /* Call this when disconnecting from Winsock API.  */
  if (WSACleanup () == SOCKET_ERROR)
    {
      svz_log_net_error ("WSACleanup");
      return 0;
    }

#endif /* not __MINGW32__ */

  return 1;
}

static void
svz__net_updn (int direction)
{
  (direction
   ? net_startup
   : net_cleanup)
    ();
}

/* These are used only in ‘svz_boot’ and ‘svz_halt’,
   so it's easier to just declare them here.  */

#define UPDN(x)  SBO void svz__ ## x ## _updn (int direction)

UPDN (log);
UPDN (sock_table);
UPDN (bindings);
UPDN (signal);
UPDN (interface);
UPDN (pipe);
UPDN (dynload);
UPDN (codec);
UPDN (config_type);

SBO void svz_portcfg_finalize (void);

/**
 * Initialize the core library.
 * @var{client} is typically a program's @code{argv[0]}.
 * If @code{NULL}, take it to be @samp{anonymous}.
 */
void
svz_boot (char const *client)
{
  svz_private = svz_malloc (sizeof (svz_private_t));
  THE (client) = svz_strdup (client ? client : "anonymous");
  THE (boot) = time (NULL);
  SVZ_RUNPARM_X (MAX_SOCKETS, 100);
  SVZ_RUNPARM_X (VERBOSITY, SVZ_LOG_DEBUG);

#define UP(x)  svz__ ## x ## _updn (1)

  UP (log);
  UP (sock_table);
  UP (bindings);
  UP (signal);
  UP (interface);
  UP (net);
  UP (pipe);
  UP (dynload);
  UP (codec);
  UP (config_type);

#undef UP
}

/**
 * Return the number of seconds since @code{svz_boot} was called,
 * or -1 if @code{svz_boot} has not yet been called.
 */
long
svz_uptime (void)
{
  return svz_private
    ? time (NULL) - THE (boot)
    : -1;
}

static int
bad_runparm (int parm)
{
  svz_log (SVZ_LOG_ERROR, "invalid runtime configuration parameter: %d",
           parm);
  return -1;
}

/**
 * Set or get a runtime parameter.
 * If @var{a} is -1, return the value of runtime parameter
 * @var{b}.  If @var{a} specifies a runtime parameter,
 * set it to @var{b} and return 0.  Otherwise, return -1.
 */
int
svz_runparm (int a, int b)
{
  switch (a)
    {
    case -1:
      switch (b)
        {
        case SVZ_RUNPARM_VERBOSITY:   return log_verbosity;
        case SVZ_RUNPARM_MAX_SOCKETS: return THE (nclient_max);
        default:                      return bad_runparm (b);
        }

    case SVZ_RUNPARM_VERBOSITY:
      log_verbosity = b;
      break;

    case SVZ_RUNPARM_MAX_SOCKETS:
      THE (nclient_max) = b;
      break;

    default:
      return bad_runparm (b);
    }
  return 0;
}

/**
 * Finalization of the core library.
 */
void
svz_halt (void)
{
#define DN(x)  svz__ ## x ## _updn (0)

  svz_portcfg_finalize ();
  DN (config_type);
  DN (codec);
  DN (dynload);
  DN (pipe);
  DN (net);
  DN (interface);
  DN (signal);
  DN (bindings);
  DN (sock_table);
  DN (log);

#undef DN

  svz_free_and_zero (THE (client));
  svz_free_and_zero (svz_private);
}
