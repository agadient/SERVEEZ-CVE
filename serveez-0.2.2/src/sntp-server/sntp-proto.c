/*
 * sntp-proto.c - simple network time protocol implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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
#include <string.h>
#include <time.h>
#include <errno.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_TIME_H && HAVE_DECL_GETTIMEOFDAY
# include <sys/time.h>
# define USE_GETTIMEOFDAY 1
#endif

#ifndef __MINGW32__
# include <sys/types.h>
#endif

#include "networking-headers.h"
#include "libserveez.h"
#include "sntp-proto.h"
#include "unused.h"

/*
 * Simple network time server configuration.
 */
sntp_config_t sntp_config =
{
  0, /* default nothing */
};

/*
 * Defining configuration file associations with key-value-pairs.
 */
svz_key_value_pair_t sntp_config_prototype [] =
{
  SVZ_REGISTER_END ()
};

/*
 * Definition of this server.
 */
svz_servertype_t sntp_server_definition =
{
  "Simple Network Time Protocol server",
  "sntp",
  NULL,
  sntp_init,
  sntp_detect_proto,
  sntp_connect_socket,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  sntp_handle_request,
  SVZ_CONFIG_DEFINE ("sntp", sntp_config, sntp_config_prototype)
};

/*
 * Initialize a SNTP server instance.
 */
int
sntp_init (UNUSED svz_server_t *server)
{
  return 0;
}

/*
 * No protocol detection for TCP/PIPE needed.
 */
int
sntp_detect_proto (UNUSED svz_server_t *server,
                   UNUSED svz_socket_t *sock)
{
  return -1;
}

/* Time offset constant.  */
#define SNTP_TIME_CONSTANT 2208988800u

typedef int (wr_t) (svz_socket_t *, char *, int len);

/*
 * Produces the SNTP reply and sends it.
 */
static int
answer (svz_socket_t *sock, wr_t *wr)
{
  char reply[8];
  unsigned long date;

#if USE_GETTIMEOFDAY

  struct timeval t;
  gettimeofday (&t, NULL);
  date = htonl (SNTP_TIME_CONSTANT + t.tv_sec);
  memcpy (reply, &date, 4);
  date = htonl (t.tv_usec);
  memcpy (&reply[4], &date, 4);
  return wr (sock, reply, 8);

#else /* not USE_GETTIMEOFDAY */

  time_t t = time (NULL);
  date = htonl (SNTP_TIME_CONSTANT + t);
  memcpy (reply, &date, 4);
  return wr (sock, reply, 4);

#endif /* not USE_GETTIMEOFDAY */
}

/*
 * Send our reply immediately for TCP/PIPE bindings and schedule this
 * connection for shutdown.
 */
int
sntp_connect_socket (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  sock->check_request = NULL;
  sock->flags |= SVZ_SOFLG_FINAL_WRITE;
  return answer (sock, svz_sock_write);
}

/*
 * The packet processor for the SNTP server.
 */
int
sntp_handle_request (svz_socket_t *sock,
                     UNUSED char *packet, UNUSED int len)
{
  answer (sock, svz_udp_write);
  return 0;
}
