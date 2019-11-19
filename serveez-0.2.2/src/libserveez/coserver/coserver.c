/*
 * coserver.c - basic internal coserver routines
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

#include "timidity.h"
#include "unused.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <errno.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <signal.h>
#ifndef __MINGW32__
# if HAVE_WAIT_H
#  include <wait.h>
# endif
# if HAVE_SYS_WAIT_H
#  include <sys/wait.h>
# endif
#endif
#include "networking-headers.h"
#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/core.h"
#include "libserveez/hash.h"
#include "libserveez/array.h"
#include "libserveez/pipe-socket.h"
#include "libserveez/server-core.h"
#include "libserveez/coserver/coserver.h"

/* coserver-TODO: include header here */
#include "dns.h"
#include "reverse-dns.h"
#include "ident.h"

#ifdef __MINGW32__
/* define for the thread priority in Win32 */
#define COSERVER_THREAD_PRIORITY THREAD_PRIORITY_IDLE
#endif /* not __MINGW32__ */

#define COSERVER_PACKET_BOUNDARY '\n' /* packet boundary */
#define COSERVER_ID_BOUNDARY     ':'  /* id boundary */

/*
 * This structure contains the type id and the callback
 * pointer of the internal coserver routines where CALLBACK is
 * the actual (blocking) processing routine.
 */
typedef struct
{
  int type;                       /* coserver type id */
  char *name;                     /* name of the internal coserver */
  char * (* callback) (char *);   /* coserver callback */
  int instances;                  /* the amount of coserver instances */
  void (* init) (void);           /* coserver initialization routine */
  long last_start;                /* time stamp of the last instance ‘fork’ */
}
svz_coservertype_t;

/*
 * Both of these variables are for storing the given callbacks which get
 * called when the coservers delivered some result.
 */
static unsigned callback_id = 1;
static svz_hash_t *callbacks = NULL;

/*
 * Internal coserver instances.
 */
static svz_array_t *coservers = NULL;

/*
 * Hash table to map a socket to a coserver.
 */
static svz_hash_t *friendly;

/*
 * Invoke a @var{request} for one of the running internal coservers
 * with type @var{type}.  @var{handle_result} and @var{arg} specify what
 * should happen if the coserver delivers a result.
 */
static void
send_request (int type, const char *request,
              svz_coserver_handle_result_t handle_result,
              void *closure)
{
  size_t n;
  svz_coserver_t *coserver, *current;
  svz_coserver_callback_t *cb;

  /*
   * Go through all coservers and find out which coserver
   * type TYPE is the least busiest.
   */
  coserver = NULL;
  svz_array_foreach (coservers, current, n)
    {
      if (current->type == type)
        {
          if (coserver == NULL
              || current->busy <= coserver->busy)
            coserver = current;
        }
    }

  /* found an appropriate coserver */
  if (coserver)
    {
      /*
       * Create new callback hash entry for this coserver request and
       * put it into the global coserver callback hash.
       */
      cb = svz_malloc (sizeof (svz_coserver_callback_t));
      cb->handle_result = handle_result;
      cb->closure = closure;
      svz_hash_put (callbacks,
                    svz_itoa (callback_id), cb);

      coserver->busy++;
#ifdef __MINGW32__
      EnterCriticalSection (&coserver->sync);
#endif /* __MINGW32__ */
      if (svz_sock_printf (coserver->sock, "%u:%s\n",
                           callback_id, request))
        {
          svz_sock_schedule_for_shutdown (coserver->sock);
        }
      callback_id++;
#ifdef __MINGW32__
      LeaveCriticalSection (&coserver->sync);
      coserver_activate (coserver->type);
#endif /* __MINGW32__ */
    }
}

svz_sock_iv_t *
svz_make_sock_iv (svz_socket_t *sock)
{
  svz_sock_iv_t *rv = svz_malloc (sizeof (svz_sock_iv_t));

  rv->id = sock->id;
  rv->version = sock->version;
  return rv;
}

/* coserver-TODO:
   place an appropiate wrapper function here */

/**
 * Enqueue a request for the reverse DNS coserver
 * to resolve address @var{addr},
 * arranging for callback @var{cb} to be called with two args:
 * the hostname (a string) and the opaque data @var{closure}.
 */
void
svz_coserver_rdns_invoke (svz_address_t *addr,
                          svz_coserver_handle_result_t cb,
                          void *closure)
{
  char buf[64];

  STILL_NO_V6_DAMMIT (addr);
  send_request (SVZ_COSERVER_REVERSE_DNS,
                SVZ_PP_ADDR (buf, addr),
                cb, closure);
}

/**
 * Enqueue a request for the DNS coserver to resolve @var{host},
 * arranging for callback @var{cb} to be called with two args:
 * the ip address in dots-and-numbers notation and the opaque
 * data @var{closure}.
 */
void
svz_coserver_dns_invoke (char *host,
                         svz_coserver_handle_result_t cb,
                         void *closure)
{
  send_request (SVZ_COSERVER_DNS, host, cb, closure);
}

/**
 * Enqueue a request for the ident coserver to resolve the client
 * identity at @var{sock}, arranging for callback @var{cb} to be called
 * with two args: the identity (string) and the opaque data @var{closure}.
 */
void
svz_coserver_ident_invoke (svz_socket_t *sock,
                           svz_coserver_handle_result_t cb,
                           void *closure)
{
  char addrbuf[64];
  char buffer[COSERVER_BUFSIZE];

  snprintf (buffer, COSERVER_BUFSIZE, "%s:%u",
            SVZ_PP_ADDR_PORT (addrbuf, sock->remote_addr, sock->remote_port),
            ntohs (sock->local_port));
  send_request (SVZ_COSERVER_IDENT, buffer, cb, closure);
}

/*
 * This static array contains the coserver structure for each type of
 * internal coserver the core library provides.
 */
static svz_coservertype_t coservertypes[] =
{
  /* coserver-TODO:
     place coserver callbacks and identification here */

  { SVZ_COSERVER_REVERSE_DNS, "reverse dns",
    reverse_dns_handle_request, 1, reverse_dns_init, 0 },

  { SVZ_COSERVER_IDENT, "ident",
    ident_handle_request, 1, NULL, 0},

  { SVZ_COSERVER_DNS, "dns",
    dns_handle_request, 1, NULL, 0 }
};

/**
 * Call @var{func} for each coserver, passing additionally the second arg
 * @var{closure}.  If @var{func} returns a negative value, return immediately
 * with that value (breaking out of the loop), otherwise, return 0.
 */
int
svz_foreach_coserver (svz_coserver_do_t *func, void *closure)
{
  size_t n;
  int rv;
  const svz_coserver_t *coserver;

  svz_array_foreach (coservers, coserver, n)
    {
      if (0 > (rv = func (coserver, closure)))
        return rv;
    }
  return 0;
}

/*
 * This routine gets the coserver hash id from a given response and
 * cuts it from the given response buffer.
 */
static unsigned
get_id (char *response)
{
  char *p = response;
  unsigned id = 0;

  while (*p >= '0' && *p <= '9')
    {
      id *= 10;
      id += *p - '0';
      p++;
    }
  if (*p != COSERVER_ID_BOUNDARY)
    {
      svz_log (SVZ_LOG_WARNING,
               "coserver: invalid protocol character (0x%02x)\n", *p);
      return 0;
    }
  p++;

  while (*p != COSERVER_PACKET_BOUNDARY)
    {
      *response++ = *p++;
    }
  *response = '\0';
  return id;
}

/*
 * This function adds a given coserver hash id to the response.
 */
static void
put_id (unsigned id, char *response)
{
  char buffer[COSERVER_BUFSIZE];

  snprintf (buffer, COSERVER_BUFSIZE, "%u:%s\n", id, response);
  strcpy (response, buffer);
}

/*************************************************************************/
/*            This is part of the coserver process / thread.             */
/*************************************************************************/

/*
 * Win32:
 * ‘big_loop’ is the actual thread routine being an infinite loop.
 * It MUST be resumed via ‘ResumeThread’ by the server.
 * When running it first checks if there is any request lingering
 * in the client structure "sock", reads it out, processes it
 * (can be blocking) and finally sends back a respond to the
 * server.
 *
 * Unices:
 * ‘big_loop’ is a infinite loop in a separate process.  It reads
 * blocking from a receive pipe, processes the request and puts the
 * result to a sending pipe to the server.
 *
 * The coserver loop heavily differs in Win32 and Unices...
 */

/* Debug info Macro.  */
#if ENABLE_DEBUG
# define COSERVER_REQUEST_INFO() \
  svz_log (SVZ_LOG_DEBUG, "%s: coserver request occurred\n",   \
           coservertypes[coserver->type].name);
#else
# define COSERVER_REQUEST_INFO()
#endif

/* Post-Processing Macro.  */
#if ENABLE_DEBUG
# define COSERVER_RESULT() \
  svz_log (SVZ_LOG_DEBUG, "%s: coserver request processed\n", \
           coservertypes[coserver->type].name);
#else
# define COSERVER_RESULT()
#endif

/* Pre-Processing Macro.  */
#define COSERVER_REQUEST()                                   \
  COSERVER_REQUEST_INFO ();                                  \
  /* Process the request here.  Might be blocking indeed!  */ \
  if ((id = get_id (request)) != 0)                          \
    {                                                        \
      if ((result = coserver->callback (request)) == NULL)   \
        {                                                    \
          result = request;                                  \
          *result = '\0';                                    \
        }                                                    \
      put_id (id, result);                                   \
    }                                                        \


#ifdef __MINGW32__
static void
big_loop (svz_coserver_t *coserver, svz_socket_t *sock)
{
  char *p;
  int len;
  char request[COSERVER_BUFSIZE];
  char *result = NULL;
  unsigned id;

  /* wait until the thread handle has been passed */
  while (svz_invalid_handle_p (coserver->thread));

  /* infinite loop */
  for (;;)
    {
      /* check if there is anything in the receive buffer */
      while (sock->send_buffer_fill > 0)
        {
          p = sock->send_buffer;
          while (*p != COSERVER_PACKET_BOUNDARY &&
                 p < sock->send_buffer + sock->send_buffer_fill)
            p++;
          len = p - sock->send_buffer + 1;

          /* Copy the coserver request to static buffer.  */
          assert (len <= COSERVER_BUFSIZE);
          memcpy (request, sock->send_buffer, len);

          /* Enter a synchronized section (exclusive access to all data).  */
          EnterCriticalSection (&coserver->sync);
          if (sock->send_buffer_fill > len)
            {
              memmove (sock->send_buffer, p + 1,
                       sock->send_buffer_fill - len);
            }
          sock->send_buffer_fill -= len;
          LeaveCriticalSection (&coserver->sync);

          COSERVER_REQUEST ();

          if (id && result)
            {
              EnterCriticalSection (&coserver->sync);
              memcpy (sock->recv_buffer + sock->recv_buffer_fill,
                      result, strlen (result));
              sock->recv_buffer_fill += strlen (result);
              LeaveCriticalSection (&coserver->sync);
              COSERVER_RESULT ();
            }
        }

      /* suspend myself and wait for being resumed ...  */
      if (SuspendThread (coserver->thread) == 0xFFFFFFFF)
        {
          svz_log_sys_error ("SuspendThread");
        }
    }
}

#else /* not __MINGW32__ */

static void
big_loop (svz_coserver_t *coserver, int in_pipe, int out_pipe)
{
  FILE *in, *out;
  char request[COSERVER_BUFSIZE];
  char *result = NULL;
  unsigned id;

  if ((in = fdopen (in_pipe, "r")) == NULL)
    {
      svz_log_sys_error ("coserver: fdopen (%d)", in_pipe);
      return;
    }
  if ((out = fdopen (out_pipe, "w")) == NULL)
    {
      svz_log_sys_error ("coserver: fdopen (%d)", out_pipe);
      return;
    }

  while (NULL != fgets (request, COSERVER_BUFSIZE, in))
    {

      COSERVER_REQUEST ();

      if (id && result)
        {
          fprintf (out, "%s", result);
          fflush (out);
          COSERVER_RESULT ();
        }
    }

  /* error in reading pipe */
  if (fclose (in))
    svz_log_sys_error ("fclose");
  if (fclose (out))
    svz_log_sys_error ("fclose");
}

#endif /* not __MINGW32__ */

/*************************************************************************/
/*                   This is part of the server process.                 */
/*************************************************************************/

#ifdef __MINGW32__

/*
 * This routine is the actual threads callback, but calls the coservers
 * callback indeed.  It is a wrapper routine for Win32, because you can pass
 * only a single argument to a thread routine.
 */
static DWORD WINAPI
coserver_thread (LPVOID thread)
{
  svz_coserver_t *coserver;

  coserver = (svz_coserver_t *) thread;
  big_loop (coserver, coserver->sock);
  ExitThread (0);

  return 0;
}

/*
 * Reactivate all specific coservers with type @var{type}.  In Win32
 * you have to call this if you want the coserver start working.
 */
static void
coserver_activate (int type)
{
  size_t n;
  int count = 0, res;
  svz_coserver_t *coserver;

  /* go through all internal coserver threads */
  svz_array_foreach (coservers, coserver, n)
    {
      /* is this structure of the requested type?  */
      if (coserver->type == type)
        {
          /* activated the thread */
          while ((res = ResumeThread (coserver->thread)) > 0);
          if (res == 0)
            count++;
        }
    }

#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "%d internal %s coserver activated\n",
           count, coservertypes[type].name);
#endif /* ENABLE_DEBUG */
}

#endif /* __MINGW32__ */

/*
 * Return the number of currently running coservers with the type @var{type}.
 */
static int
count_type (int type)
{
  size_t n;
  int count = 0;
  svz_coserver_t *coserver;

  svz_array_foreach (coservers, coserver, n)
    if (coserver->type == type)
      count++;
  return count;
}

/*
 * Delete the n'th internal coserver from coserver array.
 */
static void
delete_nth (size_t n)
{
  svz_coserver_t *coserver;

  if ((coserver = svz_array_get (coservers, n)) != NULL)
    {
      svz_free (coserver);
      svz_array_del (coservers, n);
    }
  if (svz_array_size (coservers) == 0)
    {
      svz_array_destroy (coservers);
      coservers = NULL;
    }
}

#ifndef __MINGW32__
/*
 * Disconnects a internal coserver.  This is the callback routine for the
 * socket structure entry `disconnected_socket'.
 */
static int
disconnect (svz_socket_t *sock)
{
  size_t n;
  svz_coserver_t *coserver;

  svz_array_foreach (coservers, coserver, n)
    {
      if (coserver->sock == sock)
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG,
                   "%s: killing coserver pid %d\n",
                   coservertypes[coserver->type].name, coserver->pid);
#endif /* ENABLE_DEBUG */
          if (kill (coserver->pid, SIGKILL) == -1)
            svz_log_sys_error ("kill");
#if HAVE_WAITPID
          /* cleanup coserver child process */
          else if (waitpid (coserver->pid, NULL, WNOHANG) == -1)
            svz_log_sys_error ("waitpid");
#endif /* HAVE_WAITPID */
          /* re-arrange the internal coserver array */
          delete_nth (n);
          break;
        }
    }
  return 0;
}
#endif /* not __MINGW32__ */

/*
 * This routine has to be called for coservers requests.  It is the default
 * @code{check_request} routine for coservers detecting full responses as
 * lines (trailing '\n').
 */
static int
check_request (svz_socket_t *sock)
{
  char *packet = sock->recv_buffer;
  char *p = packet;
  int request_len;
  int len = 0;
  svz_coserver_t *coserver = svz_hash_get (friendly, svz_itoa (sock->id));

  assert (coserver);
  do
    {
      /* find a line (trailing '\n') */
      while (*p != COSERVER_PACKET_BOUNDARY &&
             p < sock->recv_buffer + sock->recv_buffer_fill)
        p++;

      if (*p == COSERVER_PACKET_BOUNDARY &&
          p < sock->recv_buffer + sock->recv_buffer_fill)
        {
          coserver->busy--;
          p++;
          request_len = p - packet;
          len += request_len;
          if (sock->handle_request)
            sock->handle_request (sock, packet, request_len);
          packet = p;
        }
    }
  while (p < sock->recv_buffer + sock->recv_buffer_fill);

#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "%s: %d byte response\n",
           coservertypes[coserver->type].name, len);
#endif

  /* remove data from receive buffer if necessary */
  if (len > 0 && sock->recv_buffer_fill > len)
    {
      memmove (sock->recv_buffer, packet, sock->recv_buffer_fill - len);
    }
  sock->recv_buffer_fill -= len;

  return 0;
}

/*
 * The standard coserver @code{handle_request} routine is called whenever
 * the standard @code{check_request} detected a full packet by any coserver.
 */
static int
handle_request (UNUSED svz_socket_t *sock, char *request, int len)
{
  int ret;
  unsigned id;
  char *p, *end, *data;
  svz_coserver_callback_t *cb;

  /* Search for coserver hash id.  */
  id = 0;
  p = request;
  end = p + len;
  while (*p != COSERVER_ID_BOUNDARY && p < end)
    {
      if (*p < '0' || *p > '9')
        {
          svz_log (SVZ_LOG_WARNING,
                   "coserver: invalid character in id (0x%02X)\n", *p);
          return -1;
        }
      id *= 10;
      id += *p - '0';
      p++;
    }
  if (p == end)
    {
      svz_log (SVZ_LOG_WARNING, "coserver: invalid coserver response (no id)\n");
      return -1;
    }
  data = ++p;

  /* Search for packet end.  */
  while (*p != COSERVER_PACKET_BOUNDARY && p < end)
    p++;
  if (p == end)
    {
      svz_log (SVZ_LOG_WARNING,
               "coserver: invalid coserver response (no data)\n");
      return -1;
    }
  *p = '\0';

  /* Have a look at the coserver callback hash.  */
  if (NULL == (cb = svz_hash_get (callbacks, svz_itoa (id))))
    {
      svz_log (SVZ_LOG_ERROR, "coserver: invalid callback for id %u\n", id);
      return -1;
    }

  /*
   * Run the callback inclusive its arg.  Second arg is either NULL for
   * error detection or the actual result string.  Afterwards free the
   * callback structure and delete it from the coserver callback hash.
   */
  ret = cb->handle_result (*data ? data : NULL, cb->closure);
  svz_hash_delete (callbacks, svz_itoa (id));
  svz_free (cb);

  return ret;
}

#ifndef __MINGW32__
/*
 * This function closes the pipes (incoming and outgoing) of all coservers
 * inherited to a newly instantiated coserver.  These pipe descriptors are
 * part of server process and are inherited when we call @code{fork} in
 * order to create another coserver sub process.  Since this coserver process
 * should not access these pipes we are closing them.
 */
static void
close_pipes (svz_coserver_t *self)
{
  size_t n;
  svz_coserver_t *coserver;

  /* go through all coservers except itself */
  svz_array_foreach (coservers, coserver, n)
    {
      if (coserver != self)
        {
          close (coserver->sock->pipe_desc[SVZ_READ]);
          close (coserver->sock->pipe_desc[SVZ_WRITE]);
        }
    }
}

/*
 * Iterate each socket object and close its file/socket/pipe
 * descriptors.  Also frees the (cloned) queues.
 * Note: Duplicate memory for everything else, including server private data.
 *       We cannot take care of all that because the servers do not know
 *       that they may get cloned.  We therefore waste memory in the coservers.
 */
static void
close_all (svz_socket_t *self)
{
  svz_socket_t *sock, *next;

  for (sock = svz_sock_root; sock != NULL; sock = next)
    {
      if (sock->flags & SVZ_SOFLG_SOCK)
        if (sock->sock_desc >= 2)
          close (sock->sock_desc);
      if (sock->flags & SVZ_SOFLG_FILE)
        if (sock->file_desc >= 2)
          close (sock->file_desc);
      if (sock->flags & SVZ_SOFLG_PIPE)
        {
          if (sock->pipe_desc[SVZ_READ] >= 2)
            close (sock->pipe_desc[SVZ_READ]);
          if (sock->pipe_desc[SVZ_WRITE] >= 2)
            close (sock->pipe_desc[SVZ_WRITE]);
        }
      next = sock->next;
      if (sock != self)
        {
          svz_sock_resize_buffers (sock, 0, 0);
          svz_free (sock);
        }
    }
  svz_file_closeall ();
}

/*
 * Setup signaling for a coserver process.  This is necessary since
 * the original signal handlers get confused about signals raised by its
 * children.
 */
static void
setup_signals (void)
{
#ifdef SIGTERM
  signal (SIGTERM, SIG_IGN);
#endif
#ifdef SIGINT
  signal (SIGINT, SIG_IGN);
#endif
#ifdef SIGHUP
  signal (SIGHUP, SIG_IGN);
#endif
#ifdef SIGPIPE
  signal (SIGPIPE, SIG_IGN);
#endif
#ifdef SIGQUIT
  signal (SIGQUIT, SIG_IGN);
#endif
}

#endif /* not __MINGW32__ */

/**
 * Destroy specific coservers with the type @var{type}.
 * All instances of this coserver type will be stopped.
 */
void
svz_coserver_destroy (int type)
{
  size_t n;
  int count = 0;
  svz_coserver_t *coserver;

  svz_array_foreach (coservers, coserver, n)
    {
      if (coserver->type == type)
        {
#ifdef __MINGW32__
          /* stop the thread and close its handle */
          if (!TerminateThread (coserver->thread, 0))
            svz_log_sys_error ("TerminateThread");
          if (!CloseHandle (coserver->thread))
            svz_log_sys_error ("CloseHandle");
          DeleteCriticalSection (&coserver->sync);

          /* free all data reserved by the coserver */
          svz_sock_free (coserver->sock);
#else /* not __MINGW32__ */
          if (kill (coserver->pid, SIGKILL) == -1)
            svz_log_sys_error ("kill");
#if HAVE_WAITPID
          /* cleanup coserver child process */
          else if (waitpid (coserver->pid, NULL, WNOHANG) == -1)
            svz_log_sys_error ("waitpid");
#endif /* HAVE_WAITPID */
#endif /* not __MINGW32__ */
          delete_nth (n);
          n--;
          count++;
        }
    }

#ifdef ENABLE_DEBUG
  if (count > 0)
    {
      svz_log (SVZ_LOG_DEBUG, "%d internal %s coserver destroyed\n",
               count, coservertypes[type].name);
    }
#endif /* ENABLE_DEBUG */
}

/**
 * Return the type name of @var{coserver}.
 */
const char *
svz_coserver_type_name (const svz_coserver_t *coserver)
{
  return coservertypes[coserver->type].name;
}

/*
 * Start a specific internal coserver.  This works for Win32 and
 * Unices.  Whereas in Unix a process is @code{fork}ed and in Win32
 * a thread gets started.
 */
static svz_socket_t *
start (int type)
{
  svz_socket_t *sock;
  svz_coserver_t *coserver;

#ifndef __MINGW32__
  int s2c[2];
  int c2s[2];
  int pid;
#else /* not __MINGW32__ */
  HANDLE thread;
  DWORD tid;
#endif /* not __MINGW32__ */

  svz_log (SVZ_LOG_NOTICE, "starting internal %s coserver\n",
           coservertypes[type].name);

  coserver = svz_malloc (sizeof (svz_coserver_t));
  coserver->type = type;
  coserver->busy = 0;
  coserver->sock = NULL;

  if (coservers == NULL)
    coservers = svz_array_create (SVZ_MAX_COSERVER_TYPES, NULL);
  svz_array_add (coservers, coserver);

  /* fill in the actual coserver callback */
  coserver->callback = coservertypes[type].callback;

#ifdef __MINGW32__
  if ((sock = svz_sock_alloc ()) == NULL)
    return NULL;

  InitializeCriticalSection (&coserver->sync);
  sock->write_socket = NULL;
  sock->read_socket = NULL;
  coserver->sock = sock;
  svz_invalidate_handle (&coserver->thread);

  if (svz_invalid_handle_p
      (thread = CreateThread(
      (LPSECURITY_ATTRIBUTES) NULL, /* ignore security attributes */
      (DWORD) 0,                    /* default stack size */
      (LPTHREAD_START_ROUTINE) coserver_thread, /* thread routine */
      (LPVOID) coserver,            /* thread argument */
      (DWORD) CREATE_SUSPENDED,     /* creation flags */
      (LPDWORD) &tid)))             /* thread id */
    {
      svz_log_sys_error ("CreateThread");
      DeleteCriticalSection (&coserver->sync);
      svz_sock_free (sock);
      return NULL;
    }

  /* fill in thread access variables */
  coserver->tid = tid;
  coserver->thread = thread;

  /* set thread priority */
  if (!SetThreadPriority (thread, COSERVER_THREAD_PRIORITY))
    svz_log_sys_error ("SetThreadPriority");

#ifdef ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "coserver thread id is 0x%08X\n", tid);
#endif

#else /* not __MINGW32__ */

  /* create pipes for process communication */
  if (pipe (s2c) < 0)
    {
      svz_log_sys_error ("pipe server-coserver");
      delete_nth (svz_array_size (coservers) - 1);
      return NULL;
    }
  if (pipe (c2s) < 0)
    {
      close (s2c[SVZ_READ]);
      close (s2c[SVZ_WRITE]);
      svz_log_sys_error ("pipe coserver-server");
      delete_nth (svz_array_size (coservers) - 1);
      return NULL;
    }

  /* ‘fork’ us here */
  if ((pid = fork ()) == 0)
    {
      int in = s2c[SVZ_READ], out = c2s[SVZ_WRITE];

      /* close the servers pipe descriptors */
      if (close (s2c[SVZ_WRITE]) < 0)
        svz_log_sys_error ("close");
      if (close (c2s[SVZ_READ]) < 0)
        svz_log_sys_error ("close");

#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "coserver pipes: %d-%d\n", in, out);
#endif

      /* check if the pipes are 0, 1 or 2 already */
      if (in > 2 && out > 2)
        {
          /* reassign the pipes to stdout and stdin */
          if (dup2 (in, 0) != 0)
            svz_log_sys_error ("dup2");
          if (dup2 (out, 1) != 1)
            svz_log_sys_error ("dup2");
          /* close the old pipe descriptors */
          close (in);
          close (out);
          close (2);
          in = 0;
          out = 1;
        }
      else
        {
          if (in != 2 && out != 2)
            close (2);
          if (in != 1 && out != 1)
            close (1);
          if (in != 0 && out != 0)
            close (0);
        }

      /* close all other coserver pipes except its own */
      close_pipes (coserver);
      close_all (coserver->sock);
      setup_signals ();

      /* start the internal coserver */
      big_loop (coserver, in, out);
      exit (EXIT_SUCCESS);
    }
  else if (pid == -1)
    {
      svz_log_sys_error ("fork");
      close (s2c[SVZ_READ]);
      close (s2c[SVZ_WRITE]);
      close (c2s[SVZ_READ]);
      close (c2s[SVZ_WRITE]);
      delete_nth (svz_array_size (coservers) - 1);
      return NULL;
    }

  /* the old server process continues here */

#ifdef ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "coserver process id is %d\n", pid);
#endif

  /* close the coservers pipe descriptors */
  if (close (s2c[SVZ_READ]) < 0)
    svz_log_sys_error ("close");
  if (close (c2s[SVZ_WRITE]) < 0)
    svz_log_sys_error ("close");

  if ((sock = svz_pipe_create (c2s[SVZ_READ], s2c[SVZ_WRITE])) == NULL)
    {
      if (close (c2s[SVZ_READ]) < 0)
        svz_log_sys_error ("close");
      if (close (s2c[SVZ_WRITE]) < 0)
        svz_log_sys_error ("close");
      delete_nth (svz_array_size (coservers) - 1);
      return NULL;
    }

  coserver->pid = pid;
  coserver->sock = sock;
  sock->disconnected_socket = disconnect;
  sock->write_socket = svz_pipe_write_socket;
  sock->read_socket = svz_pipe_read_socket;
  svz_sock_enqueue (sock);

#endif /* __MINGW32__ and Unices */

  coservertypes[coserver->type].last_start = (long) time (NULL);
  svz_hash_put (friendly, svz_itoa (sock->id), coserver);
  sock->check_request = check_request;
  sock->handle_request = handle_request;
  sock->flags |= (SVZ_SOFLG_NOFLOOD | SVZ_SOFLG_COSERVER);
  return sock;
}

/**
 * Under woe32 check if there was any response from an active coserver.
 * Moreover keep the coserver threads/processes alive.  If one of the
 * coservers dies due to buffer overrun or might be overloaded,
 * start a new one.
 *
 * Call this function whenever there is time, e.g., within the timeout of the
 * @code{select} system call.
 */
void
svz_coserver_check (void)
{
  svz_coserver_t *coserver;
  svz_coservertype_t *ctype;
  svz_socket_t *sock;
  size_t n;

#ifdef __MINGW32__
  /* go through all coservers */
  svz_array_foreach (coservers, coserver, n)
    {
      sock = coserver->sock;
      while (sock->recv_buffer_fill > 0)
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "%s: coserver response detected\n",
                   coservertypes[coserver->type].name);
#endif
          /* find a full response within the receive buffer */
          if (sock->check_request)
            {
              EnterCriticalSection (&coserver->sync);
              sock->check_request (sock);
              LeaveCriticalSection (&coserver->sync);
            }
        }
    }
#endif /* __MINGW32__ */

  /* check the number of coserver instances of each coserver type */
  for (n = 0; n < SVZ_MAX_COSERVER_TYPES; n++)
    {
      ctype = &coservertypes[n];
      if (count_type (ctype->type) < ctype->instances &&
          ((long) time (NULL)) - ctype->last_start >= 3)
        start (ctype->type);
    }

  /* restart coserver instances if buffer overrun is in sight (send buffer
     fill >= 75 percent) */
  svz_array_foreach (coservers, coserver, n)
    {
      ctype = &coservertypes[coserver->type];
      sock = coserver->sock;
      if (sock->send_buffer_fill * 100 / sock->send_buffer_size >= 75 &&
          ((long) time (NULL)) - ctype->last_start >= 3 &&
          count_type (ctype->type) <= ctype->instances)
        start (coserver->type);
    }
}

/**
 * Create and return a single coserver with the given type @var{type}.
 */
svz_coserver_t *
svz_coserver_create (int type)
{
  size_t n;
  svz_coserver_t *coserver = NULL;

  if (coservertypes[type].init)
    coservertypes[type].init ();
  start (type);
  svz_array_foreach (coservers, coserver, n)
    {
      if (type == coserver->type)
        break;
    }
  return coserver;
}

static void
forget_sock (const svz_socket_t *sock)
{
  svz_hash_delete (friendly, svz_itoa (sock->id));
}

/*
 * Global coserver initialization.  Here you should start all the internal
 * coservers you want to use later.
 */
static int
init (void)
{
  int i, n;
  svz_coservertype_t *coserver;

  friendly = svz_hash_create (1, NULL);
  svz_sock_prefree (1, forget_sock);

  callbacks = svz_hash_create (4, svz_free);
  callback_id = 1;

  for (n = 0; n < SVZ_MAX_COSERVER_TYPES; n++)
    {
      coserver = &coservertypes[n];
      if (coserver->init)
        coserver->init ();
      for (i = 0; i < coserver->instances; i++)
        start (coserver->type);
    }

  return 0;
}

/*
 * Global coserver finalization.
 */
static int
finalize (void)
{
  int n;
  svz_coservertype_t *coserver;

  for (n = 0; n < SVZ_MAX_COSERVER_TYPES; n++)
    {
      coserver = &coservertypes[n];
      svz_coserver_destroy (coserver->type);
    }

#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "coserver: %d callback(s) left\n",
           svz_hash_size (callbacks));
#endif

  /* Destroy all callbacks left so far.  */
  svz_hash_destroy (callbacks);

  svz_sock_prefree (0, forget_sock);
  svz_hash_destroy (friendly);
  friendly = NULL;

  return 0;
}

/**
 * If @var{direction} is non-zero, init coserver internals.
 * Otherwise, finalize them.  Return 0 if successful.
 *
 * If @var{direction} is positive, init also starts one instance each
 * of the builtin servers.  If negative, it doesn't.
 */
int
svz_updn_all_coservers (int direction)
{
  if (0 > direction)
    for (int i = 0; i < SVZ_MAX_COSERVER_TYPES; i++)
      coservertypes[i].instances = 0;

  return (direction
          ? init
          : finalize)
    ();
}
