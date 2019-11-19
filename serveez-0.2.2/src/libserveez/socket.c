/*
 * socket.c - socket management implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include "timidity.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <signal.h>
#include <time.h>

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
# include <netdb.h>
#endif

#include "networking-headers.h"
#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/core.h"
#include "libserveez/pipe-socket.h"
#include "libserveez/tcp-socket.h"
#include "libserveez/server-core.h"
#include "libserveez/server.h"
#include "libserveez/binding.h"

/*
 * The number of currently connected sockets.
 */
int svz_sock_connections = 0;

/**
 * Return the number of currently connected sockets.
 */
int
svz_sock_nconnections (void)
{
  return svz_sock_connections;
}

/*
 * User-supplied functions called immediately prior to
 * a @code{svz_socket_t} being freed.
 */
static svz_array_t *prefree;

/**
 * Register (if @var{addsub} is non-zero), or unregister (otherwise)
 * the function @var{fn} to be called immediately
 * prior to a @code{svz_socket_t} being freed.
 * @var{fn} is called with one arg @code{sock},
 * and should not return anything.  In other words:
 *
 * @vindex svz_sock_prefree_fn
 * @example
 * typedef void (svz_sock_prefree_fn) (const svz_socket_t *);
 * @end example
 *
 * Note the @code{const}!
 */
void
svz_sock_prefree (int addsub, svz_sock_prefree_fn fn)
{
  if (prefree == NULL)
    prefree = svz_array_create (1, NULL);

  if (addsub)
    /* Add.  */
    svz_array_add (prefree, fn);
  else
    /* Subtract.  */
    {
      size_t i;

      for (i = 0; i < svz_array_size (prefree); i++)
        if (fn == svz_array_get (prefree, i))
          {
            svz_array_del (prefree, i);
            i--;
          }
    }
}

/*
 * This routine can be called if flood protection is wished for
 * socket readers.  Return non-zero if the socket should be kicked
 * because of flood.
 *
 * @strong{Note}: This always returns 0 if flood protection is not enabled.
 */
int
svz_sock_flood_protect (svz_socket_t *sock, int num_read)
{
#ifdef ENABLE_FLOOD_PROTECTION
  if (!(sock->flags & SVZ_SOFLG_NOFLOOD))
    {
      /*
       * Since the default flood limit is 100 a reader can produce
       * 5000 bytes per second before it gets kicked.
       */
      sock->flood_points += 1 + (num_read / 50);

      if (sock->flood_points > sock->flood_limit)
        {
          if (sock->kicked_socket)
            sock->kicked_socket (sock, 0);
          return -1;
        }
    }
#endif  /* ENABLE_FLOOD_PROTECTION */
  return 0;
}

/*
 * The default function which gets called when a client shuts down
 * its socket.  @var{sock} is the socket which was closed.
 */
static int
maybe_log_disconnect (svz_socket_t *sock)
{
#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "socket id %d disconnected\n", sock->id);
#endif

  return 0;
}

/*
 * This routine gets called whenever data is read from a client socket
 * accepted by any connection oriented protocol layer (TCP or PIPE).  We
 * try to detect the data streams protocol here.
 */
int
svz_sock_detect_proto (svz_socket_t *sock)
{
  size_t n;
  svz_server_t *server;
  svz_binding_t *binding;
  svz_portcfg_t *port;
  svz_array_t *bindings;

  /* return if there are no servers bound to this socket */
  if (svz_sock_bindings (sock) == NULL)
    return -1;

  /* get port configuration of parent */
  port = svz_sock_portcfg (sock);

  /* go through each server stored in the data field of this socket */
  bindings = svz_binding_filter (sock);
  svz_array_foreach (bindings, binding, n)
    {
      server = binding->server;

      /* can occur if it is actually a packet oriented server */
      if (server->detect_proto == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "%s: no detect-proto routine\n",
                   server->type->prefix);
        }
      /* call protocol detection routine of the server */
      else if (server->detect_proto (server, sock))
        {
          svz_array_destroy (bindings);
          sock->idle_func = NULL;
          svz_sock_bindings_set (sock, NULL);
          sock->cfg = server->cfg;
          sock->port = binding->port;
          if (!server->connect_socket)
            return -1;
          if (server->connect_socket (server, sock))
            return -1;
          if (sock->check_request == svz_sock_detect_proto)
            {
              svz_log (SVZ_LOG_ERROR, "%s: check-request callback unchanged\n",
                       server->type->prefix);
              sock->check_request = NULL;
            }
          if (sock->check_request)
            return sock->check_request (sock);
          return 0;
        }
    }
  svz_array_destroy (bindings);

  /*
   * Discard this socket if there were not any valid protocol
   * detected and its receive buffer fill exceeds a maximum value.
   */
  if (sock->recv_buffer_fill > port->detection_fill)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "socket id %d detection failed\n", sock->id);
#endif
      return -1;
    }

  return 0;
}

/*
 * This @code{check_request} routine could be used by any protocol to
 * detect and finally handle packets depending on a specific packet
 * boundary.  The appropriate @code{handle_request} is called for each packet
 * explicitly with the packet length inclusive the packet boundary.
 */
static int
split_packet_and_dispatch (svz_socket_t *sock)
{
  int len = 0;
  char *p, *packet, *end;

  packet = p = sock->recv_buffer;
  end = p + sock->recv_buffer_fill - sock->boundary_size + 1;

  do
    {
      /* Find packet boundary in the receive buffer.  */
      while (p < end && memcmp (p, sock->boundary, sock->boundary_size))
        p++;

      /* Found?  */
      if (p < end && !memcmp (p, sock->boundary, sock->boundary_size))
        {
          p += sock->boundary_size;
          len += (p - packet);

          /* Call the handle request callback.  */
          if (sock->handle_request)
            {
              if (sock->handle_request (sock, packet, p - packet))
                return -1;
            }
          packet = p;
        }
    }
  while (p < end);

  /* Shuffle data in the receive buffer around.  */
  svz_sock_reduce_recv (sock, len);

  return 0;
}

/*
 * This is just the same routine as above, but optimized for one byte
 * packet delimiters.
 */
static int
split_packet_unibyte_and_dispatch (svz_socket_t *sock)
{
  int len = 0;
  char *p, *packet, *end;

  packet = p = sock->recv_buffer;
  end = p + sock->recv_buffer_fill;

  do
    {
      /* Find packet boundary in the receive buffer.  */
      while (p < end && *p != *sock->boundary)
        p++;

      /* Found?  */
      if (p < end && *p == *sock->boundary)
        {
          p++;
          len += (p - packet);

          /* Call the handle request callback.  */
          if (sock->handle_request)
            {
              if (sock->handle_request (sock, packet, p - packet))
                return -1;
            }
          packet = p;
        }
    }
  while (p < end);

  /* Shuffle data in the receive buffer around.  */
  svz_sock_reduce_recv (sock, len);

  return 0;
}

/*
 * The following routine checks for fixed size packets in the receive queue
 * of the socket structure @var{sock} and calls the @code{handle_request}
 * callback if so.  It is possible to change the fixed packet size in the
 * @code{handle_request} callback dynamically.
 */
static int
split_packet_fixed_and_dispatch (svz_socket_t *sock)
{
  int len = 0;
  char *p, *packet, *end;

  packet = p = sock->recv_buffer;
  end = p + sock->recv_buffer_fill;

  while (p + sock->boundary_size < end)
    {
      len += sock->boundary_size;
      p += sock->boundary_size;

      /* Call the handle request callback.  */
      if (sock->handle_request)
        {
          if (sock->handle_request (sock, packet, sock->boundary_size))
            return -1;
        }
      packet = p;
    }

  /* Shuffle data in the receive buffer around.  */
  svz_sock_reduce_recv (sock, len);

  return 0;
}

/**
 * Check for the kind of packet delimiter within @var{sock} and
 * and assign one of the default @code{check_request} routines
 * (one or more byte delimiters or a fixed size).
 *
 * Afterwards this function will never ever be called again because
 * the callback gets overwritten here.
 */
int
svz_sock_check_request (svz_socket_t *sock)
{
  if (sock->boundary_size <= 0)
    {
      svz_log (SVZ_LOG_ERROR, "invalid boundary size: %d\n", sock->boundary_size);
      return -1;
    }

  if (sock->boundary == NULL)
    sock->check_request = split_packet_fixed_and_dispatch;
  else if (sock->boundary_size > 1)
    sock->check_request = split_packet_and_dispatch;
  else
    sock->check_request = split_packet_unibyte_and_dispatch;

  return sock->check_request (sock);
}

/*
 * Allocate a structure of type @code{svz_socket_t} and initialize its data
 * fields.  Assign some of the default callbacks for TCP connections.
 */
svz_socket_t *
svz_sock_alloc (void)
{
  svz_socket_t *sock;
  char *in;
  char *out;

  sock = svz_calloc (sizeof (svz_socket_t));
  in = svz_malloc (RECV_BUF_SIZE);
  out = svz_malloc (SEND_BUF_SIZE);

  sock->proto = SVZ_SOFLG_INIT;
  sock->flags = SVZ_SOFLG_INIT | SVZ_SOFLG_INBUF | SVZ_SOFLG_OUTBUF;
  sock->userflags = SVZ_SOFLG_INIT;
  sock->file_desc = -1;
  sock->sock_desc = (svz_t_socket) -1;
  svz_invalidate_handle (&sock->pipe_desc[SVZ_READ]);
  svz_invalidate_handle (&sock->pipe_desc[SVZ_WRITE]);
  svz_invalidate_handle (&sock->pid);

  sock->read_socket = svz_tcp_read_socket;
  sock->read_socket_oob = svz_tcp_recv_oob;
  sock->write_socket = svz_tcp_write_socket;
  sock->check_request = svz_sock_detect_proto;
  sock->disconnected_socket = maybe_log_disconnect;

  sock->recv_buffer = in;
  sock->recv_buffer_size = RECV_BUF_SIZE;
  sock->send_buffer = out;
  sock->send_buffer_size = SEND_BUF_SIZE;
  sock->last_send = time (NULL);
  sock->last_recv = time (NULL);

#if ENABLE_FLOOD_PROTECTION
  sock->flood_limit = 100;
#endif /* ENABLE_FLOOD_PROTECTION */

  return sock;
}

/**
 * Resize the send and receive buffers for the socket @var{sock}.
 * @var{send_buf_size} is the new size for the send buffer,
 * @var{recv_buf_size} for the receive buffer.  Note that data may be lost
 * when the buffers shrink.  For a new buffer size of 0 the buffer is
 * freed and the pointer set to NULL.
 */
int
svz_sock_resize_buffers (svz_socket_t *sock,
                         int send_buf_size, int recv_buf_size)
{
  char *send, *recv;

  if (send_buf_size == 0)
    {
      svz_free (sock->send_buffer);
      send = NULL;
    }
  else if (sock->send_buffer_size != send_buf_size)
    send = svz_realloc (sock->send_buffer, send_buf_size);
  else
    send = sock->send_buffer;

  if (recv_buf_size == 0)
    {
      svz_free (sock->recv_buffer);
      recv = NULL;
    }
  else if (sock->recv_buffer_size != recv_buf_size)
    recv = svz_realloc (sock->recv_buffer, recv_buf_size);
  else
    recv = sock->recv_buffer;

  sock->send_buffer = send;
  sock->recv_buffer = recv;
  sock->send_buffer_size = send_buf_size;
  sock->recv_buffer_size = recv_buf_size;

  return 0;
}

/*
 * Free the socket structure @var{sock}.  Return a non-zero value on error.
 */
int
svz_sock_free (svz_socket_t *sock)
{
  size_t i = svz_array_size (prefree);
  svz_sock_prefree_fn *fn;

  while (i--)
    {
      fn = svz_array_get (prefree, i);
      fn (sock);
    }

  if (sock->remote_addr)
    svz_free (sock->remote_addr);
  if (sock->local_addr)
    svz_free (sock->local_addr);
  if (sock->recv_buffer)
    svz_free (sock->recv_buffer);
  if (sock->send_buffer)
    svz_free (sock->send_buffer);
  if (sock->recv_pipe)
    svz_free (sock->recv_pipe);
  if (sock->send_pipe)
    svz_free (sock->send_pipe);

#ifdef __MINGW32__
  if (sock->overlap[SVZ_READ])
    svz_free (sock->overlap[SVZ_READ]);
  if (sock->overlap[SVZ_WRITE])
    svz_free (sock->overlap[SVZ_WRITE]);
#endif /* __MINGW32__ */

  svz_free (sock);

  return 0;
}

/*
 * Get local and remote addresses and ports of socket @var{sock} and save
 * them into the socket structure.
 */
int
svz_sock_intern_connection_info (svz_socket_t *sock)
{
  struct sockaddr_in s;
  socklen_t size = sizeof (s);
  in_port_t port;
  in_addr_t addr;

  if (!getpeername (sock->sock_desc, (struct sockaddr *) &s, &size))
    {
      addr = s.sin_addr.s_addr;
      port = s.sin_port;
    }
  else
    {
      addr = INADDR_ANY;
      port = 0;
    }
  sock->remote_port = port;
  SVZ_SET_ADDR (sock->remote_addr, AF_INET, &addr);

  size = sizeof (s);
  if (!getsockname (sock->sock_desc, (struct sockaddr *) &s, &size))
    {
      addr = s.sin_addr.s_addr;
      port = s.sin_port;
    }
  else
    {
      addr = INADDR_ANY;
      port = 0;
    }
  sock->local_port = port;
  SVZ_SET_ADDR (sock->local_addr, AF_INET, &addr);

  return 0;
}

/*
 * Create a socket structure from the file descriptor @var{fd}.  Set the
 * socket descriptor to non-blocking I/O.  Return @code{NULL} on errors.
 */
svz_socket_t *
svz_sock_create (int fd)
{
  svz_socket_t *sock;

  if (svz_fd_nonblock (fd) != 0)
    return NULL;
  if (svz_fd_cloexec (fd) != 0)
    return NULL;

  if ((sock = svz_sock_alloc ()) != NULL)
    {
      svz_sock_unique_id (sock);
      sock->sock_desc = fd;
      sock->flags |= (SVZ_SOFLG_CONNECTED | SVZ_SOFLG_SOCK);
      svz_sock_intern_connection_info (sock);
    }
  return sock;
}

/*
 * Disconnect the socket @var{sock} from the network and calls the disconnect
 * function for the socket if set.  Return a non-zero value on errors.
 */
int
svz_sock_disconnect (svz_socket_t *sock)
{
  /* shutdown client connection */
  if (sock->flags & SVZ_SOFLG_CONNECTED)
    {
      if (!(sock->flags & SVZ_SOFLG_NOSHUTDOWN))
        {
          if (shutdown (sock->sock_desc, 2) < 0)
            svz_log_net_error ("shutdown");
        }
      svz_sock_connections--;
    }

  /* close the server/client socket */
  if (svz_closesocket (sock->sock_desc) < 0)
    svz_log_net_error ("close");

#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "socket %d disconnected\n", sock->sock_desc);
#endif

  sock->sock_desc = INVALID_SOCKET;
  return 0;
}

/**
 * Write @var{len} bytes from the memory location pointed to by @var{buf}
 * to the output buffer of the socket @var{sock}.  Also try to flush the
 * buffer to the socket of @var{sock} if possible.  Return a non-zero value
 * on error, which normally means a buffer overflow.
 */
int
svz_sock_write (svz_socket_t *sock, char *buf, int len)
{
  int ret;
  int space;
  int orig_len = len;

  if (sock->flags & SVZ_SOFLG_KILLED)
    return 0;

  while (len > 0)
    {
      /* Try to flush the queue of this socket.  */
      if (sock->write_socket && !sock->unavailable &&
          sock->flags & SVZ_SOFLG_CONNECTED && sock->send_buffer_fill)
        {
          /* TCP-specific hack: If ‘SVZ_SOFLG_FINAL_WRITE’ is set, but
             we are flushing from a previous write, temporarily inhibit
             it around the next call to ‘sock->write_socket’ to prevent
             it (specifically, ‘svz_tcp_write_socket’) from scheduling a
             shutdown prematurely (with consequent early return from
             this function, silently dropping the current write).  */
          int inhibitp = (sock->flags & SVZ_SOFLG_FINAL_WRITE)
            && svz_tcp_write_socket == sock->write_socket
            && orig_len == len;

          if (inhibitp)
            sock->flags &= ~SVZ_SOFLG_FINAL_WRITE;
          if ((ret = sock->write_socket (sock)) != 0)
            return ret;
          if (inhibitp)
            sock->flags |= SVZ_SOFLG_FINAL_WRITE;
        }

      if (sock->send_buffer_fill >= sock->send_buffer_size)
        {
          /* Queue is full, unlucky socket or pipe ...  */
          if (sock->flags & SVZ_SOFLG_SEND_PIPE)
            svz_log (SVZ_LOG_ERROR,
                     "send buffer overflow on pipe (%d-%d) (id %d)\n",
                     sock->pipe_desc[SVZ_READ], sock->pipe_desc[SVZ_WRITE],
                     sock->id);
          else
            svz_log (SVZ_LOG_ERROR,
                     "send buffer overflow on socket %d (id %d)\n",
                     sock->sock_desc, sock->id);

          if (sock->kicked_socket)
            sock->kicked_socket (sock, 1);

          return -1;
        }

      /* Now move as much of BUF into the send queue.  */
      if (sock->send_buffer_fill + len < sock->send_buffer_size)
        {
          memcpy (sock->send_buffer + sock->send_buffer_fill, buf, len);
          sock->send_buffer_fill += len;
          len = 0;
        }
      else
        {
          space = sock->send_buffer_size - sock->send_buffer_fill;
          memcpy (sock->send_buffer + sock->send_buffer_fill, buf, space);
          sock->send_buffer_fill += space;
          len -= space;
          buf += space;
        }
    }

  return 0;
}

/**
 * Print a formatted string on the socket @var{sock}.  @var{fmt} is the
 * @code{printf}-style format string, which describes how to format the
 * optional arguments.
 */
int
svz_sock_printf (svz_socket_t *sock, const char *fmt, ...)
{
  va_list args;
  static char buffer[VSNPRINTF_BUF_SIZE];
  unsigned len;

  if (sock->flags & SVZ_SOFLG_KILLED)
    return 0;

  va_start (args, fmt);
  len = vsnprintf (buffer, VSNPRINTF_BUF_SIZE, fmt, args);
  va_end (args);

  /* Just to be sure...  */
  if (len > sizeof (buffer))
    len = sizeof (buffer);

  return svz_sock_write (sock, buffer, len);
}

/*
 * If @code{svz_socket_unavailable_error_p} returns non-zero, set @var{sock}
 * member @code{unavailable} to time @var{t} (or current time if zero) plus
 * @var{relax} seconds and return 1.  Otherwise, do nothing and return 0.
 */
int
svz_wait_if_unavailable (svz_socket_t *sock, unsigned int relax)
{
  if (svz_socket_unavailable_error_p ())
    {
      sock->unavailable = relax + time (NULL);
      return 1;
    }
  return 0;
}

/**
 * Shorten the receive buffer of @var{sock} by @var{len} bytes.
 */
void
svz_sock_reduce_recv (svz_socket_t *sock, int len)
{
  /* FIXME: What about the ‘0 > len’ case?  */
  if (len && sock->recv_buffer_fill > len)
    memmove (sock->recv_buffer, sock->recv_buffer + len,
             sock->recv_buffer_fill - len);
  sock->recv_buffer_fill -= len;
}

/**
 * Reduce the send buffer of @var{sock} by @var{len} bytes.
 */
void
svz_sock_reduce_send (svz_socket_t *sock, int len)
{
  /* FIXME: What about the ‘0 > len’ case?  */
  if (len && sock->send_buffer_fill > len)
    memmove (sock->send_buffer, sock->send_buffer + len,
             sock->send_buffer_fill - len);
  sock->send_buffer_fill -= len;
}
