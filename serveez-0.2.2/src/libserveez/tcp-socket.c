/*
 * tcp-socket.c - TCP socket connection implementation
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <time.h>

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#if HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
# include <netdb.h>
#endif

#include "networking-headers.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/core.h"
#include "libserveez/server-core.h"
#include "libserveez/tcp-socket.h"

/*
 * Default function for writing to the socket @var{sock}.  Simply flushes
 * the output buffer to the network.  Write as much as possible into the
 * socket @var{sock}.  Writing is performed non-blocking, so only as much
 * as fits into the network buffer will be written on each call.
 */
int
svz_tcp_write_socket (svz_socket_t *sock)
{
  int num_written;
  int do_write;
  svz_t_socket desc;

  desc = sock->sock_desc;

  /*
   * Write as many bytes as possible, remember how many were actually
   * sent.  Limit the maximum sent bytes to ‘SVZ_SOCK_MAX_WRITE’.
   */
  do_write = sock->send_buffer_fill;
  if (do_write > SVZ_SOCK_MAX_WRITE)
    do_write = SVZ_SOCK_MAX_WRITE;
  num_written = send (desc, sock->send_buffer, do_write, 0);

  /* Some data has been written.  */
  if (num_written > 0)
    {
      sock->last_send = time (NULL);

      /*
       * Shuffle the data in the output buffer around, so that
       * new data can get stuffed into it.
       */
      svz_sock_reduce_send (sock, num_written);
    }
  /* Error occurred while sending.  */
  else if (num_written < 0)
    {
      svz_log_net_error ("tcp: send");
      if (svz_wait_if_unavailable (sock, 1))
        num_written = 0;
    }

  /* If final write flag is set, then schedule for shutdown.  */
  if (sock->flags & SVZ_SOFLG_FINAL_WRITE && sock->send_buffer_fill == 0)
    num_written = -1;

  /* Return a non-zero value if an error occurred.  */
  return (num_written < 0) ? -1 : 0;
}

/**
 * Read all data from @var{sock} and call the @code{check_request}
 * function for the socket, if set.  Return -1 if the socket has died,
 * zero otherwise.
 *
 * This is the default function for reading from @var{sock}.
 */
int
svz_tcp_read_socket (svz_socket_t *sock)
{
  int num_read;
  int ret;
  int do_read;
  svz_t_socket desc;

  desc = sock->sock_desc;

  /*
   * Calculate how many bytes fit into the receive buffer.
   */
  do_read = sock->recv_buffer_size - sock->recv_buffer_fill;

  /*
   * Check if enough space is left in the buffer, kick the socket
   * if not.  The main loop will kill the socket if we return a non-zero
   * value.
   */
  if (do_read <= 0)
    {
      svz_log (SVZ_LOG_ERROR, "receive buffer overflow on socket %d\n", desc);
      if (sock->kicked_socket)
        sock->kicked_socket (sock, 0);
      return -1;
    }

  /*
   * Try to read as much data as possible.
   */
  num_read = recv (desc,
                   sock->recv_buffer + sock->recv_buffer_fill, do_read, 0);

  /* Error occurred while reading.  */
  if (num_read < 0)
    {
      /*
       * This means that the socket was shut down.  Close the socket in this
       * case, which the main loop will do for us if we return a non-zero
       * value.
       */
      svz_log_net_error ("tcp: recv");
      if (svz_socket_unavailable_error_p ())
        num_read = 0;
      else
        return -1;
    }
  /* Some data has been read successfully.  */
  else if (num_read > 0)
    {
      sock->last_recv = time (NULL);

#if ENABLE_FLOOD_PROTECTION
      if (svz_sock_flood_protect (sock, num_read))
        {
          svz_log (SVZ_LOG_ERROR, "kicked socket %d (flood)\n", desc);
          return -1;
        }
#endif /* ENABLE_FLOOD_PROTECTION */

      sock->recv_buffer_fill += num_read;

      if (sock->check_request)
        {
          if ((ret = sock->check_request (sock)) != 0)
            return ret;
        }
    }
  /* The socket was ‘select’ed but there is no data.  */
  else
    {
      svz_log (SVZ_LOG_ERROR, "tcp: recv: no data on socket %d\n", desc);
      return -1;
    }

  return 0;
}

/*
 * This function is the default @code{read_socket_oob} callback for
 * TCP sockets.  It stores the received out-of-band data (a single byte
 * only) in @code{sock->oob} and runs the @code{check_request_oob} callback
 * if it is set properly.  Returns -1 on failure and zero otherwise.  The
 * function does not do anything if the underlying operating system does not
 * support urgent data and simply returns -1.
 */
int
svz_tcp_recv_oob (svz_socket_t *sock)
{
#ifdef MSG_OOB
  svz_t_socket desc = sock->sock_desc;
  int num_read, ret;

#if 0
#if HAVE_POLL && ENABLE_POLL && defined (__linux__)
#ifdef SIOCATMARK
  /* FIXME: fails for ‘poll’ on GNU/Linux ???  This is a hack !!!
            With this hack you are missing some OOB data bytes if sent too
            frequently.  It is *not* necessary for ‘select’.  Don't ask.
            The symptom is: `recv(..., MSG_OOB)' return `EINVAL'.  */
  ret = ioctl (desc, SIOCATMARK, &num_read);
  if (ret != -1 && num_read == 0)
    {
      svz_log (SVZ_LOG_FATAL, "cannot read OOB data byte\n");
      num_read = 0;
    }
  else
#endif /* SIOCATMARK */
#endif /* HAVE_POLL && ENABLE_POLL && linux */
#endif

  num_read = recv (desc, (void *) &sock->oob, 1, MSG_OOB);
  if (num_read < 0)
    {
      svz_log_net_error ("tcp: recv-oob");
      return -1;
    }
  else if (num_read > 0)
    {
      if (sock->check_request_oob)
        if ((ret = sock->check_request_oob (sock)) != 0)
          return ret;
    }
  return 0;
#else /* not MSG_OOB */
  return -1;
#endif /* not MSG_OOB */
}

/**
 * If the underlying operating system supports urgent data (out-of-band) in
 * TCP streams, try to send the byte in @code{sock->oob} through the socket
 * structure @var{sock} as out-of-band data.  Return zero on success and -1
 * otherwise (also if urgent data is not supported).
 */
int
svz_tcp_send_oob (svz_socket_t *sock)
{
#ifdef MSG_OOB
  svz_t_socket desc = sock->sock_desc;
  int num_written;

  num_written = send (desc, (void *) &sock->oob, 1, MSG_OOB);
  if (num_written < 0)
    {
      svz_log_net_error ("tcp: send-oob");
      return -1;
    }
  else if (num_written == 0)
    {
      svz_log (SVZ_LOG_ERROR, "tcp: send-oob: unable to send `0x%02x'\n",
               sock->oob);
    }
  return 0;
#else /* not MSG_OOB */
  return -1;
#endif /* not MSG_OOB */
}

/*
 * The default routine for connecting a socket @var{sock}.  When we get
 * @code{select}ed or @code{poll}ed via the @var{WRITE_SET} we simply
 * check for network errors,
 */
static int
tcp_default_connect (svz_socket_t *sock)
{
  int error;
  socklen_t optlen = sizeof (int);

  /* check if the socket has been finally connected */
  if (getsockopt (sock->sock_desc, SOL_SOCKET, SO_ERROR,
                  (void *) &error, &optlen) < 0)
    {
      svz_log_net_error ("getsockopt");
      return -1;
    }

  /* any errors?  */
  if (error)
    {
#ifdef __MINGW32__
      WSASetLastError (error);
      svz_errno = error;
#else
      errno = error;
#endif
      if (error != SOCK_INPROGRESS
          && ! svz_socket_unavailable_error_p ())
        {
          svz_log_net_error ("connect");
          return -1;
        }
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "connect: %s\n", svz_net_strerror ());
#endif
      return 0;
    }

  /* successfully connected */
  sock->flags |= SVZ_SOFLG_CONNECTED;
  sock->flags &= ~SVZ_SOFLG_CONNECTING;
  svz_sock_intern_connection_info (sock);
  svz_sock_connections++;

  return 0;
}

/**
 * Create a TCP connection to host @var{host} and set the socket descriptor
 * in structure @var{sock} to the resulting socket.  Return @code{NULL} on
 * errors.
 */
svz_socket_t *
svz_tcp_connect (svz_address_t *host, in_port_t port)
{
  svz_t_socket sockfd;
  svz_socket_t *sock;

  STILL_NO_V6_DAMMIT (host);

  /* Create a socket.  */
  if ((sockfd = svz_socket_create (SVZ_PROTO_TCP)) == (svz_t_socket) -1)
    return NULL;

  /* Try connecting.  */
  if (svz_socket_connect (sockfd, host, port) == -1)
    return NULL;

  /* Create socket structure and enqueue it.  */
  if ((sock = svz_sock_alloc ()) == NULL)
    {
      svz_closesocket (sockfd);
      return NULL;
    }

  svz_sock_unique_id (sock);
  sock->sock_desc = sockfd;
  sock->proto = SVZ_PROTO_TCP;
  sock->flags |= (SVZ_SOFLG_SOCK | SVZ_SOFLG_CONNECTING);
  sock->connected_socket = tcp_default_connect;
  sock->check_request = NULL;
  svz_sock_enqueue (sock);

  return sock;
}
