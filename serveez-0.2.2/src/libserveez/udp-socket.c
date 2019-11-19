/*
 * udp-socket.c - udp socket implementations
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>
#include <time.h>

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
#endif

#include "networking-headers.h"
#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/core.h"
#include "libserveez/server-core.h"
#include "libserveez/server.h"
#include "libserveez/portcfg.h"
#include "libserveez/binding.h"
#include "libserveez/udp-socket.h"

/*
 * This routine is the default reader for UDP sockets.  Whenever the socket
 * descriptor is @code{select}'ed for reading it is called by default and
 * reads as much data as possible (whole packets only) and saves the sender
 * into the @code{sock->remote_addr} field.  The packet load is written into
 * @code{sock->recv_buffer}.
 */
static int
udp_read_socket (svz_socket_t *sock)
{
#if ENABLE_DEBUG
  char buf[64];
#endif
  int do_read, num_read;
  socklen_t len;
  struct sockaddr_in sender;

  len = sizeof (struct sockaddr_in);

  /* Check if there is enough space to save the packet.  */
  do_read = sock->recv_buffer_size - sock->recv_buffer_fill;
  if (do_read <= 0)
    {
      svz_log (SVZ_LOG_ERROR, "receive buffer overflow on udp socket %d\n",
               sock->sock_desc);
      return -1;
    }

  /* Receive data.  */
  if (!(sock->flags & SVZ_SOFLG_CONNECTED))
    {
      num_read = recvfrom (sock->sock_desc,
                           sock->recv_buffer + sock->recv_buffer_fill,
                           do_read, 0, (struct sockaddr *) &sender, &len);
    }
  else
    {
      num_read = recv (sock->sock_desc,
                       sock->recv_buffer + sock->recv_buffer_fill,
                       do_read, 0);
    }

  /* Valid packet data arrived.  */
  if (num_read > 0)
    {
      sock->last_recv = time (NULL);
      sock->recv_buffer_fill += num_read;

      /* Save sender in socket structure.  */
      if (!(sock->flags & SVZ_SOFLG_FIXED))
        {
          sock->remote_port = sender.sin_port;
          SVZ_SET_ADDR (sock->remote_addr, AF_INET, &sender.sin_addr.s_addr);
        }

#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "udp: recv%s: %s (%d bytes)\n",
               sock->flags & SVZ_SOFLG_CONNECTED ? "" : "from",
               SVZ_PP_ADDR_PORT (buf, sock->remote_addr, sock->remote_port),
               num_read);
#endif /* ENABLE_DEBUG */

      /* Check access lists.  */
      if (svz_sock_check_access (sock, sock) < 0)
        return 0;

      /* Handle packet.  */
      if (sock->check_request)
        if (sock->check_request (sock))
          return -1;
    }
  /* Some error occurred.  */
  else
    {
      svz_log_net_error ("udp: recv%s", (sock->flags & SVZ_SOFLG_CONNECTED
                                         ? ""
                                         : "from"));
      if (! svz_socket_unavailable_error_p ())
        return -1;
    }
  return 0;
}

/*
 * This routine is the default reader for UDP server sockets.  It allocates
 * necessary buffers (that's why it's called lazy) and reverts to the default
 * @code{udp_read_socket}.
 */
int
svz_udp_lazy_read_socket (svz_socket_t *sock)
{
  svz_portcfg_t *port = sock->port;

  svz_sock_resize_buffers (sock, port->send_buffer_size,
                           port->recv_buffer_size);
  sock->read_socket = udp_read_socket;

  return sock->read_socket (sock);
}

/*
 * The @code{svz_udp_write_socket} callback should be called whenever
 * the UDP socket descriptor is ready for sending.  It sends a single packet
 * within the @code{sock->send_buffer} to the destination address specified
 * by @code{sock->remote_addr} and @code{sock->remote_port}.
 */
int
svz_udp_write_socket (svz_socket_t *sock)
{
  int num_written;
  unsigned do_write;
  char *p;
  socklen_t len;
  struct sockaddr_in receiver;

  /* return here if there is nothing to send */
  if (sock->send_buffer_fill <= 0)
    return 0;

  len = sizeof (struct sockaddr_in);
  receiver.sin_family = AF_INET;

  /* get destination address, port and data length from buffer */
  p = sock->send_buffer;
  memcpy (&do_write, p, sizeof (do_write));
  p += sizeof (do_write);
  memcpy (&receiver.sin_addr.s_addr, p, sizeof (in_addr_t));
  p += sizeof (in_addr_t);
  memcpy (&receiver.sin_port, p, sizeof (sock->remote_port));
  p += sizeof (sock->remote_port);

  /* if socket is `connect ()' ed use `send ()' instead of `sendto ()' */
  if (!(sock->flags & SVZ_SOFLG_CONNECTED))
    {
      num_written = sendto (sock->sock_desc, p,
                            do_write - (p - sock->send_buffer),
                            0, (struct sockaddr *) &receiver, len);
    }
  else
    {
      num_written = send (sock->sock_desc, p,
                          do_write - (p - sock->send_buffer), 0);
    }

  /* some error occurred while sending */
  if (num_written < 0)
    {
      svz_log_net_error ("udp: send%s", (sock->flags & SVZ_SOFLG_CONNECTED
                                         ? ""
                                         : "to"));
      if (svz_socket_unavailable_error_p ())
        num_written = 0;
    }
  /* packet data could be transmitted */
  else
    {
      sock->last_send = time (NULL);
      svz_sock_reduce_send (sock, (int) do_write);
    }

#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "udp: send%s: %s:%u (%u bytes)\n",
           sock->flags & SVZ_SOFLG_CONNECTED ? "" : "to",
           svz_inet_ntoa (receiver.sin_addr.s_addr),
           ntohs (receiver.sin_port), do_write - (p - sock->send_buffer));
#endif /* ENABLE_DEBUG */

  return num_written < 0 ? -1 : 0;
}

/*
 * This is the default @code{check_request} routine for UDP servers.
 * Whenever new data arrived at an UDP server socket we call this function to
 * process the packet data.  Any given @code{handle_request} callback MUST
 * return zero if it successfully processed the data and non-zero if it
 * could not.
 */
int
svz_udp_check_request (svz_socket_t *sock)
{
  size_t n;
  svz_server_t *server;
  svz_array_t *bindings;
  svz_binding_t *binding;

  if (svz_sock_bindings (sock) == NULL && sock->handle_request == NULL)
    return -1;

  /*
   * If there is a valid `handle_request ()' callback (dedicated udp
   * connection) call it.  This kind of behaviour is due to a socket creation
   * via `udp_connect ()' and setting up a static `handle_request ()'
   * callback.
   */
  if (sock->handle_request)
    {
      if (sock->handle_request (sock, sock->recv_buffer,
                                sock->recv_buffer_fill))
        return -1;
      sock->recv_buffer_fill = 0;
      return 0;
    }

  /* go through all udp servers on this server socket */
  bindings = svz_binding_filter (sock);
  svz_array_foreach (bindings, binding, n)
    {
      server = binding->server;
      sock->cfg = server->cfg;

      if (server->handle_request)
        {
          if (!server->handle_request (sock, sock->recv_buffer,
                                       sock->recv_buffer_fill))
            {
              sock->recv_buffer_fill = 0;
              break;
            }
        }
    }
  svz_array_destroy (bindings);

  /* check if any server processed this packet */
  if (sock->recv_buffer_fill)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "rejecting udp packet on socket %d\n",
               sock->sock_desc);
#endif
      sock->recv_buffer_fill = 0;
    }

  sock->cfg = NULL;
  return 0;
}

/**
 * Write @var{buf} into the send queue of the UDP socket @var{sock}.  If
 * @var{length} argument supersedes the maximum length for UDP messages it
 * is split into smaller packets.
 */
int
svz_udp_write (svz_socket_t *sock, char *buf, int length)
{
  char *buffer;
  unsigned len, size;
  int ret = 0;

  /* return if the socket has already been killed */
  if (sock->flags & SVZ_SOFLG_KILLED)
    return 0;

  /* allocate memory block */
  buffer = svz_malloc ((length > SVZ_UDP_MSG_SIZE ? SVZ_UDP_MSG_SIZE : length) +
                       sizeof (len) + sizeof (in_addr_t) +
                       sizeof (sock->remote_port));

  while (length)
    {
      /*
       * Put the data length, destination address and port in front
       * of each data packet.
       */
      len = sizeof (len);
      svz_address_to (&buffer[len], sock->remote_addr);
      len += sizeof (in_addr_t);
      memcpy (&buffer[len], &sock->remote_port, sizeof (sock->remote_port));
      len += sizeof (sock->remote_port);

      /* copy the given buffer */
      if ((size = length) > SVZ_UDP_MSG_SIZE)
        size = SVZ_UDP_MSG_SIZE;
      memcpy (&buffer[len], buf, size);
      len += size;
      memcpy (buffer, &len, sizeof (len));
      buf += size;
      length -= size;

      /* actually send the data or put it into the send buffer queue */
      if ((ret = svz_sock_write (sock, buffer, len)) == -1)
        {
          sock->flags |= SVZ_SOFLG_KILLED;
          break;
        }
    }

  /* release memory block */
  svz_free (buffer);
  return ret;
}

/**
 * Create a UDP connection to @var{host} at @var{port} and set the socket
 * descriptor in structure @var{sock} to the resulting socket.  Return a
 * @code{NULL} value on errors.
 *
 * This function can be used for port bouncing.  If you assign the
 * @code{handle_request} callback to something server specific and the
 * @var{cfg} field of the server's configuration to the returned socket
 * structure, this socket is able to handle a dedicated UDP connection to
 * some other UDP server.
 */
svz_socket_t *
svz_udp_connect (svz_address_t *host, in_port_t port)
{
  svz_t_socket sockfd;
  svz_socket_t *sock;

  STILL_NO_V6_DAMMIT (host);

  /* Create a client socket.  */
  if ((sockfd = svz_socket_create (SVZ_PROTO_UDP)) == (svz_t_socket) -1)
    return NULL;

  /* Try to connect to the server.  Does it make sense for ICMP?  */
  if (svz_socket_connect (sockfd, host, port) == -1)
    return NULL;

  /* Create socket structure and enqueue it.  */
  if ((sock = svz_sock_alloc ()) == NULL)
    {
      svz_closesocket (sockfd);
      return NULL;
    }

  svz_sock_resize_buffers (sock, SVZ_UDP_BUF_SIZE, SVZ_UDP_BUF_SIZE);
  svz_sock_unique_id (sock);
  sock->sock_desc = sockfd;
  sock->proto = SVZ_PROTO_UDP;
  sock->flags |= (SVZ_SOFLG_SOCK | SVZ_SOFLG_CONNECTED | SVZ_SOFLG_FIXED);
  svz_sock_enqueue (sock);
  svz_sock_intern_connection_info (sock);

  sock->read_socket = udp_read_socket;
  sock->write_socket = svz_udp_write_socket;
  sock->check_request = svz_udp_check_request;

  svz_sock_connections++;
  return sock;
}
