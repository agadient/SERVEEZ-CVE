/*
 * server-socket.c - server sockets for TCP, UDP, ICMP and pipes
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
#include <string.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <time.h>

#ifdef __MINGW32__
# if HAVE_WS2TCPIP_H
#  include <ws2tcpip.h>
# endif
#endif
#include "networking-headers.h"
#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
# include <netdb.h>
#endif

#include "libserveez/boot.h"
#include "libserveez/util.h"
#include "libserveez/alloc.h"
#include "libserveez/core.h"
#include "libserveez/socket.h"
#include "libserveez/pipe-socket.h"
#include "libserveez/udp-socket.h"
#include "libserveez/icmp-socket.h"
#include "libserveez/server-core.h"
#include "libserveez/server.h"
#include "libserveez/binding.h"
#include "libserveez/portcfg.h"
#include "libserveez/server-socket.h"
#include "misc-macros.h"

/*
 * Default idle function.  This routine simply checks for "dead"
 * (non-receiving) sockets (connection oriented protocols only) and rejects
 * them by return a non-zero value.
 */
static int
idle_protect (svz_socket_t *sock)
{
  svz_portcfg_t *port = svz_sock_portcfg (sock);

  if (time (NULL) - sock->last_recv > port->detection_wait)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "socket id %d detection failed\n", sock->id);
#endif
      return -1;
    }

  sock->idle_counter = 1;
  return 0;
}

/* This is 1 if ‘time_t’ cannot be cast into a ‘void *’.  If that's the
   case, ‘check_frequency’ stores pointers to ‘svz_malloc’d
   values in the array (arranging to ‘svz_free’ them when done).  */
#define TIME_T_TOO_FAT  (SIZEOF_VOID_P < sizeof (time_t))

/*
 * This routine checks the connection frequency of the socket structure
 * @var{child} for the port configuration of the listener socket structure
 * @var{parent}.  Returns zero if the connection frequency is valid,
 * otherwise non-zero.
 */
static int
check_frequency (svz_socket_t *parent, svz_socket_t *child)
{
  svz_portcfg_t *port = parent->port;
  char ip[64];
  time_t *t, *s = TIME_T_TOO_FAT ? svz_malloc (sizeof (time_t)) : NULL;
  time_t current;
  size_t n;
  int nr, ret = 0;
  svz_array_t *accepted = NULL;

  SVZ_PP_ADDR (ip, child->remote_addr);

  /* Check connect frequency.  */
  if (port->accepted)
    accepted = svz_hash_get (port->accepted, ip);
  else
    port->accepted = svz_hash_create (4, (svz_free_func_t) svz_array_destroy);
  current = time (s);

  if (accepted != NULL)
    {
      /* Delete older entries and count valid entries.  */
      nr = 0;
      svz_array_foreach (accepted, t, n)
        {
          if ((TIME_T_TOO_FAT ? *t : (time_t) SVZ_PTR2NUM (t)) < current - 4)
            {
              svz_array_del (accepted, n);
              n--;
            }
          else
            nr++;
        }
      /* Check the connection frequency.  */
      if ((nr /= 4) > port->connect_freq)
        {
          svz_log (SVZ_LOG_NOTICE, "connect frequency reached: %s: %d/%d\n",
                   ip, nr, port->connect_freq);
          ret = -1;
        }
    }
  /* Not yet connected.  */
  else
    {
      accepted = svz_array_create (1, TIME_T_TOO_FAT ? svz_free : NULL);
    }

  svz_array_add (accepted, TIME_T_TOO_FAT ? s : SVZ_NUM2PTR (current));
  svz_hash_put (port->accepted, ip, accepted);
  return ret;
}

/*
 * Something happened on the a server socket, most probably a client
 * connection which we will normally accept.  This is the default callback
 * for @code{read_socket} for listening tcp sockets.
 */
static int
tcp_accept (svz_socket_t *server_sock)
{
  svz_t_socket client_socket;   /* socket to accept clients on */
  struct sockaddr_in client;    /* address of connecting clients */
  socklen_t client_size;        /* size of the address above */
  svz_socket_t *sock;
  svz_portcfg_t *port = server_sock->port;
  int max_sockets;

  memset (&client, 0, sizeof (client));
  client_size = sizeof (client);

  client_socket = accept (server_sock->sock_desc, (struct sockaddr *) &client,
                          &client_size);

  if (client_socket == INVALID_SOCKET)
    {
      svz_log (SVZ_LOG_WARNING, "accept: %s\n", svz_net_strerror ());
      return 0;
    }

  max_sockets = SVZ_RUNPARM (MAX_SOCKETS);
  if ((svz_t_socket) svz_sock_connections >= max_sockets)
    {
      svz_log (SVZ_LOG_WARNING, "socket descriptor exceeds "
               "socket limit %d\n", max_sockets);
      if (svz_closesocket (client_socket) < 0)
        {
          svz_log_net_error ("close");
        }
      return 0;
    }

  svz_log (SVZ_LOG_NOTICE, "TCP:%u: accepting client on socket %d\n",
           ntohs (server_sock->local_port), client_socket);

  /*
   * Sanity check.  Just to be sure that we always handle
   * correctly connects/disconnects.
   */
  sock = svz_sock_root;
  while (sock && sock->sock_desc != client_socket)
    sock = sock->next;
  if (sock)
    {
      svz_log (SVZ_LOG_FATAL, "socket %d already in use\n", sock->sock_desc);
      if (svz_closesocket (client_socket) < 0)
        {
          svz_log_net_error ("close");
        }
      return -1;
    }

  /*
   * Now enqueue the accepted client socket and assign the
   * CHECK_REQUEST callback.
   */
  if ((sock = svz_sock_create (client_socket)) != NULL)
    {
      sock->flags |= SVZ_SOFLG_CONNECTED;
      svz_sock_bindings_set (sock, server_sock);
      sock->check_request = server_sock->check_request;
      sock->idle_func = idle_protect;
      sock->idle_counter = 1;

      svz_sock_resize_buffers (sock, port->send_buffer_size,
                               port->recv_buffer_size);
      svz_sock_enqueue (sock);
      svz_sock_setparent (sock, server_sock);
      sock->proto = server_sock->proto;
      svz_sock_connections++;

      /* Check access and connect frequency here.  */
      if (svz_sock_check_access (server_sock, sock) < 0 ||
          check_frequency (server_sock, sock) < 0)
        svz_sock_schedule_for_shutdown (sock);

      /*
       * We call the ‘check_request’ routine here once in order to
       * allow "greedy" protocols (always returning success
       * in the ‘detect_proto’ routine) to get their connection without
       * sending anything.
       */
      if (sock->check_request)
        if (sock->check_request (sock))
          svz_sock_schedule_for_shutdown (sock);
    }

  return 0;
}

/*
 * Check if client pipe is connected.  This is the default callback for
 * @code{idle_func} for listening pipe sockets.
 */
static int
pipe_accept (svz_socket_t *server_sock)
{
#ifdef __MINGW32__
  DWORD connect;
#endif

#if defined (HAVE_MKFIFO) || defined (HAVE_MKNOD) || defined (__MINGW32__)
  svz_t_handle recv_pipe, send_pipe;
  svz_socket_t *sock;
  svz_portcfg_t *port = server_sock->port;
  server_sock->idle_counter = 1;
#endif

#if HAVE_MKFIFO || HAVE_MKNOD
  /*
   * Try opening the server's send pipe.  This will fail
   * until the client has opened it for reading.
   */
  send_pipe = open (server_sock->send_pipe, O_NONBLOCK | O_WRONLY);
  if (send_pipe == -1)
    {
      if (errno != ENXIO)
        {
          svz_log_sys_error ("open");
          return -1;
        }
      return 0;
    }
  recv_pipe = server_sock->pipe_desc[SVZ_READ];

  /* Create a socket structure for the client pipe.  */
  if ((sock = svz_pipe_create (recv_pipe, send_pipe)) == NULL)
    {
      close (send_pipe);
      return 0;
    }

#elif defined (__MINGW32__) /* not HAVE_MKFIFO */

  recv_pipe = server_sock->pipe_desc[SVZ_READ];
  send_pipe = server_sock->pipe_desc[SVZ_WRITE];

  /* Try connecting to one of these pipes.  This will fail until a client
     has been connected.  */
  if (server_sock->flags & SVZ_SOFLG_CONNECTING)
    {
      if (!GetOverlappedResult (send_pipe, server_sock->overlap[SVZ_WRITE],
                                &connect, FALSE))
        {
          if (GetLastError () != ERROR_IO_INCOMPLETE)
            {
              svz_log_sys_error ("pipe: GetOverlappedResult");
              return -1;
            }
          return 0;
        }
      else
        {
          server_sock->flags &= ~SVZ_SOFLG_CONNECTING;
          svz_log (SVZ_LOG_NOTICE, "pipe: send pipe %s connected\n",
                   server_sock->send_pipe);
        }

      if (!GetOverlappedResult (recv_pipe, server_sock->overlap[SVZ_READ],
                                &connect, FALSE))
        {
          if (GetLastError () != ERROR_IO_INCOMPLETE)
            {
              svz_log_sys_error ("pipe: GetOverlappedResult");
              return -1;
            }
          return 0;
        }
      else
        {
          server_sock->flags &= ~SVZ_SOFLG_CONNECTING;
          svz_log (SVZ_LOG_NOTICE, "pipe: receive pipe %s connected\n",
                   server_sock->recv_pipe);
        }
    }

  /* Try to schedule both of the named pipes for connection.  */
  else
    {
      if (ConnectNamedPipe (send_pipe, server_sock->overlap[SVZ_WRITE]))
        return 0;
      connect = GetLastError ();

      /* Pipe is listening?  */
      if (connect == ERROR_PIPE_LISTENING)
        return 0;
      /* Connection in progress?  */
      else if (connect == ERROR_IO_PENDING)
        server_sock->flags |= SVZ_SOFLG_CONNECTING;
      /* Pipe finally connected?  */
      else if (connect != ERROR_PIPE_CONNECTED)
        {
          svz_log_sys_error ("ConnectNamedPipe");
          return -1;
        }

      if (ConnectNamedPipe (recv_pipe, server_sock->overlap[SVZ_READ]))
        return 0;
      connect = GetLastError ();

      /* Pipe is listening?  */
      if (connect == ERROR_PIPE_LISTENING)
        return 0;
      /* Connection in progress?  */
      else if (connect == ERROR_IO_PENDING)
        server_sock->flags |= SVZ_SOFLG_CONNECTING;
      /* Pipe finally connected?  */
      else if (connect != ERROR_PIPE_CONNECTED)
        {
          svz_log_sys_error ("ConnectNamedPipe");
          return -1;
        }

      /* Both pipes scheduled for connection?  */
      if (server_sock->flags & SVZ_SOFLG_CONNECTING)
        {
          svz_log (SVZ_LOG_NOTICE, "connection scheduled for pipe (%d-%d)\n",
                   recv_pipe, send_pipe);
          return 0;
        }
    }

  /* Create a socket structure for the client pipe.  */
  if ((sock = svz_pipe_create (recv_pipe, send_pipe)) == NULL)
    {
      /* Just disconnect the client pipes.  */
      if (!DisconnectNamedPipe (send_pipe))
        svz_log_sys_error ("DisconnectNamedPipe");
      if (!DisconnectNamedPipe (recv_pipe))
        svz_log_sys_error ("DisconnectNamedPipe");
      return 0;
    }

  /* Copy overlapped structures to client pipes.  */
  if (svz_mingw_at_least_nt4_p ())
    {
      sock->overlap[SVZ_READ] = server_sock->overlap[SVZ_READ];
      sock->overlap[SVZ_WRITE] = server_sock->overlap[SVZ_WRITE];
    }

#else /* not __MINGW32__ */

  return -1;

#endif /* neither HAVE_MKFIFO nor __MINGW32__ */

#if defined (HAVE_MKFIFO) || defined (HAVE_MKNOD) || defined (__MINGW32__)
  sock->read_socket = svz_pipe_read_socket;
  sock->write_socket = svz_pipe_write_socket;
  svz_sock_setreferrer (sock, server_sock);
  svz_sock_bindings_set (sock, server_sock);
  sock->check_request = server_sock->check_request;
  sock->disconnected_socket = server_sock->disconnected_socket;
  sock->idle_func = idle_protect;
  sock->idle_counter = 1;
  svz_sock_resize_buffers (sock, port->send_buffer_size,
                           port->recv_buffer_size);
  svz_sock_enqueue (sock);
  svz_sock_setparent (sock, server_sock);
  sock->proto = server_sock->proto;

  svz_log (SVZ_LOG_NOTICE, "%s: accepting client on pipe (%d-%d)\n",
           server_sock->recv_pipe,
           sock->pipe_desc[SVZ_READ], sock->pipe_desc[SVZ_WRITE]);

  server_sock->flags |= SVZ_SOFLG_INITED;
  svz_sock_setreferrer (server_sock, sock);

  /* Call the ‘check_request’ routine once for greedy protocols.  */
  if (sock->check_request)
    if (sock->check_request (sock))
      svz_sock_schedule_for_shutdown (sock);

  return 0;
#endif /* HAVE_MKFIFO or __MINGW32__ */
}

/*
 * Create a listening server socket (network or pipe).  @var{port} is the
 * port configuration to bind the server socket to.  Return a @code{NULL}
 * pointer on errors.
 */
svz_socket_t *
svz_server_create (svz_portcfg_t *port)
{
  svz_t_socket server_socket; /* server socket descriptor */
  svz_socket_t *sock;         /* socket structure */
  int optval;                 /* value for ‘setsockopt’ */
  struct sockaddr_in *addr;   /* bind address */

  /* Create listening pipe server?  */
  if (port->proto & SVZ_PROTO_PIPE)
    {
      if ((sock = svz_sock_alloc ()) != NULL)
        {
          svz_sock_unique_id (sock);
        }
      else
        {
          svz_log (SVZ_LOG_ERROR, "unable to allocate socket structure\n");
          return NULL;
        }
    }

  /* Create listening TCP, UDP, ICMP or RAW server socket.  */
  else
    {
      /* First, create a server socket for listening.  */
      if ((server_socket = svz_socket_create (port->proto)) ==
          (svz_t_socket) -1)
        return NULL;

      /* Set this ip option if we are using raw sockets.  */
      if (port->proto & SVZ_PROTO_RAW)
        {
#ifdef IP_HDRINCL
          optval = 1;
          if (setsockopt (server_socket, IPPROTO_IP, IP_HDRINCL,
                          (void *) &optval, sizeof (optval)) < 0)
            {
              svz_log_net_error ("setsockopt");
              if (svz_closesocket (server_socket) < 0)
                svz_log_net_error ("close");
              return NULL;
            }
#else /* not IP_HDRINCL */
          svz_closesocket (server_socket);
          svz_log (SVZ_LOG_ERROR, "setsockopt: IP_HDRINCL undefined\n");
          return NULL;
#endif /* IP_HDRINCL */
        }

      /*
       * Make the socket be reusable (Minimize socket deadtime on
       * server death).
       */
      optval = 1;
      if (setsockopt (server_socket, SOL_SOCKET, SO_REUSEADDR,
                      (void *) &optval, sizeof (optval)) < 0)
        {
          svz_log_net_error ("setsockopt");
          if (svz_closesocket (server_socket) < 0)
            svz_log_net_error ("close");
          return NULL;
        }

      /* Fetch the ‘bind’ address.  */
      addr = svz_portcfg_addr (port);

#ifdef SO_BINDTODEVICE
      /* On a Linux 2.x.x you can bind to `eth0' by this control setting
         if root priviledges are ensured.  Includes aliases.  Connections can
         be established from the physical outside only.  */
      if (svz_portcfg_device (port))
        {
          char *device = svz_portcfg_device (port);
          if (setsockopt (server_socket, SOL_SOCKET, SO_BINDTODEVICE,
                          (void *) device, strlen (device) + 1) < 0)
            {
              svz_log_net_error ("setsockopt (%s)", device);
              if (svz_closesocket (server_socket) < 0)
                svz_log_net_error ("close");
              return NULL;
            }
          memset (&addr->sin_addr, 0, sizeof (&addr->sin_addr));
        }
#endif /* SO_BINDTODEVICE */

      /* Second, bind the socket to a port.  */
      if (bind (server_socket, (struct sockaddr *) addr,
                sizeof (struct sockaddr)) < 0)
        {
          svz_log_net_error ("bind");
          if (svz_closesocket (server_socket) < 0)
            svz_log_net_error ("close");
          return NULL;
        }

      /* Prepare for listening on that port (if TCP).  */
      if (port->proto & SVZ_PROTO_TCP)
        {
          if (listen (server_socket, SVZ_CFG_TCP (port, backlog)) < 0)
            {
              svz_log_net_error ("listen");
              if (svz_closesocket (server_socket) < 0)
                svz_log_net_error ("close");
              return NULL;
            }
        }

      /* Create a unique socket structure for the listening server socket.  */
      if ((sock = svz_sock_create (server_socket)) == NULL)
        {
          /* Close the server socket if this routine failed.  */
          if (svz_closesocket (server_socket) < 0)
            svz_log_net_error ("close");
          return NULL;
        }

      /* Modify the port configuration if there was no network port given.
         In this case the systems selects a free one.  */
      if (port->proto & (SVZ_PROTO_TCP | SVZ_PROTO_UDP)
          && addr->sin_port == 0)
        {
          addr->sin_port = sock->local_port;
          if (port->proto & SVZ_PROTO_TCP)
            SVZ_CFG_TCP (port, port) = ntohs (sock->local_port);
          else
            SVZ_CFG_UDP (port, port) = ntohs (sock->local_port);
        }
    }

  /*
   * Free the receive and send buffers not needed for TCP server
   * sockets and PIPE server.
   */
  if (port->proto & (SVZ_PROTO_TCP | SVZ_PROTO_PIPE))
    {
      svz_sock_resize_buffers (sock, 0, 0);
      sock->check_request = svz_sock_detect_proto;
    }

  /* Setup the socket structure.  */
  sock->flags |= SVZ_SOFLG_LISTENING;
  sock->flags &= ~SVZ_SOFLG_CONNECTED;
  sock->proto |= port->proto;

  if (port->proto & SVZ_PROTO_PIPE)
    {
      sock->read_socket = pipe_accept;
      if (svz_pipe_listener (sock,
                             &SVZ_CFG_PIPE (port, recv),
                             &SVZ_CFG_PIPE (port, send)) == -1)
        {
          svz_sock_free (sock);
          return NULL;
        }
    }
  else
    {
      if (port->proto & SVZ_PROTO_TCP)
        {
          sock->read_socket = tcp_accept;
        }
      else if (port->proto & SVZ_PROTO_UDP)
        {
          svz_sock_resize_buffers (sock, 0, 0);
          sock->read_socket = svz_udp_lazy_read_socket;
          sock->write_socket = svz_udp_write_socket;
          sock->check_request = svz_udp_check_request;
        }
      else if (port->proto & SVZ_PROTO_ICMP)
        {
          svz_sock_resize_buffers (sock, 0, 0);
          sock->read_socket = svz_icmp_lazy_read_socket;
          sock->write_socket = svz_icmp_write_socket;
          sock->check_request = svz_icmp_check_request;
          sock->itype = SVZ_CFG_ICMP (port, type);
        }
    }
  {
    char buf[128];

    svz_pp_portcfg (buf, 128, port);
    svz_log (SVZ_LOG_NOTICE, "listening on %s\n", buf);
  }
  return sock;
}
