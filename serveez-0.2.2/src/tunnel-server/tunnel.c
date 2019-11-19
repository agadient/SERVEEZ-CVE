/*
 * tunnel.c - port forward implementations
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
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>

#ifndef __MINGW32__
# include <sys/socket.h>
#endif

#include "networking-headers.h"
#include "libserveez.h"
#include "tunnel.h"
#include "unused.h"

/*
 * The default tunnel server configuration.
 */
tnl_config_t tnl_config =
{
  NULL, /* the source port to forward from */
  NULL, /* target port to forward to */
  NULL  /* the source client socket hash */
};

/*
 * Defining configuration file associations with key-value-pairs.
 */
svz_key_value_pair_t tnl_config_prototype [] =
{
  SVZ_REGISTER_PORTCFG ("source", tnl_config.source, SVZ_ITEM_NOTDEFAULTABLE),
  SVZ_REGISTER_PORTCFG ("target", tnl_config.target, SVZ_ITEM_NOTDEFAULTABLE),
  SVZ_REGISTER_END ()
};

/*
 * Definition of this server.
 */
svz_servertype_t tnl_server_definition =
{
  "tunnel server",
  "tunnel",
  tnl_global_init,
  tnl_init,
  tnl_detect_proto,
  tnl_connect_socket,
  tnl_finalize,
  tnl_global_finalize,
  NULL,
  NULL,
  NULL,
  NULL,
  tnl_handle_request_udp_source,
  SVZ_CONFIG_DEFINE ("tunnel", tnl_config, tnl_config_prototype)
};

/*
 * The tunnel server's global initializer.
 */
int
tnl_global_init (UNUSED svz_servertype_t *server)
{
  return 0;
}

/*
 * The tunnel server's global finalizer.
 */
int
tnl_global_finalize (UNUSED svz_servertype_t *server)
{
  return 0;
}

static int
proto_support_p (svz_portcfg_t *portcfg)
{
  return portcfg->proto & (SVZ_PROTO_TCP
                           | SVZ_PROTO_ICMP
                           | SVZ_PROTO_UDP
                           | SVZ_PROTO_PIPE);
}

/*
 * Tunnel server instance initializer.  Check the configuration.
 */
int
tnl_init (svz_server_t *server)
{
  tnl_config_t *cfg = server->cfg;
  struct sockaddr_in *addr;

  /* protocol supported?  */
  if (!proto_support_p (cfg->source) || !proto_support_p (cfg->target))
    {
      svz_log (SVZ_LOG_ERROR, "tunnel: protocol not supported\n");
      return -1;
    }

  /* check identity of source and target port configurations */
  if (svz_portcfg_equal (cfg->source, cfg->target) == SVZ_PORTCFG_EQUAL)
    {
      svz_log (SVZ_LOG_ERROR, "tunnel: source and target identical\n");
      return -1;
    }

  if (!(cfg->target->proto & SVZ_PROTO_PIPE))
    {
      /* broadcast target ip address not allowed */
      addr = svz_portcfg_addr (cfg->target);
      if (addr->sin_addr.s_addr == INADDR_ANY)
        {
          svz_log (SVZ_LOG_ERROR, "tunnel: broadcast target ip not allowed\n");
          return -1;
        }
    }

  /* create source client hash (for UDP and ICMP only) */
  cfg->client = svz_hash_create (4, svz_free);

  /* assign the appropriate handle request routine of the server */
  if (cfg->source->proto & SVZ_PROTO_UDP)
    server->handle_request = tnl_handle_request_udp_source;
  if (cfg->source->proto & SVZ_PROTO_ICMP)
    server->handle_request = tnl_handle_request_icmp_source;

  return 0;
}

/*
 * The tunnel server instance finalizer.
 */
int
tnl_finalize (svz_server_t *server)
{
  tnl_config_t *cfg = server->cfg;

  /* release source connection hash if necessary */
  svz_hash_destroy (cfg->client);

  return 0;
}

/*
 * Create a hash string (key) for the source client hash.  Identifiers
 * are the remote ip address and port.
 */
static char *
tnl_addr (svz_socket_t *sock)
{
  static char buf[64];

  SVZ_PP_ADDR_PORT (buf, sock->remote_addr, sock->remote_port);
  return buf;
}

/*
 * Release referring tunnel structure.
 */
static void
tnl_free_connect (svz_socket_t *sock)
{
  if (sock->data)
    {
      svz_free (sock->data);
      sock->data = NULL;
    }
}

/*
 * Create a referring tunnel connection structure.
 */
static tnl_connect_t *
tnl_create_connect (void)
{
  return svz_calloc (sizeof (tnl_connect_t));
}

static void
resize_buffers (svz_socket_t *sock)
{
  svz_sock_resize_buffers (sock, SVZ_UDP_BUF_SIZE, SVZ_UDP_BUF_SIZE);
}

/*
 * Depending on the given socket structure target flag this routine
 * tries to connect to the servers target configuration and delivers a
 * new socket structure or NULL if it failed.
 */
static svz_socket_t *
tnl_create_socket (svz_socket_t *sock, int source)
{
  tnl_config_t *cfg = sock->cfg;
  svz_address_t *ip = NULL;
  in_port_t port = 0;
  svz_socket_t *xsock = NULL;
  struct sockaddr_in *addr;
  char buf[64];

  /* get host and target ip if necessary */
  if (!(cfg->target->proto & SVZ_PROTO_PIPE))
    {
      addr = svz_portcfg_addr (cfg->target);
      ip = svz_address_make (AF_INET, &addr->sin_addr.s_addr);
      port = addr->sin_port;
    }

  /*
   * Depending on the target configuration we assign different
   * callbacks, set other flags and use various connection routines.
   */
  switch (cfg->target->proto)
    {
    case SVZ_PROTO_TCP:
      sock->userflags |= TNL_FLAG_TGT_TCP;
      break;
    case SVZ_PROTO_UDP:
      sock->userflags |= TNL_FLAG_TGT_UDP;
      break;
    case SVZ_PROTO_ICMP:
      sock->userflags |= TNL_FLAG_TGT_ICMP;
      break;
    case SVZ_PROTO_PIPE:
      sock->userflags |= TNL_FLAG_TGT_PIPE;
      break;
    default:
      svz_log (SVZ_LOG_ERROR, "tunnel: invalid target configuration\n");
      goto unlucky;
    }

  /* target is a TCP connection */
  if (sock->userflags & TNL_FLAG_TGT_TCP)
    {
      if ((xsock = svz_tcp_connect (ip, port)) == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "tunnel: tcp: cannot connect to %s\n",
                   SVZ_PP_ADDR_PORT (buf, ip, port));
          goto unlucky;
        }

#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "tunnel: tcp: connecting to %s\n",
               SVZ_PP_ADDR_PORT (buf, ip, port));
#endif /* ENABLE_DEBUG */
      xsock->check_request = tnl_check_request_tcp_target;
      resize_buffers (xsock);
    }

  /* target is an UDP connection */
  else if (sock->userflags & TNL_FLAG_TGT_UDP)
    {
      if ((xsock = svz_udp_connect (ip, port)) == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "tunnel: udp: cannot connect to %s\n",
                   SVZ_PP_ADDR_PORT (buf, ip, port));
          goto unlucky;
        }

#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "tunnel: udp: connecting to %s\n",
               SVZ_PP_ADDR_PORT (buf, ip, port));
#endif /* ENABLE_DEBUG */
      xsock->handle_request = tnl_handle_request_udp_target;
      xsock->idle_func = tnl_idle;
      xsock->idle_counter = TNL_TIMEOUT;
    }

  /* target is an ICMP connection */
  else if (sock->userflags & TNL_FLAG_TGT_ICMP)
    {
      if ((xsock = svz_icmp_connect (ip, port, SVZ_CFG_ICMP
                                     (cfg->target, type)))
          == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "tunnel: icmp: cannot connect to %s\n",
                   SVZ_PP_ADDR (buf, ip));
          goto unlucky;
        }

#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "tunnel: icmp: connecting to %s\n",
               SVZ_PP_ADDR (buf, ip));
#endif /* ENABLE_DEBUG */
      xsock->handle_request = tnl_handle_request_icmp_target;
      xsock->idle_func = tnl_idle;
      xsock->idle_counter = TNL_TIMEOUT;
    }

  /* target is a pipe connection */
  else if (sock->userflags & TNL_FLAG_TGT_PIPE)
    {
      svz_pipe_t *r = &SVZ_CFG_PIPE (cfg->target, recv);
      svz_pipe_t *s = &SVZ_CFG_PIPE (cfg->target, send);

      if ((xsock = svz_pipe_connect (r, s)) == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "tunnel: pipe: cannot connect to %s\n",
                   s->name);
          return NULL;
        }

#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "tunnel: pipe: connecting to %s\n",
               s->name);
#endif /* ENABLE_DEBUG */
      xsock->check_request = tnl_check_request_pipe_target;
      resize_buffers (xsock);
    }

  xsock->cfg = cfg;
  xsock->flags |= SVZ_SOFLG_NOFLOOD;
  xsock->userflags = (sock->userflags | source) & ~(TNL_FLAG_TGT);
  xsock->disconnected_socket = tnl_disconnect_target;

 unlucky:
  if (ip)
    svz_free (ip);
  return xsock;
}

/*
 * Forward a given packet with a certain length to a target connection.
 * This routine can be used by all source connection handler passing
 * the targets socket and its own userflags.
 */
static int
tnl_send_request_source (svz_socket_t *sock, char *packet, int len, int flag)
{
  /* target is TCP or PIPE */
  if (flag & (TNL_FLAG_TGT_TCP | TNL_FLAG_TGT_PIPE))
    {
      if (svz_sock_write (sock, packet, len) == -1)
        return -1;
    }
  /* target is UDP */
  else if (flag & TNL_FLAG_TGT_UDP)
    {
      if (svz_udp_write (sock, packet, len) == -1)
        return -1;
    }
  /* target is ICMP */
  else if (flag & TNL_FLAG_TGT_ICMP)
    {
      if (svz_icmp_write (sock, packet, len) == -1)
        return -1;
    }

  return 0;
}

/*
 * Forward a given packet with a certain length to a source connection.
 * This routine can be used by all target connection handler passing
 * the sources socket and its own userflags.
 */
static int
tnl_send_request_target (svz_socket_t *sock, char *packet, int len, int flag)
{
  /* source is TCP or PIPE */
  if (flag & (TNL_FLAG_SRC_TCP | TNL_FLAG_SRC_PIPE))
    {
      if (svz_sock_write (sock, packet, len) == -1)
        return -1;
    }
  /* source is UDP */
  else if (flag & TNL_FLAG_SRC_UDP)
    {
      if (svz_udp_write (sock, packet, len) == -1)
        return -1;
    }
  /* source is ICMP */
  else if (flag & TNL_FLAG_SRC_ICMP)
    {
      if (svz_icmp_write (sock, packet, len) == -1)
        return -1;
    }

  return 0;
}

/*
 * Tunnel server TCP and pipe detection routine.  It is greedy.  Thus it cannot
 * share the port configuration with other TCP or pipe servers.
 */
int
tnl_detect_proto (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  svz_log (SVZ_LOG_NOTICE, "tunnel: %s connection accepted\n",
           sock->flags & SVZ_SOFLG_PIPE ? "pipe" : "tcp");
  return -1;
}

/*
 * If any TCP or pipe connection has been accepted this routine is called
 * to setup the tunnel server specific callbacks.
 */
int
tnl_connect_socket (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  svz_socket_t *xsock = NULL;
  tnl_connect_t *source;

  sock->flags |= SVZ_SOFLG_NOFLOOD;
  sock->check_request = sock->flags & SVZ_SOFLG_PIPE ?
    tnl_check_request_pipe_source : tnl_check_request_tcp_source;
  sock->disconnected_socket = tnl_disconnect_source;
  resize_buffers (sock);

  /* try connecting to target */
  xsock = tnl_create_socket (sock, sock->flags & SVZ_SOFLG_PIPE ?
                             TNL_FLAG_SRC_PIPE : TNL_FLAG_SRC_TCP);
  if (xsock == NULL)
    return -1;

  /* put the source connection into data field */
  source = tnl_create_connect ();
  source->source_sock = sock;
  source->target_sock = xsock;
  svz_address_to (&source->ip, sock->remote_addr);
  source->port = sock->remote_port;
  xsock->data = source;
  sock->data = source;

  return 0;
}

/*
 * The tunnel servers TCP check_request routine for target connections.
 * Each TCP target connection gets assigned this function in order to
 * send data back to its source connection.
 */
int
tnl_check_request_tcp_target (svz_socket_t *sock)
{
  svz_socket_t *xsock = NULL;
  tnl_connect_t *source = sock->data;

#if ENABLE_DEBUG
  if (source == NULL || source->source_sock == NULL)
    {
      svz_log (SVZ_LOG_FATAL, "tunnel: tcp target has no source connection\n");
      return -1;
    }
#endif /* ENABLE_DEBUG */

  /* obtain source connection */
  xsock = source->source_sock;
  SVZ_SET_ADDR (xsock->remote_addr, AF_INET, &source->ip);
  xsock->remote_port = source->port;

  /* forward data to source connection */
  if (tnl_send_request_target (xsock, sock->recv_buffer,
                               sock->recv_buffer_fill, sock->userflags) == -1)
    {
      svz_sock_schedule_for_shutdown (xsock);
      return -1;
    }

  /* empty the receive buffer of this target connection */
  sock->recv_buffer_fill = 0;
  return 0;
}

/*
 * The tunnel servers TCP ‘check_request’ routine for the source connections.
 * It simply copies the received data to the send buffer of the target
 * connection.
 */
int
tnl_check_request_tcp_source (svz_socket_t *sock)
{
  tnl_connect_t *target = sock->data;
  svz_socket_t *xsock;

#if ENABLE_DEBUG
  if (target == NULL || target->target_sock == NULL)
    {
      svz_log (SVZ_LOG_FATAL, "tunnel: tcp source has no target connection\n");
      return -1;
    }
#endif /* ENABLE_DEBUG */

  /* obtain target connection */
  xsock = target->target_sock;

  /* forward data to target connection */
  if (tnl_send_request_source (xsock, sock->recv_buffer,
                               sock->recv_buffer_fill, sock->userflags) == -1)
    {
      return -1;
    }

  sock->recv_buffer_fill = 0;
  return 0;
}

/*
 * This function is the ‘handle_request’ routine for target UDP sockets.
 */
int
tnl_handle_request_udp_target (svz_socket_t *sock, char *packet, int len)
{
  tnl_connect_t *source = sock->data;
  svz_socket_t *xsock = NULL;

#if ENABLE_DEBUG
  if (source == NULL || source->source_sock == NULL)
    {
      svz_log (SVZ_LOG_FATAL, "tunnel: udp target has no source connection\n");
      return -1;
    }
#endif /* ENABLE_DEBUG */

  /* get source connection from data field */
  xsock = source->source_sock;
  SVZ_SET_ADDR (xsock->remote_addr, AF_INET, &source->ip);
  xsock->remote_port = source->port;

  /* forward packet data to source connection */
  if (tnl_send_request_target (xsock, packet, len, sock->userflags) == -1)
    {
      /* shutdown the source connection if it is TCP or pipe */
      if (sock->userflags & (TNL_FLAG_SRC_TCP | TNL_FLAG_SRC_PIPE))
        svz_sock_schedule_for_shutdown (xsock);
      return -1;
    }

  /* returning zero means that the packet has been processed */
  return 0;
}

/*
 * This function is the ‘handle_request’ routine for source UDP sockets.
 * It accepts UDP connections (listening connection) or forwards data
 * to existing target sockets.
 */
int
tnl_handle_request_udp_source (svz_socket_t *sock, char *packet, int len)
{
  tnl_config_t *cfg = sock->cfg;
  tnl_connect_t *source;
  svz_socket_t *xsock = NULL;

  /* check if there is such a connection in the source hash already */
  source = svz_hash_get (cfg->client, tnl_addr (sock));
  if (source)
    {
      /* get existing target socket */
      xsock = source->target_sock;
    }
  else
    {
      /* start connecting to a new target */
      if ((xsock = tnl_create_socket (sock, TNL_FLAG_SRC_UDP)) == NULL)
        return 0;

      /* foreign address not in hash, create new target connection */
      source = tnl_create_connect ();
      source->source_sock = sock;
      svz_address_to (&source->ip, sock->remote_addr);
      source->port = sock->remote_port;
      svz_hash_put (cfg->client, tnl_addr (sock), source);

      /* put the source connection into data field of target */
      xsock->data = source;
      source->target_sock = xsock;
    }

  /* forward packet data to target connection */
  if (tnl_send_request_source (xsock, packet, len, sock->userflags) == -1)
    {
      svz_sock_schedule_for_shutdown (xsock);
      return 0;
    }

  return 0;
}

/*
 * This function is the handle_request routine for target ICMP sockets.
 */
int
tnl_handle_request_icmp_target (svz_socket_t *sock, char *packet, int len)
{
  tnl_connect_t *source = sock->data;
  svz_socket_t *xsock = NULL;

#if ENABLE_DEBUG
  if (source == NULL || source->source_sock == NULL)
    {
      svz_log (SVZ_LOG_FATAL,
               "tunnel: icmp target has no source connection\n");
      return -1;
    }
#endif /* ENABLE_DEBUG */

  /* get source connection from data field */
  xsock = source->source_sock;
  SVZ_SET_ADDR (xsock->remote_addr, AF_INET, &source->ip);
  xsock->remote_port = source->port;

  /* forward packet data to source connection */
  if (tnl_send_request_target (xsock, packet, len, sock->userflags) == -1)
    {
      /* shutdown source connection if it is TCP or pipe */
      if (sock->userflags & (TNL_FLAG_SRC_TCP | TNL_FLAG_SRC_PIPE))
        svz_sock_schedule_for_shutdown (xsock);
      return -1;
    }

  /* packet successfully processed */
  return 0;
}

/*
 * This function is the ‘handle_request’ routine for source ICMP sockets.
 * It accepts ICMP connections (listening connection) or forwards data
 * to existing target sockets.
 */
int
tnl_handle_request_icmp_source (svz_socket_t *sock, char *packet, int len)
{
  tnl_config_t *cfg = sock->cfg;
  tnl_connect_t *source;
  svz_socket_t *xsock = NULL;

  /* check if there is such a connection in the source hash already */
  source = svz_hash_get (cfg->client, tnl_addr (sock));
  if (source)
    {
      /* get existing target socket */
      xsock = source->target_sock;
    }
  else
    {
      /* start connecting */
      if ((xsock = tnl_create_socket (sock, TNL_FLAG_SRC_ICMP)) == NULL)
        return 0;

      /* foreign address not in hash, create new target connection */
      source = tnl_create_connect ();
      source->source_sock = sock;
      svz_address_to (&source->ip, sock->remote_addr);
      source->port = sock->remote_port;
      svz_hash_put (cfg->client, tnl_addr (sock), source);

      /* put the source connection into data field */
      xsock->data = source;
      source->target_sock = xsock;
    }

  /* forward packet data to target connection */
  if (tnl_send_request_source (xsock, packet, len, sock->userflags) == -1)
    {
      svz_sock_schedule_for_shutdown (xsock);
      return 0;
    }

  return 0;
}

/*
 * The targets's disconnection routine for types of targets (TCP, PIPE,
 * ICMP and UDP).
 */
int
tnl_disconnect_target (svz_socket_t *sock)
{
  tnl_config_t *cfg = sock->cfg;
  tnl_connect_t *source = sock->data;
  svz_socket_t *xsock;
  char *key;

  /* do not do anything if we are shutting down */
  if (svz_shutting_down_p ())
    {
      /* if source is TCP or PIPE then shutdown referring connection */
      if (sock->userflags & (TNL_FLAG_SRC_TCP | TNL_FLAG_SRC_PIPE))
        {
          xsock = source ? source->source_sock : NULL;
          if (xsock)
            xsock->data = NULL;
          tnl_free_connect (sock);
        }
      return 0;
    }

#if ENABLE_DEBUG
  if (source == NULL || source->source_sock == NULL)
    {
      svz_log (SVZ_LOG_FATAL, "tunnel: target has no source connection\n");
      return -1;
    }
#endif /* ENABLE_DEBUG */

  /* obtain source connection */
  xsock = source->source_sock;

  /* if the source connection is ICMP send a disconnection message */
  if (sock->userflags & TNL_FLAG_SRC_ICMP)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG,
               "tunnel: sending icmp disconnect on socket id %d\n",
               xsock->id);
#endif /* ENABLE_DEBUG */
      svz_icmp_send_control (xsock, SVZ_ICMP_SERVEEZ_CLOSE);
    }

  /* if source is TCP or PIPE then shutdown referring connection */
  if (sock->userflags & (TNL_FLAG_SRC_TCP | TNL_FLAG_SRC_PIPE))
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "tunnel: shutdown referrer id %d\n", xsock->id);
#endif /* ENABLE_DEBUG */
      svz_sock_schedule_for_shutdown (xsock);
      xsock->data = NULL;
      tnl_free_connect (sock);
    }
  /* else delete target connection from its hash */
  else if ((key = svz_hash_contains (cfg->client, source)) != NULL)
    {
      svz_hash_delete (cfg->client, key);
      tnl_free_connect (sock);
    }

  return 0;
}

/*
 * What happens if a source connection gets lost.
 */
int
tnl_disconnect_source (svz_socket_t *sock)
{
  tnl_connect_t *target = sock->data;
  svz_socket_t *xsock;

  /* if target is TCP or PIPE shutdown referring connection */
  if (sock->userflags & (TNL_FLAG_TGT_TCP | TNL_FLAG_TGT_PIPE))
    {
#if ENABLE_DEBUG
      if (target == NULL || target->target_sock == NULL)
        {
          svz_log (SVZ_LOG_DEBUG, "tunnel: tcp/pipe source has no "
                   "target connection\n");
          return -1;
        }
#endif /* ENABLE_DEBUG */

      /* get target connection */
      xsock = target->target_sock;

#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "tunnel: shutdown referrer id %d\n", xsock->id);
#endif /* ENABLE_DEBUG */
      svz_sock_schedule_for_shutdown (xsock);
      xsock->data = NULL;
      tnl_free_connect (sock);
    }

  return 0;
}

/*
 * Because UDP and ICMP sockets cannot not be detected as being closed
 * we need to shutdown target sockets ourselves.
 */
int
tnl_idle (svz_socket_t *sock)
{
  time_t t = time (NULL);

  if (t - sock->last_recv < TNL_TIMEOUT || t - sock->last_send < TNL_TIMEOUT)
    {
      sock->idle_counter = TNL_TIMEOUT;
      return 0;
    }
  return -1;
}
