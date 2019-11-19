/*
 * tunnel.h - port forward definition header
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __TUNNEL_H__
#define __TUNNEL_H__ 1

/*
 * Tunnel server configuration structure.
 */
typedef struct
{
  svz_portcfg_t *source; /* the source port to forward from */
  svz_portcfg_t *target; /* target port to forward to */
  svz_hash_t *client;    /* source client hash */
}
tnl_config_t;

/* the referrer connection structure */
typedef struct
{
  in_addr_t ip;              /* the ip address to send to */
  in_port_t port;            /* port to send to */
  svz_socket_t *source_sock; /* source socket structure */
  svz_socket_t *target_sock; /* target socket */
}
tnl_connect_t;

/* tunnel server specific protocol flags */
#define TNL_TIMEOUT       30

/* flags for targets */
#define TNL_FLAG_SRC_TCP  0x0001
#define TNL_FLAG_SRC_UDP  0x0002
#define TNL_FLAG_SRC_ICMP 0x0004
#define TNL_FLAG_SRC_PIPE 0x0008
#define TNL_FLAG_SRC (TNL_FLAG_SRC_TCP  | TNL_FLAG_SRC_UDP | \
                      TNL_FLAG_SRC_ICMP | TNL_FLAG_SRC_PIPE)

/* flags for sources */
#define TNL_FLAG_TGT_TCP  0x0010
#define TNL_FLAG_TGT_UDP  0x0020
#define TNL_FLAG_TGT_ICMP 0x0040
#define TNL_FLAG_TGT_PIPE 0x0080
#define TNL_FLAG_TGT (TNL_FLAG_TGT_TCP  | TNL_FLAG_TGT_UDP | \
                      TNL_FLAG_TGT_ICMP | TNL_FLAG_TGT_PIPE)

/*
 * Basic server callback definitions.
 */
int tnl_init (svz_server_t *server);
int tnl_global_init (svz_servertype_t *server);
int tnl_finalize (svz_server_t *server);
int tnl_global_finalize (svz_servertype_t *server);

/* Rest of all the callbacks.  */
int tnl_detect_proto (svz_server_t *server, svz_socket_t *sock);
int tnl_connect_socket (svz_server_t *server, svz_socket_t *sock);
int tnl_check_request_tcp_source (svz_socket_t *sock);
int tnl_check_request_tcp_target (svz_socket_t *sock);
int tnl_handle_request_udp_source (svz_socket_t *sock, char *packet, int len);
int tnl_handle_request_udp_target (svz_socket_t *sock, char *packet, int len);
int tnl_handle_request_icmp_source (svz_socket_t *sock, char *packet, int len);
int tnl_handle_request_icmp_target (svz_socket_t *sock, char *packet, int len);
int tnl_disconnect_source (svz_socket_t *sock);
int tnl_disconnect_target (svz_socket_t *sock);
int tnl_idle (svz_socket_t *sock);

#define tnl_check_request_pipe_source tnl_check_request_tcp_source
#define tnl_check_request_pipe_target tnl_check_request_tcp_target

/*
 * This server's definition.
 */
extern svz_servertype_t tnl_server_definition;

#endif /* not __TUNNEL_H__ */
