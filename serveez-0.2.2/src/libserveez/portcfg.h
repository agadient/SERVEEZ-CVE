/*
 * portcfg.h - port configuration interface
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __PORTCFG_H__
#define __PORTCFG_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/array.h"
#include "libserveez/hash.h"
#include "libserveez/pipe-socket.h"
/* end svzint */

/* Port configuration items.  */
#define SVZ_PORTCFG_ANY   "any"
#define SVZ_PORTCFG_NOIP  "*"

/* begin svzint */
/* Port configuration flags.  */
#define PORTCFG_FLAG_ANY    0x0001
#define PORTCFG_FLAG_ALL    0x0002
#define PORTCFG_FLAG_DEVICE 0x0004
/* end svzint */

/* Return values for port configuration comparisons.  */
#define SVZ_PORTCFG_NOMATCH  0x0001
#define SVZ_PORTCFG_EQUAL    0x0002
#define SVZ_PORTCFG_MATCH    0x0004
#define SVZ_PORTCFG_CONFLICT 0x0008

/*
 * Definition of a single port configuration reflecting either a network
 * (TCP, UDP, ICMP or RAW) or filesystem configuration (PIPE).
 */
typedef struct svz_portcfg
{
  /* the symbolic name of this port configuration */
  char *name;

  /* one of the PROTO_ flags defined in <core.h> */
  int proto;

  /* one of PORTCFG_FLAG_ flags */
  int flags;

  /* unified structure for each type of port */
  union protocol_t
  {
    /* tcp port */
    struct tcp_t
    {
      in_port_t port;          /* TCP/IP port */
      char *ipaddr;            /* dotted decimal or "*" for any address */
      struct sockaddr_in addr; /* converted from the above 2 values */
      char *device;            /* network device */
      int backlog;             /* backlog argument for ‘listen’ */
    } tcp;

    /* udp port */
    struct udp_t
    {
      in_port_t port;          /* UDP port */
      char *ipaddr;            /* dotted decimal or "*" */
      struct sockaddr_in addr; /* converted from the above 2 values */
      char *device;            /* network device */
    } udp;

    /* icmp port */
    struct icmp_t
    {
      char *ipaddr;            /* dotted decimal or "*" */
      struct sockaddr_in addr; /* converted from the above value */
      char *device;            /* network device */
      uint8_t type;            /* message type */
    } icmp;

    /* raw ip port */
    struct raw_t
    {
      char *ipaddr;            /* dotted decimal or "*" */
      struct sockaddr_in addr; /* converted from the above value */
      char *device;            /* network device */
    } raw;

    /* pipe port */
    struct pipe_t
    {
      svz_pipe_t recv;         /* pipe for sending data into serveez */
      svz_pipe_t send;         /* pipe serveez sends responses out on */
    } pipe;
  }
  protocol;

  /* maximum number of bytes for protocol identification */
  int detection_fill;

  /* maximum seconds for protocol identification */
  int detection_wait;

  /* initial buffer sizes */
  int send_buffer_size;
  int recv_buffer_size;

  /* allowed number of connects per second (hammer protection) */
  int connect_freq;

  /* remembers connect frequency for each ip */
  svz_hash_t *accepted;

  /* denied and allowed access list (ip based) */
  svz_array_t *deny;
  svz_array_t *allow;
}
svz_portcfg_t;

/*
 * Accessor definitions for each type of protocol.
 */
#define SVZ_CFG_TCP(portcfg,x)     (portcfg->protocol.tcp.x)
#define SVZ_CFG_UDP(portcfg,x)     (portcfg->protocol.udp.x)
#define SVZ_CFG_ICMP(portcfg,x)    (portcfg->protocol.icmp.x)
#define SVZ_CFG_RAW(portcfg,x)     (portcfg->protocol.raw.x)
#define SVZ_CFG_PIPE(portcfg,x)    (portcfg->protocol.pipe.x)

__BEGIN_DECLS
SBO void svz_portcfg_free (svz_portcfg_t *);
SBO void svz_portcfg_prepare (svz_portcfg_t *);
SBO svz_array_t *svz_portcfg_expand (svz_portcfg_t *);

SERVEEZ_API struct sockaddr_in *svz_portcfg_addr (svz_portcfg_t *);
SERVEEZ_API char *svz_portcfg_ipaddr (svz_portcfg_t *);
SERVEEZ_API char *svz_portcfg_device (svz_portcfg_t *);
SERVEEZ_API in_port_t svz_portcfg_port (svz_portcfg_t *);

SERVEEZ_API svz_portcfg_t *svz_portcfg_create (void);
SERVEEZ_API int svz_portcfg_equal (svz_portcfg_t *, svz_portcfg_t *);
SERVEEZ_API svz_portcfg_t *svz_portcfg_add (char *, svz_portcfg_t *);
SERVEEZ_API svz_portcfg_t *svz_portcfg_get (char *);
SERVEEZ_API void svz_portcfg_destroy (svz_portcfg_t *);
SERVEEZ_API int svz_portcfg_mkaddr (svz_portcfg_t *);
SBO size_t svz_pp_portcfg (char *, size_t, svz_portcfg_t *);
SERVEEZ_API svz_portcfg_t *svz_portcfg_dup (svz_portcfg_t *);

__END_DECLS

#endif /* __PORTCFG_H__ */
