/*
 * udp-socket.h - udp socket header definitions
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

#ifndef __UDP_SOCKET_H__
#define __UDP_SOCKET_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/address.h"
#include "libserveez/socket.h"
/* end svzint */

/* The maximum size of a UDP packet.  */
#define SVZ_UDP_MSG_SIZE  (64 * 1024)

/* Space for 4 messages.  */
#define SVZ_UDP_BUF_SIZE  (4 * (SVZ_UDP_MSG_SIZE + 24))

__BEGIN_DECLS

SBO int svz_udp_lazy_read_socket (svz_socket_t *);
SBO int svz_udp_write_socket (svz_socket_t *);
SBO int svz_udp_check_request (svz_socket_t *);
SERVEEZ_API svz_socket_t *svz_udp_connect (svz_address_t *, in_port_t);
SERVEEZ_API int svz_udp_write (svz_socket_t *, char *, int);

__END_DECLS

#endif /* not __UDP_SOCKET_H__ */
