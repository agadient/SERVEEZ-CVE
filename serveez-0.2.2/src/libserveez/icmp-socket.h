/*
 * icmp-socket.h - ICMP socket definitions and declarations
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

#ifndef __ICMP_SOCKET_H__
#define __ICMP_SOCKET_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/socket.h"
#include "libserveez/address.h"
/* end svzint */
/* begin svzint */
#define ICMP_HEADER_SIZE 10
#define ICMP_MSG_SIZE    (64 * 1024)
#define ICMP_BUF_SIZE    (4 * (ICMP_MSG_SIZE + ICMP_HEADER_SIZE + 24))
/* end svzint */

/* Serveez-ICMP types and sub-codes.  */
#define SVZ_ICMP_SERVEEZ        42
#define SVZ_ICMP_SERVEEZ_DATA    0
#define SVZ_ICMP_SERVEEZ_REQ     1
#define SVZ_ICMP_SERVEEZ_ACK     2
#define SVZ_ICMP_SERVEEZ_CLOSE   3
#define SVZ_ICMP_SERVEEZ_CONNECT 4

__BEGIN_DECLS

SBO int svz_icmp_lazy_read_socket (svz_socket_t *);
SBO int svz_icmp_write_socket (svz_socket_t *);
SBO int svz_icmp_check_request (svz_socket_t *);
SERVEEZ_API svz_socket_t *svz_icmp_connect (svz_address_t *, in_port_t, uint8_t);
SERVEEZ_API int svz_icmp_send_control (svz_socket_t *, uint8_t);
SERVEEZ_API int svz_icmp_write (svz_socket_t *, char *, int);

__END_DECLS

#endif /* !__ICMP_SOCKET_H__ */
