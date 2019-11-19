/*
 * tcp-socket.h - TCP socket connection definition
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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

#ifndef __TCP_SOCKET_H__
#define __TCP_SOCKET_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/socket.h"
#include "libserveez/address.h"
/* end svzint */

__BEGIN_DECLS

SERVEEZ_API svz_socket_t *svz_tcp_connect (svz_address_t *, in_port_t);
SERVEEZ_API int svz_tcp_read_socket (svz_socket_t *);
SBO int svz_tcp_write_socket (svz_socket_t *);
SBO int svz_tcp_recv_oob (svz_socket_t *);
SERVEEZ_API int svz_tcp_send_oob (svz_socket_t *);

__END_DECLS

#endif /* not __TCP_SOCKET_H__ */
