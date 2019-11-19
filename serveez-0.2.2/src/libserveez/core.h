/*
 * core.h - socket and file descriptor declarations and definitions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __CORE_H__
#define __CORE_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/array.h"
#include "libserveez/address.h"
/* end svzint */

/* protocol definitions */
#define SVZ_PROTO_TCP   0x00000001 /* tcp  - bidirectional, reliable */
#define SVZ_PROTO_UDP   0x00000002 /* udp  - multidirectional, unreliable */
#define SVZ_PROTO_PIPE  0x00000004 /* pipe - unidirectional, reliable */
#define SVZ_PROTO_ICMP  0x00000008 /* icmp - multidirectional, unreliable */
#define SVZ_PROTO_RAW   0x00000010 /* raw  - multidirectional, unreliable */

/* Silence the "declared inside parameter list" warning.  */
struct stat;

__BEGIN_DECLS

SBO int svz_fd_nonblock (int);
SBO int svz_fd_block (int);
SERVEEZ_API int svz_fd_cloexec (int);
SERVEEZ_API int svz_tcp_cork (svz_t_socket, int);
SERVEEZ_API int svz_tcp_nodelay (svz_t_socket, int, int *);
SBO int svz_socket_connect (svz_t_socket, svz_address_t *, in_port_t);
SBO svz_t_socket svz_socket_create (int);
SERVEEZ_API int svz_closesocket (svz_t_socket);
SBO int svz_socket_create_pair (int, svz_t_socket desc[2]);
SERVEEZ_API char *svz_inet_ntoa (in_addr_t);
SERVEEZ_API int svz_inet_aton (char *, struct sockaddr_in *);
SERVEEZ_API int svz_sendfile (int, int, off_t *, size_t);
SERVEEZ_API int svz_open (const char *, int, mode_t);
SERVEEZ_API int svz_close (int);
SERVEEZ_API int svz_fstat (int, struct stat *);
SERVEEZ_API FILE *svz_fopen (const char *, const char *);
SERVEEZ_API int svz_fclose (FILE *);

/* begin svzint */
#ifndef __MINGW32__
SBO void svz_file_closeall (void);
#endif
/* end svzint */

__END_DECLS

#endif /* not __CORE_H__ */
