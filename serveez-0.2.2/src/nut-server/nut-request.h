/*
 * nut-request.h - gnutella requests header file
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __NUT_REQUEST_H__
#define __NUT_REQUEST_H__ 1

/* Exported functions.  */
in_addr_t nut_v4addr_from (nut_config_t *cfg,
                           struct sockaddr_in *addr,
                           svz_socket_t *sock);
int nut_reply (svz_socket_t *sock, nut_header_t *hdr, uint8_t *packet);
int nut_push_request (svz_socket_t *sock, nut_header_t *hdr,
                      uint8_t *packet);
int nut_query (svz_socket_t *sock, nut_header_t *hdr, uint8_t *packet);
int nut_pong (svz_socket_t *sock, nut_header_t *hdr, uint8_t *packet);
int nut_ping (svz_socket_t *sock, nut_header_t *hdr, uint8_t *null);

#endif /* not __NUT_REQUEST_H__ */
