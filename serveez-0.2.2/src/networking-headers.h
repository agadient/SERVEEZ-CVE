/* networking-headers.h --- <netinet/in.h> et al
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
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

#ifndef __NETWORKING_HEADERS_H__
#define __NETWORKING_HEADERS_H__ 1

#ifdef __MINGW32__
# include <winsock2.h>
/* FIXME: How to DTRT here?
 */
# define IPV6_OK  0
#else
# include <arpa/inet.h>
# include <netinet/in.h>
/* See:
 * http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap02.html
 */
# define IPV6_OK  (0 < _POSIX_IPV6)
#endif

/* We want to prepare for IPv6 but not yet *really* support it,
   hence this lameness.  */
#include <stdlib.h>
#define STILL_NO_V6_DAMMIT(x)                   \
  if (AF_INET != svz_address_family (x))        \
    abort ()

#endif  /* !defined __NETWORKING_HEADERS_H__ */
