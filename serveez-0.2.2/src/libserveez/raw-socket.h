/*
 * raw-socket.h - raw ip socket header definitions
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

#ifndef __RAW_SOCKET_H__
#define __RAW_SOCKET_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
/* end svzint */

__BEGIN_DECLS

SBO uint16_t svz_raw_ip_checksum (uint8_t *, int);
SBO int svz_raw_check_ip_header (uint8_t *, int);

__END_DECLS

#endif /* not __RAW_SOCKET_H__ */
