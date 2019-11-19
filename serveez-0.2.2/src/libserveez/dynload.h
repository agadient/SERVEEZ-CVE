/*
 * dynload.h - dynamic server loading interface
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __DYNLOAD_H__
#define __DYNLOAD_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
/* end svzint */

__BEGIN_DECLS

SBO svz_servertype_t *svz_servertype_load (char *);
SERVEEZ_API void svz_dynload_path_set (svz_array_t *);
SERVEEZ_API svz_array_t *svz_dynload_path_get (void);

__END_DECLS

#endif /* not __DYNLOAD_H__ */
