/*
 * soprop.h - socket property
 *
 * Copyright (C) 2013 Thien-Thi Nguyen
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

#ifndef __SOPROP_H__
#define __SOPROP_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/alloc.h"
#include "libserveez/hash.h"
#include "libserveez/socket.h"
/* end svzint */

__BEGIN_DECLS
SBO svz_hash_t *svz_soprop_create (size_t, svz_free_func_t);
SBO void *svz_soprop_put (svz_hash_t *, const svz_socket_t *, void *);
SBO void *svz_soprop_get (svz_hash_t *, const svz_socket_t *);
SBO void svz_soprop_destroy (svz_hash_t *);
__END_DECLS

#endif /* not __SOPROP_H__ */
