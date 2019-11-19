/*
 * array.h - array declarations
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2001 Raimund Jacob <raimi@lkcc.org>
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

#ifndef __ARRAY_H__
#define __ARRAY_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/alloc.h"
/* end svzint */

typedef struct svz_array svz_array_t;

__BEGIN_DECLS

SERVEEZ_API svz_array_t *svz_array_create (size_t, svz_free_func_t);
SERVEEZ_API void svz_array_destroy (svz_array_t *);
SERVEEZ_API void *svz_array_get (svz_array_t *, size_t);
SERVEEZ_API void *svz_array_set (svz_array_t *, size_t, void *);
SERVEEZ_API void svz_array_add (svz_array_t *, void *);
SERVEEZ_API void *svz_array_del (svz_array_t *, size_t);
SERVEEZ_API size_t svz_array_size (svz_array_t *);
SBO svz_array_t *svz_array_dup (svz_array_t *);
SBO svz_array_t *svz_array_strdup (svz_array_t *);
SBO svz_array_t *svz_array_destroy_zero (svz_array_t *);

__END_DECLS

/**
 * Expand into a @code{for}-statement header, for iterating over
 * @var{array}.  On each cycle, @var{value} is assigned to successive
 * elements of @var{array}, and @var{i} the element's position.
 */
#define svz_array_foreach(array, value, i)                      \
  for ((i) = 0, (value) = svz_array_get ((array), 0);           \
       (array) && (i) < svz_array_size (array);                 \
       ++(i), (value) = svz_array_get ((array), (i)))

#endif /* not __ARRAY_H__ */
