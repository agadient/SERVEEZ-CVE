/*
 * alloc.h - memory allocation module declarations
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __XALLOC_H__
#define __XALLOC_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
/* end svzint */

__BEGIN_DECLS

/* Function type definitions.  */
typedef void * (* svz_malloc_func_t) (size_t);
typedef void * (* svz_realloc_func_t) (void *, size_t);
typedef void (* svz_free_func_t) (void *);

/* Global allocator functions.  */
SERVEEZ_API void svz_set_mm_funcs (svz_malloc_func_t,
                                   svz_realloc_func_t,
                                   svz_free_func_t);
SBO svz_malloc_func_t svz_malloc_func;
SBO svz_realloc_func_t svz_realloc_func;
SBO svz_free_func_t svz_free_func;

/* Internal allocator functions.  */
SERVEEZ_API void *svz_malloc (size_t);
SERVEEZ_API void *svz_calloc (size_t);
SERVEEZ_API void *svz_realloc (void *, size_t);
SERVEEZ_API void svz_free (void *);
SERVEEZ_API char *svz_strdup (const char *);

/* begin svzint */
/* Internal permanent allocator functions.  */
SBO void *svz_prealloc (void *, size_t);
SBO char *svz_pstrdup (const char *);

#if DEBUG_MEMORY_LEAKS
SERVEEZ_API void svz_heap (void);
#endif /* DEBUG_MEMORY_LEAKS */
/* end svzint */

SERVEEZ_API void svz_get_curalloc (size_t *);

__END_DECLS

#endif /* not __XALLOC_H__ */
