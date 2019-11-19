/*
 * hash.h - hash function interface
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __HASH_H__
#define __HASH_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
/* end svzint */

typedef struct svz_hash_entry svz_hash_entry_t;
typedef struct svz_hash_bucket svz_hash_bucket_t;
typedef struct svz_hash svz_hash_t;
/* begin svzint */
/*
 * This structure keeps information of a specific hash table.
 * It's here (rather than in .c) for the benefit of cfg.c ‘hash_dup’.
 */
struct svz_hash
{
  size_t buckets;                  /* number of buckets in the table */
  size_t fill;                     /* number of filled buckets */
  size_t keys;                     /* number of stored keys */
  int (* equals) (const char *, const char *); /* key string equality callback */
  unsigned long (* code) (const char *); /* hash code calculation callback */
  size_t (* keylen) (const char *);      /* how to get the hash key length */
  svz_free_func_t destroy;         /* element destruction callback */
  svz_hash_bucket_t *table;        /* hash table */
};
/* end svzint */

typedef void (svz_hash_do_t) (void *, void *, void *);

__BEGIN_DECLS

/*
 * Basic hash table functions.
 */
SERVEEZ_API svz_hash_t *svz_hash_create (size_t, svz_free_func_t);
SERVEEZ_API svz_hash_t *
svz_hash_configure (svz_hash_t *hash,
                    size_t (* keylen) (const char *),
                    unsigned long (* code) (const char *),
                    int (* equals) (const char *, const char *));
SERVEEZ_API void svz_hash_destroy (svz_hash_t *);
SERVEEZ_API void *svz_hash_delete (svz_hash_t *, const char *);
SERVEEZ_API void *svz_hash_put (svz_hash_t *, const char *, void *);
SERVEEZ_API void *svz_hash_get (const svz_hash_t *, const char *);
SERVEEZ_API void svz_hash_foreach (svz_hash_do_t *, svz_hash_t *, void *);
SERVEEZ_API size_t svz_hash_size (const svz_hash_t *);
SERVEEZ_API char *svz_hash_contains (const svz_hash_t *, void *);
SERVEEZ_API int svz_hash_exists (const svz_hash_t *, char *);

__END_DECLS

#endif /* not __HASH_H__ */
