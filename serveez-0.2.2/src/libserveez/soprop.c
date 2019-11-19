/*
 * soprop.c - socket property
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

#include "config.h"
#include <stddef.h>                     /* offsetof(3) */
#include <string.h>                     /* memcmp(3) */
#include "libserveez/alloc.h"
#include "libserveez/hash.h"
#include "libserveez/socket.h"

/*
 * soprop (socket property)
 *
 * A hash table where the key is ‘svz_socket_t *’ and only non-‘NULL’
 * values are saved (specifying a ‘NULL’ value reclaims storage).
 *
 * Actually, to conform to the hash table interface, the "effective key"
 * is the two ‘svz_socket_t’ members ‘id’ and ‘version’, taken together.
 * We assume they are contiguous.
 */

#define SOPROP_KFIELD_OFFS  (offsetof (svz_socket_t, id))
#define SOPROP_KFIELD_SIZE  (sizeof (sock->id) + sizeof (sock->version))

static size_t
soprop_keylen (const char *key)
{
  const svz_socket_t *sock = (const svz_socket_t *) key;

  return SOPROP_KFIELD_OFFS + SOPROP_KFIELD_SIZE;
}

static unsigned long
soprop_code (const char *key)
{
  const svz_socket_t *sock = (const svz_socket_t *) key;

  return sock->id;
}

static int
soprop_equals (const char *a, const char *b)
{
  svz_socket_t *sock;                   /* for ‘SOPROP_KFIELD_SIZE’ (ugh) */

  return memcmp (SOPROP_KFIELD_OFFS + a,
                 SOPROP_KFIELD_OFFS + b,
                 SOPROP_KFIELD_SIZE);
}

svz_hash_t *
svz_soprop_create (size_t size, svz_free_func_t destroy)
{
  svz_hash_t *ht;

  ht = svz_hash_create (size, destroy);
  svz_hash_configure (ht, soprop_keylen, soprop_code, soprop_equals);
  return ht;
}

void *
svz_soprop_put (svz_hash_t *ht, const svz_socket_t *sock, void *value)
{
  const char *key = (const char *) sock;

  return value
    ? svz_hash_put (ht, key, value)
    : svz_hash_delete (ht, key);
}

void *
svz_soprop_get (svz_hash_t *ht, const svz_socket_t *sock)
{
  return svz_hash_get (ht, (const char *) sock);
}

void
svz_soprop_destroy (svz_hash_t *ht)
{
  svz_hash_destroy (ht);
}
