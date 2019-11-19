/*
 * hash.c - hash table functions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

#include "timidity.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/hash.h"

#if DEBUG_MEMORY_LEAKS
# define svz_free(ptr) svz_free_func (ptr)
# define svz_malloc(size) svz_malloc_func (size)
# define svz_realloc(ptr, size) svz_realloc_func (ptr, size)
#endif /* DEBUG_MEMORY_LEAKS */

/* some useful defines */
#define HASH_SHRINK_LIMIT(hash) (hash->buckets >> 2)
#define HASH_EXPAND_LIMIT(hash) ((hash->buckets >> 1) + (hash->buckets >> 2))
#define HASH_BUCKET(code, hash) (code & (hash->buckets - 1))

/* useful defines */
#define SVZ_HASH_SHRINK   4
#define SVZ_HASH_EXPAND   8
#define SVZ_HASH_MIN_SIZE 4

/*
 * This is the basic structure of a hash entry consisting of its
 * key, the actual value stored in the hash table and the hash code
 * of the key.
 */
struct svz_hash_entry
{
  unsigned long code;
  char *key;
  void *value;
};

/*
 * The hash table consists of different hash buckets.  This contains the
 * bucket's size and the entry array.
 */
struct svz_hash_bucket
{
  int size;
  svz_hash_entry_t *entry;
};

/*
 * Calculate the hash code for a given string @var{key}.  This is the standard
 * callback for any newly created hash table.
 */
static unsigned long
hash_code (const char *key)
{
  unsigned long code = 0;
  const char *p = key;

  assert (key);
  while (*p)
    {
      code = (code << 1) ^ *p;
      p++;
    }
  return code;
}

/*
 * This is the default callback for any newly created hash for determining
 * two keys (@var{key1} and @var{key2}) being equal.  Return zero if both
 * strings are equal, otherwise non-zero.
 */
static int
hash_equal (const char *key1, const char *key2)
{
  const char *p1, *p2;

  assert (key1 && key2);

  if (key1 == key2)
    return 0;

  p1 = key1;
  p2 = key2;

  while (*p1 && *p2)
    {
      if (*p1 != *p2)
        return -1;
      p1++;
      p2++;
    }

  if (!*p1 && !*p2)
    return 0;
  return -1;
}

/*
 * This is the default routine for determining the actual hash table
 * key length of the given key @var{key}.
 */
static size_t
hash_key_length (const char *key)
{
  size_t len = 0;

  assert (key);
  while (*key++)
    len++;
  len++;

  return len;
}

#if ENABLE_HASH_ANALYSE
/*
 * This routine prints all the hash table @var{hash}.  It is for debugging
 * purposes only and should not go into distributions.
 */
static void
display_analysis (svz_hash_t *hash)
{
  svz_hash_bucket_t *bucket;
  int n, e, buckets, depth, entries;

  for (entries = 0, depth = 0, buckets = 0, n = 0; n < hash->buckets; n++)
    {
      bucket = &hash->table[n];
      if (bucket->size > 0)
        buckets++;
      for (e = 0; e < bucket->size; e++)
        {
          entries++;
#if 0
          fprintf (stdout, "bucket %04d: entry %02d: code: %08lu "
                   "value: %p key: %-20s\n",
                   n + 1, e + 1, bucket->entry[e].code,
                   bucket->entry[e].value, bucket->entry[e].key);
#endif /* 0 */
          if (e > depth)
            depth = e;
        }
    }
#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG,
           "%d/%d buckets (%d), %d entries (%d), depth: %d\n",
           buckets, hash->buckets, hash->fill,
           entries, hash->keys, depth + 1);
#endif /* ENABLE_DEBUG */
}
#endif  /* ENABLE_HASH_ANALYSE */

/**
 * Create a new hash table with an initial capacity @var{size}.  Return a
 * non-zero pointer to the newly created hash.  The size is calculated down
 * to a binary value.  The @var{destroy} callback specifies an
 * element destruction callback for use by @code{svz_hash_clear} and
 * @code{svz_hash_destroy} for each value.  If no such operation should be
 * performed the argument must be @code{NULL}.
 */
svz_hash_t *
svz_hash_create (size_t size, svz_free_func_t destroy)
{
  size_t n;
  svz_hash_t *hash;

  /* set initial hash table size to a binary value */
  for (n = size, size = 1; n != 1; n >>= 1)
    size <<= 1;
  if (size < SVZ_HASH_MIN_SIZE)
    size = SVZ_HASH_MIN_SIZE;

  /* allocate space for the hash itself */
  hash = svz_malloc (sizeof (svz_hash_t));
  hash->buckets = size;
  hash->fill = 0;
  hash->keys = 0;
  hash->code = hash_code;
  hash->equals = hash_equal;
  hash->keylen = hash_key_length;
  hash->destroy = destroy;

  /* allocate space for the hash table and initialize it */
  hash->table = svz_malloc (sizeof (svz_hash_bucket_t) * size);
  for (n = 0; n < size; n++)
    {
      hash->table[n].size = 0;
      hash->table[n].entry = NULL;
    }

  return hash;
}

/**
 * Set the internal @var{keylen}, @var{code} and and @var{equals}
 * functions for hash table @var{hash}.  Return @var{hash}.
 *
 * @var{keylen} takes @code{const char *data} and returns @code{size_t},
 * the number of bytes in @var{data} representing the key.
 *
 * @var{code} takes @code{const char *data}
 * and returns @code{unsigned long}.
 *
 * @var{equals} takes @code{const char *data1, const char *data2}
 * and returns @code{int}, which should be non-zero if equal.
 *
 * As a special case, a @code{NULL} value means don't set that function,
 * leaving it to its default value.
 */
svz_hash_t *
svz_hash_configure (svz_hash_t *hash,
                    size_t (* keylen) (const char *),
                    unsigned long (* code) (const char *),
                    int (* equals) (const char *, const char *))
{
  if (keylen) hash->keylen = keylen;
  if (code)   hash->code   = code;
  if (equals) hash->equals = equals;
  return hash;
}

/**
 * Destroy the existing hash table @var{hash}, @code{svz_free}ing
 * all keys within the hash, the hash table and the hash itself.
 * If a non-@code{NULL} element destruction callback was specified to
 * @code{svz_hash_create}, that function is called on each value.
 */
void
svz_hash_destroy (svz_hash_t *hash)
{
  size_t n;
  int e;
  svz_hash_bucket_t *bucket;

  if (hash == NULL)
    return;

  for (n = 0; n < hash->buckets; n++)
    {
      bucket = &hash->table[n];
      if (bucket->size)
        {
          for (e = 0; e < bucket->size; e++)
            {
              svz_free (bucket->entry[e].key);
              if (hash->destroy)
                hash->destroy (bucket->entry[e].value);
            }
          svz_free (bucket->entry);
        }
    }
  svz_free (hash->table);
  svz_free (hash);
}

/*
 * Rehash a given hash table @var{hash}.  Double (@var{type} is
 * @code{SVZ_HASH_EXPAND}) its size and expand the hash codes or half (@var{type}
 * is @code{SVZ_HASH_SHRINK}) its size and shrink the hash codes if these would
 * be placed somewhere else.
 */
static void
rehash (svz_hash_t *hash, int type)
{
  size_t n;
  int e;
  svz_hash_bucket_t *bucket, *next_bucket;

#if ENABLE_HASH_ANALYSE
  display_analysis (hash);
#endif

  if (type == SVZ_HASH_EXPAND)
    {
      /*
       * Reallocate and initialize the hash table itself.
       */
      hash->buckets <<= 1;
      hash->table = svz_realloc (hash->table,
                                 sizeof (svz_hash_bucket_t) * hash->buckets);
      for (n = hash->buckets >> 1; n < hash->buckets; n++)
        {
          hash->table[n].size = 0;
          hash->table[n].entry = NULL;
        }

      /*
       * Go through all hash table entries and check if it is necessary
       * to relocate them.
       */
      for (n = 0; n < (hash->buckets >> 1); n++)
        {
          bucket = &hash->table[n];
          for (e = 0; e < bucket->size; e++)
            {
              if (n != HASH_BUCKET (bucket->entry[e].code, hash))
                {
                  /* copy this entry to the far entry */
                  next_bucket =
                    &hash->table[HASH_BUCKET (bucket->entry[e].code, hash)];
                  next_bucket->entry = svz_realloc (next_bucket->entry,
                                                    (next_bucket->size + 1) *
                                                    sizeof (svz_hash_entry_t));
                  next_bucket->entry[next_bucket->size] = bucket->entry[e];
                  next_bucket->size++;
                  if (next_bucket->size == 1)
                    hash->fill++;

                  /* delete this entry */
                  bucket->size--;
                  if (bucket->size == 0)
                    {
                      svz_free (bucket->entry);
                      bucket->entry = NULL;
                      hash->fill--;
                    }
                  else
                    {
                      bucket->entry[e] = bucket->entry[bucket->size];
                      bucket->entry = svz_realloc (bucket->entry,
                                                   bucket->size *
                                                   sizeof (svz_hash_entry_t));
                    }
                  e--;
                }
            }
        }
    }
  else if (type == SVZ_HASH_SHRINK && hash->buckets > SVZ_HASH_MIN_SIZE)
    {
      hash->buckets >>= 1;
      for (n = hash->buckets; n < hash->buckets << 1; n++)
        {
          bucket = &hash->table[n];
          if (bucket->size)
            {
              for (e = 0; e < bucket->size; e++)
                {
                  next_bucket =
                    &hash->table[HASH_BUCKET (bucket->entry[e].code, hash)];
                  next_bucket->entry = svz_realloc (next_bucket->entry,
                                                    (next_bucket->size + 1) *
                                                    sizeof (svz_hash_entry_t));
                  next_bucket->entry[next_bucket->size] = bucket->entry[e];
                  next_bucket->size++;
                  if (next_bucket->size == 1)
                    hash->fill++;
                }
              svz_free (bucket->entry);
            }
          hash->fill--;
        }
      hash->table = svz_realloc (hash->table,
                                 sizeof (svz_hash_bucket_t) * hash->buckets);
    }

#if ENABLE_HASH_ANALYSE
  display_analysis (hash);
#endif
}

/**
 * Add a new element consisting of @var{key} and @var{value} to @var{hash}.
 * When @var{key} already exists, replace and return the old value.
 * @strong{Note}: This is sometimes the source of memory leaks.
 */
void *
svz_hash_put (svz_hash_t *hash, const char *key, void *value)
{
  unsigned long code = 0;
  int e;
  void *old;
  svz_hash_entry_t *entry;
  svz_hash_bucket_t *bucket;

  code = hash->code (key);

  /* Check if the key is already stored.  If so replace the value.  */
  bucket = &hash->table[HASH_BUCKET (code, hash)];
  for (e = 0; e < bucket->size; e++)
    {
      if (bucket->entry[e].code == code &&
          hash->equals (bucket->entry[e].key, key) == 0)
        {
          old = bucket->entry[e].value;
          bucket->entry[e].value = value;
          return old;
        }
    }

  /* Reallocate this bucket.  */
  bucket = &hash->table[HASH_BUCKET (code, hash)];
  bucket->entry = svz_realloc (bucket->entry,
                               sizeof (svz_hash_entry_t) * (bucket->size + 1));

  /* Fill this entry.  */
  entry = &bucket->entry[bucket->size];
  entry->key = svz_malloc (hash->keylen (key));
  memcpy (entry->key, key, hash->keylen (key));
  entry->value = value;
  entry->code = code;
  bucket->size++;
  hash->keys++;

  /* 75% filled?  */
  if (bucket->size == 1)
    {
      hash->fill++;
      if (hash->fill > HASH_EXPAND_LIMIT (hash))
        {
          rehash (hash, SVZ_HASH_EXPAND);
        }
    }
  return NULL;
}

/**
 * Delete an existing entry accessed via a @var{key} from the
 * hash table @var{hash}.  Return @code{NULL} if there is no
 * such key, otherwise the previous value.
 */
void *
svz_hash_delete (svz_hash_t *hash, const char *key)
{
  int n;
  unsigned long code;
  svz_hash_bucket_t *bucket;
  void *value;

  code = hash->code (key);
  bucket = &hash->table[HASH_BUCKET (code, hash)];

  for (n = 0; n < bucket->size; n++)
    {
      if (bucket->entry[n].code == code &&
          hash->equals (bucket->entry[n].key, key) == 0)
        {
          value = bucket->entry[n].value;
          bucket->size--;
          svz_free (bucket->entry[n].key);
          if (bucket->size)
            {
              bucket->entry[n] = bucket->entry[bucket->size];
              bucket->entry = svz_realloc (bucket->entry,
                                           sizeof (svz_hash_entry_t) *
                                           bucket->size);
            }
          else
            {
              svz_free (bucket->entry);
              bucket->entry = NULL;
              hash->fill--;
              if (hash->fill < HASH_SHRINK_LIMIT (hash))
                {
                  rehash (hash, SVZ_HASH_SHRINK);
                }
            }
          hash->keys--;
          return value;
        }
    }

  return NULL;
}

/**
 * Return the value associated with @var{key} in the hash table
 * @var{hash}, or @code{NULL} if there is no such key.
 */
void *
svz_hash_get (const svz_hash_t *hash, const char *key)
{
  int n;
  unsigned long code;
  svz_hash_bucket_t *bucket;

  code = hash->code (key);
  bucket = &hash->table[HASH_BUCKET (code, hash)];

  for (n = 0; n < bucket->size; n++)
    {
      if (bucket->entry[n].code == code &&
          hash->equals (bucket->entry[n].key, key) == 0)
        {
          return bucket->entry[n].value;
        }
    }

  return NULL;
}

/**
 * Return non-zero if @code{key} is stored within
 * the hash table @code{hash}, otherwise zero.
 * This function is useful when you cannot tell whether the return
 * value of @code{svz_hash_get} (@code{== NULL}) indicates a real
 * value in the hash or a non-existing hash key.
 */
int
svz_hash_exists (const svz_hash_t *hash, char *key)
{
  int n;
  unsigned long code;
  svz_hash_bucket_t *bucket;

  code = hash->code (key);
  bucket = &hash->table[HASH_BUCKET (code, hash)];

  for (n = 0; n < bucket->size; n++)
    if (bucket->entry[n].code == code &&
        hash->equals (bucket->entry[n].key, key) == 0)
      return -1;
  return 0;
}

/**
 * Iterate @var{func} over each key/value pair in @var{hash}.
 * @var{func} is called with three @code{void *} args: the key,
 * the value and the opaque (to @code{svz_hash_foreach}) @var{closure}.
 */
void
svz_hash_foreach (svz_hash_do_t *func, svz_hash_t *hash, void *closure)
{
  size_t i, n;
  int e;

  for (i = 0, n = 0;
       i < hash->keys && n < hash->buckets;
       n++)
    {
      svz_hash_bucket_t *bucket = &hash->table[n];

      for (e = 0;
           e < bucket->size;
           e++, i++)
        {
          svz_hash_entry_t *entry = &bucket->entry[e];

          func (entry->key, entry->value, closure);
        }
    }
}

/**
 * Return the number of keys in the hash table @var{hash}.
 * If @var{hash} is @code{NULL}, return zero.
 */
size_t
svz_hash_size (const svz_hash_t *hash)
{
  if (hash == NULL)
    return 0;
  return hash->keys;
}

/**
 * Return the key associated with @var{value} in the hash table
 * @var{hash}, or @code{NULL} if there is no such value.
 */
char *
svz_hash_contains (const svz_hash_t *hash, void *value)
{
  svz_hash_bucket_t *bucket;
  size_t n;
  int e;

  for (n = 0; n < hash->buckets; n++)
    {
      bucket = &hash->table[n];
      for (e = 0; e < bucket->size; e++)
        {
          if (bucket->entry[e].value == value)
            return bucket->entry[e].key;
        }
    }
  return NULL;
}
