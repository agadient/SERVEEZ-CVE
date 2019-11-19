/*
 * http-cache.c - http protocol file cache
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
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

#include "config.h"

#include "timidity.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_FLOSS_H
# include <floss.h>
#endif
#include "networking-headers.h"

#ifdef __MINGW32__
# include <io.h>
#endif

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
#endif

#include "libserveez.h"
#include "misc-macros.h"
#include "http-proto.h"
#include "http-core.h"
#include "http-cache.h"
#include "unused.h"

svz_hash_t *http_cache = NULL;               /* actual cache entry hash */
size_t http_cache_entries = 0;               /* amount of cache entries */
http_cache_entry_t *http_cache_first = NULL; /* most recent entry */
http_cache_entry_t *http_cache_last = NULL;  /* least recent entry */

/*
 * This will initialize the http cache entries.
 */
void
http_alloc_cache (size_t entries)
{
  if (entries > http_cache_entries || http_cache == NULL)
    {
      if (http_cache)
        http_free_cache ();
      http_cache = svz_hash_create (entries, NULL);
      http_cache_entries = entries;
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "cache: created %d cache entries\n", entries);
#endif
    }
}

/*
 * Free all the cache entries.
 */
void
http_free_cache (void)
{
  int total, files;
  http_cache_entry_t *cache, *next;

  files = total = 0;
  for (cache = http_cache_first; cache; cache = next)
    {
      next = cache->next;
      total += cache->size;
      files++;
      svz_free (cache->buffer);
      svz_free (cache->file);
      svz_free (cache);
    }

  svz_hash_destroy (http_cache);
  http_cache_first = http_cache_last = NULL;
  http_cache = NULL;
#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "cache: freeing %d byte in %d entries\n", total, files);
#endif
}

#if ENABLE_DEBUG
static void
cache_consistency_internal (UNUSED void *k, void *v,
                            UNUSED void *closure)
{
  http_cache_entry_t *ent = v;

  /* Each cache entry must have a file name.  */
  assert (ent->file);

  /* Cache entry must be completely unused if not ready.  */
  if (!ent->ready)
    assert (!ent->size
            && !ent->buffer
            && !ent->hits);
  /* Otherwise, a cache entry must contain something.  */
  else
    assert (ent->size >= 0
            && ent->buffer
            && ent->hits >= 0);
}

/*
 * Check consistency of the http cache.  Remove this function once the
 * server is stable.
 */
static void
http_cache_consistency (void)
{
  svz_hash_foreach (cache_consistency_internal, http_cache, NULL);
}
#else /* not ENABLE_DEBUG */
# define http_cache_consistency()
#endif /* not ENABLE_DEBUG */

#if ENABLE_CACHE_PRINT
static void
http_cache_print (void)
{
  int n;
  http_cache_entry_t *entry;

  printf ("cache first: %p\n", (void *) http_cache_first);
  for (n = 0, entry = http_cache_first; entry; entry = entry->next, n++)
    {
      printf ("cache entry: %p, prev: %p, next: %p\n",
              (void *) entry, (void *) entry->prev, (void *) entry->next);
    }
  printf ("cache last: %p\n", (void *) http_cache_last);
}
#endif  /* ENABLE_CACHE_PRINT */

/*
 * Returns the urgency value of the given http cache entry CACHE.
 */
int
http_cache_urgency (http_cache_entry_t *cache)
{
  int n;
  http_cache_entry_t *entry;

  for (n = 0, entry = http_cache_first; entry; entry = entry->next, n++)
    if (entry == cache)
      return n;
  return -1;
}

/*
 * This function will make the given cache entry CACHE the most recent
 * within the whole HTTP file cache.  All other used entries will be less
 * urgent afterwards.
 */
void
http_urgent_cache (http_cache_entry_t *cache)
{
  if (cache->prev)
    {
      cache->prev->next = cache->next;
      if (cache->next)
        cache->next->prev = cache->prev;
      else
        http_cache_last = cache->prev;
      http_cache_first->prev = cache;
      cache->next = http_cache_first;
      cache->prev = NULL;
      http_cache_first = cache;
    }
}

/*
 * This routine checks if a certain FILE is already within the HTTP file
 * cache.  It returns HTTP_CACHE_COMPLETE if it is already cached and fills
 * in the CACHE entry.  This entry will be additionally the most recent
 * afterwards.  If the given FILE is going to be in the cache then return
 * HTTP_CACHE_INCOMPLETE, return HTTP_CACHE_NO if it is not at all in the
 * cache.
 */
int
http_check_cache (char *file, http_cache_t *cache)
{
  http_cache_entry_t *cachefile;

  if ((cachefile = svz_hash_get (http_cache, file)) != NULL)
    {
      /* set this entry to the most recent, ready or not  */
      http_urgent_cache (cachefile);
      http_cache_consistency ();

      /* is this entry fully read by the cache reader?  */
      if (cachefile->ready)
        {
          /* fill in the cache entry for the cache writer */
          cache->entry = cachefile;
          cache->buffer = cachefile->buffer;
          cache->size = cachefile->size;
          return HTTP_CACHE_COMPLETE;
        }
      /* not but is going to be ...  */
      return HTTP_CACHE_INCOMPLETE;
    }
  return HTTP_CACHE_NO;
}

/*
 * Create a new http cache entry and initialize it.
 */
static http_cache_entry_t *
http_cache_create_entry (void)
{
  return svz_calloc (sizeof (http_cache_entry_t));
}

/*
 * Destroy an existing http cache entry and remove it from the cache hash.
 */
static void
http_cache_destroy_entry (http_cache_entry_t *cache)
{
  http_cache_consistency ();

  /* Delete cache entry from hash.  */
  if (svz_hash_delete (http_cache, cache->file) != cache)
    svz_log (SVZ_LOG_FATAL, "cache: inconsistent http hash\n");

  /* Update the double chained list of entries.  */
  if (cache->prev)
    cache->prev->next = cache->next;
  else
    http_cache_first = cache->next;
  if (cache->next)
    cache->next->prev = cache->prev;
  else
    http_cache_last = cache->prev;

  if (cache->ready)
    svz_free (cache->buffer);
  svz_free (cache->file);
  svz_free (cache);
}

/*
 * Reset the cache entry of a http sockets cache structure.
 */
static void
http_cache_reset (http_cache_t *cache)
{
  cache->size = 0;
  cache->buffer = NULL;
  cache->entry = NULL;
}

/*
 * This is a extended callback for the sock->disconnected_socket entry
 * of a socket structure.  You should assign it if the socket reads a
 * cache entry.
 */
int
http_cache_disconnect (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;

  if (http->cache && http->cache->entry)
    {
      /* if the cache entry has not been fully read then free it */
      if (!http->cache->entry->ready)
        {
          http_cache_destroy_entry (http->cache->entry);
          svz_free (http->cache->buffer);
          http_cache_reset (http->cache);
        }
    }
  return http_disconnect (sock);
}

/*
 * Find a free slot in the http file cache entries.  If necessary
 * delete the least recent.  Return zero if there was a free slot.
 */
int
http_init_cache (char *file, http_cache_t *cache)
{
  http_cache_entry_t *entry, *slot = NULL;

  /*
   * If there are still empty cache entries then create a
   * new cache entry.
   */
  if (svz_hash_size (http_cache) < http_cache_entries)
    {
      slot = http_cache_create_entry ();
    }

  /*
   * Otherwise find the least recent which is not currently in
   * use by the cache writer or reader.
   */
  else
    {
      for (entry = http_cache_last; entry; entry = entry->prev)
        if (!entry->usage && entry->ready)
          {
            slot = entry;
            break;
          }

      /* not a "reinitialable" cache entry found */
      if (!slot)
        {
          http_cache_reset (cache);
          http_cache_consistency ();
          return -1;
        }

      /* is currently used, so free the entry previously */
      http_cache_destroy_entry (slot);
      slot = http_cache_create_entry ();
    }

  svz_hash_put (http_cache, file, slot);
  slot->file = svz_strdup (file);
  if ((slot->next = http_cache_first) == NULL)
    http_cache_last = slot;
  else
    http_cache_first->prev = slot;
  http_cache_first = slot;

  /*
   * initialize the cache entry for the cache file reader: cachebuffer
   * is not allocated yet and current cache length is zero
   */
  http_cache_reset (cache);
  cache->entry = slot;

  http_cache_consistency ();
  return 0;
}

/*
 * Refresh a certain cache entry for reusing it afterwards.  So we do not
 * destroy the entry, but the actual cache content.
 */
void
http_refresh_cache (http_cache_t *cache)
{
  svz_free_and_zero (cache->entry->buffer);
  cache->entry->ready = 0;
  cache->entry->hits = 0;
  cache->entry->usage = 0;
  cache->size = 0;
  cache->buffer = NULL;
}

/*
 * Send a complete cache entry to a http connection.
 */
int
http_cache_write (svz_socket_t *sock)
{
  int num_written;
  int do_write;
  http_socket_t *http;
  http_cache_t *cache;

  /* get additional cache and http structures */
  http = sock->data;
  cache = http->cache;
  assert (cache->entry);

  do_write = cache->size;
  if (do_write > (SVZ_SOCK_MAX_WRITE << 5))
    do_write = (SVZ_SOCK_MAX_WRITE << 5);
  num_written = send (sock->sock_desc, cache->buffer, do_write, 0);

  if (num_written > 0)
    {
      sock->last_send = time (NULL);
      cache->buffer += num_written;
      cache->size -= num_written;
      http->length += num_written;
    }
  else if (num_written < 0)
    {
      svz_log_net_error ("cache: send");
      if (svz_wait_if_unavailable (sock, 1))
        num_written = 0;
    }

  /*
   * Check if the http cache has (success)fully been sent.
   * If yes then return non-zero in order to shutdown the
   * socket SOCK.
   */
  if (cache->size <= 0)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "cache: file successfully sent\n");
#endif
      num_written = http_keep_alive (sock);
    }

  /*
   * Return a non-zero value if an error occurred.
   */
  return (num_written < 0) ? -1 : 0;
}

/*
 * Do just the same as the ‘http_file_read’ but additionally copy
 * the data into the cache entry.
 */
int
http_cache_read (svz_socket_t *sock)
{
  int num_read;
  int do_read;
  http_socket_t *http;
  http_cache_t *cache;

  /* get additional cache and http structures */
  http = sock->data;
  cache = http->cache;

  do_read = sock->send_buffer_size - sock->send_buffer_fill;

  /*
   * This means the send buffer is currently full, we have to
   * wait until some data has been send via the socket.
   */
  if (do_read <= 0)
    {
      return 0;
    }

  /*
   * Try to read as much data as possible from the file.
   */
#ifndef __MINGW32__
  num_read = read (sock->file_desc,
                   sock->send_buffer + sock->send_buffer_fill, do_read);
#else
  if (!ReadFile ((svz_t_handle) sock->file_desc,
                 sock->send_buffer + sock->send_buffer_fill,
                 do_read, (DWORD *) &num_read, NULL))
    {
      num_read = -1;
    }
#endif

  /* Read error occurred.  */
  if (num_read < 0)
    {
#ifndef __MINGW32__
      svz_log_sys_error ("cache: read");
#else
      svz_log_sys_error ("cache: ReadFile");
#endif

      /* release the actual cache entry previously reserved */
      if (cache->size > 0)
        {
          svz_free (cache->buffer);
          http_cache_reset (cache);
        }
      return -1;
    }

  /* Data has been read.  */
  else if (num_read > 0)
    {
      /*
       * Reserve some more memory and then copy the gained data
       * to the cache entry.
       */
      cache->buffer = svz_realloc (cache->buffer, cache->size + num_read);
      memcpy (cache->buffer + cache->size,
              sock->send_buffer + sock->send_buffer_fill, num_read);
      cache->size += num_read;

      sock->send_buffer_fill += num_read;
      http->filelength -= num_read;
      http->length += num_read;
    }

  /* Bogus file.  File size from ‘stat’ was not true.  */
  if (num_read == 0 && http->filelength != 0)
    {
      cache->entry->size = cache->size;
      cache->entry->buffer = cache->buffer;
      cache->entry->ready = 42;
      http_cache_reset (cache);
      return -1;
    }

  /* EOF reached and set the appropriate flags */
  if (http->filelength <= 0)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "cache: `%s' successfully read\n",
               cache->entry->file);
#endif

      /* fill in the actual cache entry */
      cache->entry->size = cache->size;
      cache->entry->buffer = cache->buffer;
      cache->entry->ready = 42;
      http_cache_reset (cache);

      /* set flags and reassign default reader */
      sock->read_socket = svz_tcp_read_socket;
      sock->userflags |= HTTP_FLAG_DONE;
      sock->flags &= ~SVZ_SOFLG_FILE;
    }

  return 0;
}
