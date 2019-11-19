/*
 * http-cache.h - http protocol cache header file
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

#ifndef __HTTP_CACHE_H__
#define __HTTP_CACHE_H__ 1

/*
 * Some #defines.  These are just default values for configurable
 * variables.
 */
#define MAX_CACHE          64       /* cache file entries */
#define MAX_CACHE_SIZE     1024*200 /* maximum cache file size */

/*
 * This structure contains the info for a cached file.
 */
typedef struct http_cache_entry http_cache_entry_t;
struct http_cache_entry
{
  http_cache_entry_t *next; /* next in list */
  http_cache_entry_t *prev; /* previous in list */
  char *buffer;             /* pointer to cache buffer */
  int size;                 /* cache buffer size (size of file) */
  char *file;               /* actual filename */
  time_t date;              /* date of last modification */
  int usage;                /* how often this is currently used */
  int hits;                 /* cache hits */
  int ready;                /* this flag indicates if the entry is ok */
};

/*
 * The http_cache_t type is a structure containing the info
 * a cache writer and reader needs to know.
 */
typedef struct
{
  char *buffer;              /* pointer to cache buffer */
  int size;                  /* bytes left within this buffer */
  http_cache_entry_t *entry; /* pointer to original cache file entry */
}
http_cache_t;

/*
 * http cache structures.
 */
extern svz_hash_t *http_cache;
extern size_t http_cache_entries;
extern http_cache_entry_t *http_cache_first;
extern http_cache_entry_t *http_cache_last;

/*
 * Basic http cache functions.
 */
void http_alloc_cache (size_t entries);
void http_free_cache (void);
void http_refresh_cache (http_cache_t *cache);
int http_cache_urgency (http_cache_entry_t *cache);
int http_init_cache (char *file, http_cache_t *cache);
int http_check_cache (char *file, http_cache_t *cache);
int http_cache_write (svz_socket_t *sock);
int http_cache_read (svz_socket_t *sock);
int http_cache_disconnect (svz_socket_t *sock);

/*
 * Return values for ‘http_check_cache’.
 */
#define HTTP_CACHE_COMPLETE   0 /* file is in the cache */
#define HTTP_CACHE_INCOMPLETE 1 /* file is going to be in the cache */
#define HTTP_CACHE_NO         2 /* file is not in the cache */
#define HTTP_CACHE_INHIBIT    3 /* inhibit file caching */

#endif /* __HTTP_CACHE_H__ */
