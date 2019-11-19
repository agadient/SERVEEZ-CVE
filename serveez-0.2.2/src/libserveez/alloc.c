/*
 * alloc.c - memory allocation module implementation
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

#include "config.h"

#include "timidity.h"
#include "unused.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libserveez/alloc.h"
#include "libserveez/util.h"

#if DEBUG_MEMORY_LEAKS
# include "libserveez/hash.h"
# include "le-u32-hash.h"
#endif /* DEBUG_MEMORY_LEAKS */

#if ENABLE_DEBUG
/* The overall number of bytes allocated by libserveez.  */
static size_t allocated_bytes = 0;
/* The number of memory blocks reserved by libserveez.  */
static size_t allocated_blocks = 0;
#endif /* ENABLE_DEBUG */

/* Default memory management functions.  */
svz_malloc_func_t svz_malloc_func = malloc;
svz_realloc_func_t svz_realloc_func = realloc;
svz_free_func_t svz_free_func = free;

/**
 * Set the internal memory management functions to @var{cus_malloc},
 * @var{cus_realloc} and @var{cus_free}, respectively.
 * The default internal values are @code{malloc}, @code{realloc}
 * and @code{free}.
 */
void
svz_set_mm_funcs (svz_malloc_func_t cus_malloc,
                  svz_realloc_func_t cus_realloc,
                  svz_free_func_t cus_free)
{
  svz_malloc_func = cus_malloc;
  svz_realloc_func = cus_realloc;
  svz_free_func = cus_free;
}

#if DEBUG_MEMORY_LEAKS

/* heap hash table */
static svz_hash_t *heap = NULL;

/* return static heap hash code key length */
static size_t
heap_hash_keylen (UNUSED const char *id)
{
  return SIZEOF_VOID_P;
}

/* compare two heap hash values */
static int
heap_hash_equals (const char *id1, const char *id2)
{
  return memcmp (id1, id2, SIZEOF_VOID_P);
}

/* calculate heap hash code */
static unsigned long
heap_hash_code (const char *id)
{
  unsigned long code = le_u32_hash (id);
  code >>= 3;
  return code;
}

/* structure for heap management */
typedef struct
{
  void *ptr;       /* memory pointer */
  size_t size;     /* memory block's size */
  void *caller;    /* the caller */
}
heap_block_t;

/* add another heap block to the heap management */
static void
heap_add (heap_block_t *block)
{
  if (heap == NULL)
    heap = svz_hash_configure (svz_hash_create (4, NULL),
                               heap_hash_keylen,
                               heap_hash_code,
                               heap_hash_equals);
  svz_hash_put (heap, (char *) &block->ptr, block);
}

#ifdef _MSC_VER
# include <windows.h>
# include <imagehlp.h>
# define __builtin_return_address(no) ((void *) (stack.AddrReturn.Offset))
# define heap_caller()                                                     \
    STACKFRAME stack;                                                      \
    StackWalk (IMAGE_FILE_MACHINE_I386, GetCurrentProcess (),              \
               GetCurrentThread (), &stack, NULL, NULL, NULL, NULL, NULL)
#else
# ifndef __GNUC__
#  define __builtin_return_address(no) 0
# endif
# define heap_caller()
#endif

#else /* !DEBUG_MEMORY_LEAKS */
# define heap_caller()
#endif /* !DEBUG_MEMORY_LEAKS */

/* FIXME: Make ‘static inline’ func w/ attribute ‘SVZ_EXITING’.  */
#define oom(who)  do                                            \
    {                                                           \
      svz_log (SVZ_LOG_FATAL, who ": virtual memory exhausted\n");  \
      exit (EXIT_FAILURE);                                      \
    }                                                           \
  while (0)

/**
 * Allocate @var{size} bytes of memory and return a pointer to this block.
 */
void *
svz_malloc (size_t size)
{
  void *ptr;
#if ENABLE_DEBUG
  size_t *p;
#if DEBUG_MEMORY_LEAKS
  heap_block_t *block;
#endif /* DEBUG_MEMORY_LEAKS */
#endif /* ENABLE_DEBUG */

  heap_caller ();
  assert (size);

#if ENABLE_DEBUG
  if ((ptr = (void *) svz_malloc_func (size + 2 *
                                       sizeof (size_t))) != NULL)
    {
#if ENABLE_HEAP_COUNT
      /* save size at the beginning of the block */
      p = (size_t *) ptr;
      *p = size;
      p += 2;
      ptr = (void *) p;
#if DEBUG_MEMORY_LEAKS
      /* put heap pointer into special heap hash */
      block = svz_malloc_func (sizeof (heap_block_t));
      block->ptr = ptr;
      block->size = size;
      block->caller = __builtin_return_address (0);
      heap_add (block);
#endif /* DEBUG_MEMORY_LEAKS */
      allocated_bytes += size;
#endif /* ENABLE_HEAP_COUNT */
      allocated_blocks++;
      return ptr;
    }
#else /* not ENABLE_DEBUG */
  if ((ptr = (void *) svz_malloc_func (size)) != NULL)
    {
      return ptr;
    }
#endif /* not ENABLE_DEBUG */
  else
    oom ("malloc");
}

/**
 * Allocate @var{size} bytes of memory and return a pointer to this block.
 * The memory is cleared (filled with zeros).
 */
void *
svz_calloc (size_t size)
{
  void *ptr = svz_malloc (size);
  memset (ptr, 0, size);
  return ptr;
}

/**
 * Change the size of a block of memory at @var{ptr}, previously
 * returned by @code{svz_malloc}, to @var{size} bytes.  If @var{ptr}
 * is @code{NULL}, allocate a new block.
 */
void *
svz_realloc (void *ptr, size_t size)
{
#if ENABLE_DEBUG
  size_t old_size, *p;
#endif /* ENABLE_DEBUG */
#if DEBUG_MEMORY_LEAKS
  heap_block_t *block;
#endif /* DEBUG_MEMORY_LEAKS */

  heap_caller ();
  assert (size);

  if (ptr)
    {
#if ENABLE_DEBUG
#if ENABLE_HEAP_COUNT
#if DEBUG_MEMORY_LEAKS
      if ((block = svz_hash_delete (heap, (char *) &ptr)) == NULL ||
          block->ptr != ptr)
        {
          fprintf (stdout, "realloc: %p not found in heap (caller: %p)\n",
                   ptr, __builtin_return_address (0));
          assert (0);
        }
      svz_free_func (block);
#endif /* DEBUG_MEMORY_LEAKS */

      /* get previous blocksize */
      p = (size_t *) ptr;
      p -= 2;
      old_size = *p;
      ptr = (void *) p;
#endif /* ENABLE_HEAP_COUNT */

      if ((ptr = (void *) svz_realloc_func (ptr, size + 2 *
                                            sizeof (size_t))) != NULL)
        {
#if ENABLE_HEAP_COUNT
          /* save block size */
          p = (size_t *) ptr;
          *p = size;
          p += 2;
          ptr = (void *) p;

#if DEBUG_MEMORY_LEAKS
          block = svz_malloc_func (sizeof (heap_block_t));
          block->ptr = ptr;
          block->size = size;
          block->caller = __builtin_return_address (0);
          heap_add (block);
#endif /* DEBUG_MEMORY_LEAKS */

          allocated_bytes += size - old_size;
#endif /* ENABLE_HEAP_COUNT */

          return ptr;
        }
#else /* not ENABLE_DEBUG */
      if ((ptr = (void *) svz_realloc_func (ptr, size)) != NULL)
        {
          return ptr;
        }
#endif /* not ENABLE_DEBUG */
      else
        oom ("realloc");
    }
  else
    {
      ptr = svz_malloc (size);
      return ptr;
    }
}

/**
 * Free a block of memory at @var{ptr}, previously returned by
 * @code{svz_malloc} or @code{svz_realloc}.  If @var{ptr} is
 * @code{NULL}, do nothing.
 */
void
svz_free (void *ptr)
{
#if ENABLE_DEBUG
#if ENABLE_HEAP_COUNT
  size_t size, *p;
#if DEBUG_MEMORY_LEAKS
  heap_block_t *block;
#endif /* DEBUG_MEMORY_LEAKS */
#endif /* ENABLE_HEAP_COUNT */
#endif /* ENABLE_DEBUG */

  heap_caller ();

  if (ptr)
    {
#if ENABLE_DEBUG
#if ENABLE_HEAP_COUNT
#if DEBUG_MEMORY_LEAKS
      if ((block = svz_hash_delete (heap, (char *) &ptr)) == NULL ||
          block->ptr != ptr)
        {
          fprintf (stdout, "free: %p not found in heap (caller: %p)\n",
                   ptr, __builtin_return_address (0));
          assert (0);
        }
      svz_free_func (block);
#endif /* DEBUG_MEMORY_LEAKS */

      /* get blocksize */
      p = (size_t *) ptr;
      p -= 2;
      size = *p;
      ptr = (void *) p;
      assert (size);
      allocated_bytes -= size;
#endif /* ENABLE_HEAP_COUNT */

      allocated_blocks--;
#endif /* ENABLE_DEBUG */
      svz_free_func (ptr);
    }
}

#if DEBUG_MEMORY_LEAKS
static void
heap_internal (UNUSED void *k, void *v, UNUSED void *closure)
{
  heap_block_t *block = v;
  size_t *p = (size_t *) block->ptr;

  p -= 2;
  printf ("heap: caller = %p, ptr = %p, size = %zu\n",
          block->caller, block->ptr, block->size);
  svz_hexdump (stdout, "unreleased heap",
               (int) block->ptr, block->ptr, *p, 256);
  svz_free_func (block);
}

/*
 * Print a list of non-released memory blocks.  This is for debugging only
 * and should never occur in final software releases.  The function goes
 * through the heap hash and states each blocks address, size and caller.
 */
void
svz_heap (void)
{
  if (svz_hash_size (heap))
    {
      svz_hash_foreach (heap_internal, heap, NULL);
    }
  else
    {
      fprintf (stdout, "heap: no unreleased heap blocks\n");
    }
  svz_hash_destroy (heap);
  heap = NULL;
}
#endif /* DEBUG_MEMORY_LEAKS */

/**
 * Duplicate the given string @var{src} if it is not @code{NULL} and has
 * non-zero length.  Return the new string.
 */
char *
svz_strdup (const char *src)
{
  char *dst;
  int len;

  if (src == NULL || (len = strlen (src)) == 0)
    return NULL;

  dst = svz_malloc (len + 1);
  memcpy (dst, src, len + 1);
  return dst;
}

/*
 * Resize the memory block pointed to by @var{ptr} to @var{size} bytes.
 * This routine also allocates memory permanently.
 */
void *
svz_prealloc (void *ptr, size_t size)
{
  void *dst = svz_realloc_func (ptr, size);
  if (dst == NULL)
    oom ("realloc");
  return dst;
}

/*
 * Duplicate the given character string @var{src} permanently.
 */
char *
svz_pstrdup (const char *src)
{
  char *dst;
  size_t count;

  assert (src);
  count = 1 + strlen (src);
  if (! (dst = svz_malloc_func (count)))
    oom ("malloc");
  memcpy (dst, src, count);

  return dst;
}

/**
 * Write values to @code{to[0]} and @code{to[1]} representing the
 * number of currently allocated bytes and blocks, respectively.
 * If Serveez was not configured with @samp{--enable-debug},
 * the values are always 0.
 */
void
svz_get_curalloc (size_t *to)
{
#ifndef ENABLE_DEBUG
  to[0] = to[1] = 0;
#else
  to[0] = allocated_bytes;
  to[1] = allocated_blocks;
#endif
}
