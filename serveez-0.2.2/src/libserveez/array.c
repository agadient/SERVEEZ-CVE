/*
 * array.c - array functions
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

#include "config.h"

#include "timidity.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/array.h"

struct svz_array
{
  size_t size;             /* Real size of the array.  */
  size_t capacity;         /* Current capacity.  */
  svz_free_func_t destroy; /* The destroy callback.  */
  void **data;             /* Data pointer.  */
};

/**
 * Create a new array with the initial capacity @var{capacity} and return
 * a pointer to it.  If @var{capacity} is zero it defaults to some value.
 * If @var{destroy} is non-@code{NULL}, @code{svz_array_destroy} calls
 * that function (typically used to free dynamically allocated memory).
 * For example, if the array contains data allocated by @code{svz_malloc},
 * @var{destroy} should be specified as @code{svz_free}.  If the array
 * contains data which should not be released, @var{destroy} should
 * be @code{NULL}.
 */
svz_array_t *
svz_array_create (size_t capacity, svz_free_func_t destroy)
{
  svz_array_t *array;

  if (!capacity)
    capacity = 4;
  array = svz_calloc (sizeof (svz_array_t));
  array->data = svz_malloc (sizeof (void *) * capacity);
  array->capacity = capacity;
  array->destroy = destroy;
  return array;
}

/**
 * Completely destroy the array @var{array}.  The @var{array} handle is
 * invalid afterwards.  The routine runs the @var{destroy} callback for each
 * element of the array.
 */
void
svz_array_destroy (svz_array_t *array)
{
  if (array)
    {
      if (array->data)
        {
          if (array->destroy != NULL)
            {
              size_t n;

              for (n = 0; n < array->size; n++)
                array->destroy (array->data[n]);
            }
          svz_free (array->data);
          array->data = NULL;
          array->size = 0;
          array->capacity = 0;
        }
      svz_free (array);
    }
}

/**
 * Return the array element at the position @var{index} of the array
 * @var{array} if the index is within the array range.  Return @code{NULL}
 * if not.
 */
void *
svz_array_get (svz_array_t *array, size_t index)
{
  if (array == NULL || index >= array->size)
    return NULL;
  return array->data[index];
}

/**
 * Replace the array element at the position @var{index} of the array
 * @var{array} with the value @var{value} and return the previous value
 * at this index.  Return @code{NULL} and do nothing
 * if @var{array} is @code{NULL} or the @var{index} is out of the array
 * range.
 */
void *
svz_array_set (svz_array_t *array, size_t index, void *value)
{
  void *prev;

  if (array == NULL || index >= array->size)
    return NULL;
  prev = array->data[index];
  array->data[index] = value;
  return prev;
}

/**
 * Append the value @var{value} at the end of the array @var{array}.
 * Do nothing if @var{array} is @code{NULL}.
 */
void
svz_array_add (svz_array_t *array, void *value)
{
  if (array)
    {
      size_t bigger = 1 + array->size;

      if (bigger > array->capacity)
        {
          array->capacity = array->capacity * 3 / 2 + 1;
          array->data = svz_realloc (array->data, sizeof (void *) *
                                     array->capacity);
        }
      array->data[array->size] = value;
      array->size = bigger;
    }
}

/**
 * Remove the array element at the position @var{index} of the array
 * @var{array}.  Return its previous value or @code{NULL} if the index
 * is out of the array's range.
 */
void *
svz_array_del (svz_array_t *array, size_t index)
{
  void *value;

  if (array == NULL || index >= array->size)
    return NULL;
  value = array->data[index];
  if (index != array->size - 1)
    memmove (&array->data[index], &array->data[index + 1],
             (array->size - index - 1) * sizeof (void *));
  array->size--;
  return value;
}

/**
 * Return the current size of @var{array}.
 */
size_t
svz_array_size (svz_array_t *array)
{
  if (array == NULL)
    return 0;
  return array->size;
}

/*
 * This function replicates the given array @var{array}.  It returns
 * @code{NULL} if there is nothing to do and an identical copy if the
 * array otherwise.
 */
svz_array_t *
svz_array_dup (svz_array_t *array)
{
  svz_array_t *dup;

  if (array == NULL)
    return NULL;
  dup = svz_array_create (array->size, array->destroy);
  dup->size = array->size;
  if (array->size)
    memcpy (dup->data, array->data, array->size * sizeof (void *));
  return dup;
}

/*
 * This function works something like @code{svz_array_dup} but considers
 * the values within the array @var{array} to be zero-terminated character
 * strings and duplicates these via @code{svz_strdup}.
 */
svz_array_t *
svz_array_strdup (svz_array_t *array)
{
  svz_array_t *dup;
  size_t n;

  if (array == NULL)
    return NULL;
  dup = svz_array_create (array->size, svz_free);
  dup->size = array->size;
  for (n = 0; n < array->size; n++)
    dup->data[n] = svz_strdup (array->data[n]);
  return dup;
}

/*
 * This function destroys the given array @var{array} if it holds no
 * elements and returns @code{NULL} in this case.  Otherwise the
 * function returns the given array.
 */
svz_array_t *
svz_array_destroy_zero (svz_array_t *array)
{
  if (array && array->size == 0)
    {
      svz_array_destroy (array);
      return NULL;
    }
  return array;
}
