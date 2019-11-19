/*
 * guile-bin.c - binary data exchange layer for Guile servers
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

#if ENABLE_GUILE_SERVER

#include <stdio.h>
#include <string.h>
#include <libguile.h>
#include "networking-headers.h"
#include "libserveez.h"
#include "misc-macros.h"
#include "gi.h"
#include "guile-api.h"
#include "guile-bin.h"
#include "unused.h"

/*
 * Structure definition of the data the binary smob refers to.
 */
typedef struct guile_bin
{
  uint8_t *data;       /* data pointer */
  int size;            /* size of the above data */
  int garbage;         /* if set the data pointer got allocated by
                          the smob functions */
}
guile_bin_t;

/* The smob tag.  */
static svz_smob_tag_t guile_bin_tag;

#define NEW_BIN(data)  gi_make_smob (guile_bin_tag, data)

/* Useful defines for accessing the binary smob.  */
#define CHECK_BIN_SMOB(binary) \
  gi_smob_tagged_p (binary, guile_bin_tag)
#define CHECK_BIN_SMOB_ARG(binary, arg, var)                       \
  if (!CHECK_BIN_SMOB (binary))                                    \
    scm_wrong_type_arg_msg (FUNC_NAME, arg, binary, BSMOB_WHAT);   \
  var = gi_smob_data (binary)
#define MAKE_BIN_SMOB()                                    \
  ((guile_bin_t *) ((void *)                               \
    gi_malloc (sizeof (guile_bin_t), BSMOB_WHAT)))

SCM_DEFINE
(guile_bin_p,
 "binary?", 1, 0, 0,
 (SCM obj),
 doc: /***********
Return @code{#t} if @var{obj} is an instance of the binary smob type.  */)
{
#define FUNC_NAME s_guile_bin_p
  return CHECK_BIN_SMOB (obj) ? SCM_BOOL_T : SCM_BOOL_F;
#undef FUNC_NAME
}

/* Smob print function: Displays a text representation of the given
   cell @var{binary} to the output port @var{port}.  */
static int
guile_bin_print (SCM binary, SCM port,
                 UNUSED scm_print_state *state)
{
  guile_bin_t *bin = gi_smob_data (binary);
  static char txt[256];

  sprintf (txt, "#<%s %p, size: %d>", BSMOB_WHAT, bin->data, bin->size);
  scm_puts (txt, port);
  return 1;
}

/* Smob free function: Releases any allocated resources used the given
   cell @var{binary}.  No need to mark any referring scheme cell.  Returns
   the number of bytes actually @code{free}'d.  */
static size_t
guile_bin_free (SCM binary)
{
  guile_bin_t *bin = gi_smob_data (binary);
  size_t size = sizeof (guile_bin_t);

  /* Free the data pointer if it has been allocated by ourselves and
     is not just a reference.  */
  if (bin->garbage)
    {
      size += bin->size;
      BFREE (DATA, bin->data, bin->size);
    }
  BFREE (SMOB, bin, sizeof (guile_bin_t));
  return gi_smob_free_rv (size);
}

/* Smob equal function: Return #t if the both cells @var{a} and @var{b}
   are definitely or virtually equal.  Otherwise return #f.  */
static SCM
guile_bin_equal (SCM a, SCM b)
{
  guile_bin_t *bin1 = gi_smob_data (a);
  guile_bin_t *bin2 = gi_smob_data (b);

  if (bin1 == bin2)
    return SCM_BOOL_T;
  else if (bin1->size == bin2->size)
    {
      if (bin1->data == bin2->data)
        return SCM_BOOL_T;
      else if (memcmp (bin1->data, bin2->data, bin1->size) == 0)
        return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

SCM_DEFINE
(guile_string_to_bin,
 "string->binary", 1, 0, 0,
 (SCM string),
 doc: /***********
Convert the given @var{string} into a binary smob.  The data pointer of
the binary smob is marked as garbage which must be @code{free}'d in the
sweep phase of the garbage collector.  */)
{
#define FUNC_NAME s_guile_string_to_bin
  guile_bin_t *bin;

  ASSERT_STRING (1, string);

  bin = MAKE_BIN_SMOB ();
  bin->size = (int) gi_string_length (string);
  if (bin->size > 0)
    {
      bin->data = gi_malloc (1 + bin->size, BDATA_WHAT);
      gi_get_xrep ((char *) bin->data, 1 + bin->size, string);
      bin->garbage = 1;
    }
  else
    {
      bin->data = NULL;
      bin->garbage = 0;
    }

  return NEW_BIN (bin);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_bin_to_string,
 "binary->string", 1, 0, 0,
 (SCM binary),
 doc: /***********
Convert the given binary smob @var{binary} into a string.
Return the string itself.  */)
{
#define FUNC_NAME s_guile_bin_to_string
  guile_bin_t *bin;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  return gi_nstring2scm (bin->size, (char *) bin->data);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_bin_search,
 "binary-search", 2, 0, 0,
 (SCM binary, SCM needle),
 doc: /***********
Search through the binary smob @var{binary} for @var{needle},
which can be an exact number, character,
string or another binary smob.  Return @code{#f} if the needle could
not be found, or a positive number indicating the position of the first
occurrence of @var{needle} in the binary smob @var{binary}.  */)
{
#define FUNC_NAME s_guile_bin_search
  SCM ret = SCM_BOOL_F;
  guile_bin_t *bin;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  SCM_ASSERT (gi_stringp (needle) || SCM_CHARP (needle) ||
              gi_exactp (needle) || CHECK_BIN_SMOB (needle),
              needle, SCM_ARG2, FUNC_NAME);

  /* Search for a pattern.  */
  if (gi_stringp (needle) || CHECK_BIN_SMOB (needle))
    {
      char buf[128];
      guile_bin_t *search = NULL;
      int len;
      uint8_t *p, *end, *start;

      if (CHECK_BIN_SMOB (needle))
        {
          search = gi_smob_data (needle);
          len = search->size;
          p = search->data;
        }
      else
        {
          len = GI_GET_XREP (buf, needle);
          p = (uint8_t *) buf;
        }
      start = bin->data;
      end = start + bin->size - len;

      /* Return #f if searching in empty data sets.  */
      if (len == 0 || p == NULL || start == NULL)
        return ret;

      /* Iterate the data.  */
      while (start <= end)
        {
          if (*start == *p && memcmp (start, p, len) == 0)
            {
              ret = gi_integer2scm (start - bin->data);
              break;
            }
          start++;
        }
    }

  /* Search for a single byte.  */
  else if (SCM_CHARP (needle) || gi_exactp (needle))
    {
      uint8_t c;
      uint8_t *p, *end;

      c = (uint8_t)
        (SCM_CHARP (needle) ? SCM_CHAR (needle) :
         (uint8_t) gi_scm2int (needle));
      p = bin->data;
      end = p + bin->size;

      while (p < end)
        {
          if (*p == c)
            {
              ret = gi_integer2scm (p - bin->data);
              break;
            }
          p++;
        }
    }
  return ret;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_bin_reverse_x,
 "binary-reverse!", 1, 0, 0,
 (SCM binary),
 doc: /***********
Perform an in-place reversal of the given binary smob @var{binary}
and return it.  */)
{
#define FUNC_NAME s_guile_bin_reverse_x
  guile_bin_t *bin;
  int first, last;
  uint8_t b;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);

  for (first = 0, last = bin->size - 1; first < last; first++, last--)
    {
      b = bin->data[first];
      bin->data[first] = bin->data[last];
      bin->data[last] = b;
    }
  return binary;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_bin_reverse,
 "binary-reverse", 1, 0, 0,
 (SCM binary),
 doc: /***********
Return a new binary smob with the reverse byte order of the given
binary smob @var{binary}.  */)
{
#define FUNC_NAME s_guile_bin_reverse
  guile_bin_t *bin, *reverse;
  int first, last;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  reverse = MAKE_BIN_SMOB ();
  reverse->size = bin->size;

  /* Return empty smob if necessary.  */
  if (reverse->size == 0)
    {
      reverse->garbage = 0;
      reverse->data = NULL;
      return NEW_BIN (reverse);
    }

  /* Reserve some memory for the new smob.  */
  reverse->data = gi_malloc (reverse->size, BDATA_WHAT);
  reverse->garbage = 1;

  /* Apply reverse byte order to the new smob.  */
  for (first = 0, last = reverse->size - 1; first < reverse->size; )
    reverse->data[first++] = bin->data[last--];

  return NEW_BIN (reverse);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_bin_set_x,
 "binary-set!", 3, 0, 0,
 (SCM binary, SCM index, SCM value),
 doc: /***********
Set the byte at position @var{index} of the binary smob @var{binary} to
the value given in @var{value} which can be either a character or an
exact number.  */)
{
#define FUNC_NAME s_guile_bin_set_x
  guile_bin_t *bin;
  int idx;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  ASSERT_EXACT (2, index);
  SCM_ASSERT_TYPE (gi_exactp (value) || SCM_CHARP (value),
                   value, SCM_ARG3, FUNC_NAME, "char or exact");

  /* Check the range of the index argument.  */
  idx = gi_scm2int (index);
  if (idx < 0 || idx >= bin->size)
    SCM_OUT_OF_RANGE (SCM_ARG2, index);

  bin->data[idx] = (uint8_t)
    (SCM_CHARP (value) ? SCM_CHAR (value) :
     (uint8_t) gi_scm2int (value));
  return SCM_UNSPECIFIED;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_bin_ref,
 "binary-ref", 2, 0, 0,
 (SCM binary, SCM index),
 doc: /***********
Obtain the byte at position @var{index} of the binary smob
@var{binary}.  */)
{
#define FUNC_NAME s_guile_bin_ref
  guile_bin_t *bin;
  int idx;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  ASSERT_EXACT (2, index);

  /* Check the range of the index argument.  */
  idx = gi_scm2int (index);
  if (idx < 0 || idx >= bin->size)
    SCM_OUT_OF_RANGE (SCM_ARG2, index);

  return SCM_MAKE_CHAR (bin->data[idx]);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_bin_length,
 "binary-length", 1, 0, 0,
 (SCM binary),
 doc: /***********
Return the size in bytes of the binary smob @var{binary}.  */)
{
#define FUNC_NAME s_guile_bin_length
  guile_bin_t *bin;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  return gi_integer2scm (bin->size);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_bin_concat_x,
 "binary-concat!", 2, 0, 0,
 (SCM binary, SCM append),
 doc: /***********
Append either the binary smob or string @var{append} onto the binary
smob @var{binary}.  If @var{binary} has been a simple data pointer
reference it is then a standalone binary smob as returned by
@code{string->binary}.  */)
{
#define FUNC_NAME s_guile_bin_concat_x
  char buf[8192];
  guile_bin_t *bin, *concat = NULL;
  int len, equal;
  uint8_t *p;

  /* Check arguments first.  */
  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  SCM_ASSERT (gi_stringp (append) || CHECK_BIN_SMOB (append),
              append, SCM_ARG2, FUNC_NAME);

  if (CHECK_BIN_SMOB (append))
    {
      concat = gi_smob_data (append);
      len = concat->size;
      p = concat->data;
    }
  else
    {
      len = GI_GET_XREP (buf, append);
      p = (uint8_t *) buf;
    }
  equal = (p == bin->data) ? 1 : 0;

  /* Return here if there is nothing to concatenate.  */
  if (len <= 0)
    return binary;

  if (bin->garbage)
    {
      bin->data = gi_realloc (bin->data, bin->size, bin->size + len,
                              BDATA_WHAT);
    }
  else
    {
      uint8_t *odata = bin->data;
      bin->data = gi_malloc (bin->size + len, BDATA_WHAT);
      memcpy (bin->data, odata, bin->size);
    }

  /* Reapply concatenation pointer if identical binaries have been passed.  */
  p = equal ? bin->data : p;

  memcpy (bin->data + bin->size, p, len);
  bin->size += len;
  bin->garbage = 1;
  return binary;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_bin_subset,
 "binary-subset", 2, 1, 0,
 (SCM binary, SCM start, SCM end),
 doc: /***********
Create a subset binary smob from the given binary smob @var{binary}.  The
range of this subset is specified by @var{start} and @var{end} both
inclusive (thus the resulting size is @code{@var{end} - @var{start} + 1}).
With a single exception: If @var{end} is not given or specified with -1,
return all data until the end of @var{binary}.  */)
{
#define FUNC_NAME s_guile_bin_subset
  guile_bin_t *bin, *ret;
  int from, to;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  ASSERT_EXACT (2, start);
  if (BOUNDP (end))
    ASSERT_EXACT (3, end);

  from = gi_scm2int (start);
  to = !BOUNDP (end) ? -1 : gi_scm2int (end);
  if (to == -1)
    to = bin->size - 1;

  /* Check the ranges of both indices.  */
  if (from < 0 || from >= bin->size)
    SCM_OUT_OF_RANGE (SCM_ARG2, start);
  if (to < 0 || to >= bin->size || to < from)
    SCM_OUT_OF_RANGE (SCM_ARG3, end);

  ret = MAKE_BIN_SMOB ();
  ret->size = to - from + 1;
  ret->data = bin->data + from;
  ret->garbage = 0;

  return NEW_BIN (ret);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_bin_to_list,
 "binary->list", 1, 0, 0,
 (SCM binary),
 doc: /***********
Convert the given binary smob @var{binary} into a scheme list.  The list
is empty if the size of @var{binary} is zero.  */)
{
#define FUNC_NAME s_guile_bin_to_list
  guile_bin_t *bin;
  uint8_t *p;
  SCM list;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  for (list = SCM_EOL, p = bin->data + bin->size; p-- > bin->data; )
    list = scm_cons (gi_nnint2scm (*p), list);
  return list;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_list_to_bin,
 "list->binary", 1, 0, 0,
 (SCM list),
 doc: /***********
Convert the scheme list @var{list} into a binary smob.  Each of the
elements of @var{list} is checked for validity.  The elements can be
either exact numbers in a byte's range or characters.  */)
{
#define FUNC_NAME s_guile_list_to_bin
  guile_bin_t *bin;
  uint8_t *p;
  int value;
  SCM val;

  SCM_ASSERT_TYPE (SCM_LISTP (list), list, SCM_ARG1, FUNC_NAME, "list");
  bin = MAKE_BIN_SMOB ();
  bin->size = gi_scm2ulong (scm_length (list));

  if (bin->size > 0)
    {
      p = bin->data = gi_malloc (bin->size, BDATA_WHAT);
      bin->garbage = 1;
    }
  else
    {
      bin->garbage = 0;
      bin->data = NULL;
      return NEW_BIN (bin);
    }

  /* Iterate over the list and build up binary smob.  */
  while (SCM_PAIRP (list))
    {
      val = SCM_CAR (list);
      if (!gi_exactp (val) && !SCM_CHARP (val))
        {
          BFREE (DATA, bin->data, bin->size);
          BFREE (SMOB, bin, sizeof (guile_bin_t));
          scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARGn, val, "char or exact");
        }
      value = SCM_CHARP (val) ?
        ((int) SCM_CHAR (val)) : gi_scm2int (val);
      if (value < -128 || value > 255)
        {
          BFREE (DATA, bin->data, bin->size);
          BFREE (SMOB, bin, sizeof (guile_bin_t));
          SCM_OUT_OF_RANGE (SCM_ARGn, val);
        }
      *p++ = (uint8_t) value;
      list = SCM_CDR (list);
    }

  return NEW_BIN (bin);
#undef FUNC_NAME
}

/* Checks if the given scheme cell @var{binary} is a binary smob or not.
   Returns zero if not, otherwise non-zero.  */
int
guile_bin_check (SCM binary)
{
  return CHECK_BIN_SMOB (binary) ? 1 : 0;
}

/* This routine converts any given data pointer @var{data} with a
   certain @var{size} into a binary smob.  This contains then a simple
   reference to the given data pointer.  */
SCM
guile_data_to_bin (void *data, int size)
{
  guile_bin_t *bin;

  bin = MAKE_BIN_SMOB ();
  bin->size = size;
  bin->data = data;
  bin->garbage = 0;
  return NEW_BIN (bin);
}

/* Converts the data pointer @var{data} with a size of @var{size} bytes
   into a binary smob which is marked as garbage.  This means the data
   pointer must be allocated by @code{gi_malloc} or @code{gi_realloc}.  */
SCM
guile_garbage_to_bin (void *data, int size)
{
  guile_bin_t *bin;

  bin = MAKE_BIN_SMOB ();
  bin->size = size;
  bin->data = data;
  bin->garbage = 1;
  return NEW_BIN (bin);
}

/* This function converts the given binary smob @var{binary} back to a
   data pointer.  If the @var{size} argument is not NULL the current size
   of the binary smob will be stored there.  */
void *
guile_bin_to_data (SCM binary, int *size)
{
  guile_bin_t *bin = gi_smob_data (binary);
  if (size)
    *size = bin->size;
  return bin->data;
}

#define BIN_REF_HEAD(ctype)                             \
SCM_DEFINE                                              \
 (guile_bin_ ## ctype ## _ref,                          \
  "binary-" #ctype "-ref", 2, 0, 0,                     \
  (SCM binary, SCM index),                              \
  "Return the @code{" #ctype "} value of the binary\n"  \
  "smob @var{binary} at the array index @var{index}.")

/* The following macro expands to a procedure body which accesses a
   binary smob's data for reading depending on the given @var{ctype}.  */
#define BIN_REF_BODY(ctype)                                             \
  guile_bin_t *bin;                                                     \
  int idx;                                                              \
  long val = 0;                                                         \
  void *data;                                                           \
                                                                        \
  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);                           \
  ASSERT_EXACT (2, index);                                              \
  idx = gi_scm2int (index);                                             \
  if (idx < 0 || idx >= (int) (bin->size / sizeof (ctype)))             \
    SCM_OUT_OF_RANGE (SCM_ARG2, index);                                 \
  data = SVZ_NUM2PTR (SVZ_PTR2NUM (bin->data) + idx * sizeof (ctype));  \
  memcpy (&val, data, sizeof (ctype));                                  \
  return gi_integer2scm (val)

/* Checks whether the scheme value @var{value} can be stored within a
   @var{ctype}. The macro stores valid values in @var{val} and throws an
   exception if it is out of range.  */
#define CTYPE_CHECK_RANGE(ctype, value, pos, val) do {                     \
    if (SCM_POSITIVEP (value)) {                                           \
      unsigned long uval = gi_scm2ulong (value);                           \
      unsigned ctype cval = (unsigned ctype) uval;                         \
      if (uval != (unsigned long) cval) SCM_OUT_OF_RANGE (pos, value);     \
      val = (unsigned long) uval;                                          \
    } else {                                                               \
      signed long ival = gi_scm2long (value);                              \
      signed ctype cval = (signed ctype) ival;                             \
      if (ival != (signed long) cval) SCM_OUT_OF_RANGE (pos, value);       \
      val = (unsigned long) ival;                                          \
    }                                                                      \
  } while (0)

#define BIN_SET_HEAD(ctype)                                             \
SCM_DEFINE                                                              \
 (guile_bin_ ## ctype ## _set,                                          \
  "binary-" #ctype "-set!", 3, 0, 0,                                    \
  (SCM binary, SCM index, SCM value),                                   \
  "Set the @code{" #ctype "} value of the binary smob @var{binary}\n"   \
  "at the array index @var{index} to the given value @var{value}.\n"    \
  "Return the previous (overridden) value.")

/* The following macro expands to a procedure definition which accesses a
   binary smob's data for writing depending on the given @var{ctype}.  */
#define BIN_SET_BODY(ctype)                                     \
  guile_bin_t *bin;                                             \
  int idx;                                                      \
  unsigned long val;                                            \
  SCM old;                                                      \
  unsigned long oldval = 0;                                     \
  void *data;                                                   \
                                                                \
  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);                   \
  ASSERT_EXACT (2, index);                                      \
  idx = gi_scm2int (index);                                     \
  if (idx < 0 || idx >= (int) (bin->size / sizeof (ctype)))     \
    SCM_OUT_OF_RANGE (SCM_ARG2, index);                         \
  ASSERT_EXACT (3, value);                                      \
  CTYPE_CHECK_RANGE (ctype, value, SCM_ARG3, val);              \
  data = SVZ_NUM2PTR                                            \
    (SVZ_PTR2NUM (bin->data) + idx * sizeof (ctype));           \
  memcpy (&oldval, data, sizeof (ctype));                       \
  old = gi_integer2scm (oldval);                                \
  memcpy (data, &val, sizeof (ctype));                          \
  return old

BIN_REF_HEAD (long)
{
#define FUNC_NAME s_guile_bin_long_ref
  BIN_REF_BODY (long);
#undef FUNC_NAME
}

BIN_SET_HEAD (long)
{
#define FUNC_NAME s_guile_bin_long_set
  BIN_SET_BODY (long);
#undef FUNC_NAME
}

BIN_REF_HEAD (int)
{
#define FUNC_NAME s_guile_bin_int_ref
  BIN_REF_BODY (int);
#undef FUNC_NAME
}

BIN_SET_HEAD (int)
{
#define FUNC_NAME s_guile_bin_int_ref
  BIN_SET_BODY (int);
#undef FUNC_NAME
}

BIN_REF_HEAD (short)
{
#define FUNC_NAME s_guile_bin_short_ref
  BIN_REF_BODY (short);
#undef FUNC_NAME
}

BIN_SET_HEAD (short)
{
#define FUNC_NAME s_guile_bin_short_set
  BIN_SET_BODY (short);
#undef FUNC_NAME
}

BIN_REF_HEAD (char)
{
#define FUNC_NAME s_guile_bin_char_ref
  BIN_REF_BODY (char);
#undef FUNC_NAME
}

BIN_SET_HEAD (char)
{
#define FUNC_NAME s_guile_bin_char_set
  BIN_SET_BODY (char);
#undef FUNC_NAME
}

/* Initialize the binary smob with all its features.  Call this function
   once at application startup and before running any scheme file
   evaluation.  */
void
guile_bin_init (void)
{
  guile_bin_tag = gi_make_tag (BSMOB_WHAT, sizeof (guile_bin_t),
                               guile_bin_free,
                               guile_bin_print,
                               guile_bin_equal);

#include "guile-bin.x"
}

#else /* not ENABLE_GUILE_SERVER */

static int have_guile_bin = 0;

#endif/* ENABLE_GUILE_SERVER */
