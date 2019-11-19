/* gi.c --- buffer against libguile interface churn
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
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
#include <string.h>
#include <libguile.h>

#define COMBINED(maj,min)  ((100 * (maj)) + (min))

#if defined SCM_MAJOR_VERSION && defined SCM_MINOR_VERSION
#define GUILE_COMBINED_V  COMBINED (SCM_MAJOR_VERSION, SCM_MINOR_VERSION)
#else
#define GUILE_COMBINED_V  COMBINED (1, 4)
#endif

#define GUILE_V_LT(maj,min)  (GUILE_COMBINED_V < COMBINED (maj, min))
#define GUILE_V_GE(maj,min)  (! GUILE_V_LT (maj, min))

/* If the system Guile is 1.8 but we are compiling for 2.0, for example,
   avoid pulling in the system Guile gh.h.  */
#if defined HAVE_GUILE_GH_H && GUILE_V_LT (1, 9)
# include <guile/gh.h>
#endif

#include "timidity.h"
#include "unused.h"

#define V19  GUILE_V_GE (1, 9)
#define V17  GUILE_V_GE (1, 7)
#define V15  GUILE_V_GE (1, 5)

void *
gi_malloc (size_t len, const char *name)
{
#if V17
  return scm_gc_malloc (len, name);
#else
  return scm_must_malloc (len, name);
#endif
}

void *
gi_realloc (void *mem, size_t olen, size_t nlen, const char *name)
{
#if V17
  return scm_gc_realloc (mem, olen, nlen, name);
#else
  return scm_must_realloc (mem, olen, nlen, name);
#endif
}

#if V17
#define POSSIBLY_UNUSED_GI_FREE_PARAM
#else
#define POSSIBLY_UNUSED_GI_FREE_PARAM  UNUSED
#endif

void
gi_free (void *mem,
         POSSIBLY_UNUSED_GI_FREE_PARAM size_t len,
         POSSIBLY_UNUSED_GI_FREE_PARAM const char *name)
{
#if V17
  scm_gc_free (mem, len, name);
#else
  scm_must_free (mem);
#endif
}

#if V17 /* FIXME: Is this the correct demarcation?  */
#define POSSIBLY_UNUSED_GI_SMOB_FREE_RV_PARAM  UNUSED
#else
#define POSSIBLY_UNUSED_GI_SMOB_FREE_RV_PARAM
#endif

size_t
gi_smob_free_rv (POSSIBLY_UNUSED_GI_SMOB_FREE_RV_PARAM size_t len)
{
#if V17 /* FIXME: Is this the correct demarcation?  */
  return 0;
#else
  return len;
#endif
}

#if V15
#define gc_protect    scm_gc_protect_object
#define gc_unprotect  scm_gc_unprotect_object
#else
#define gc_protect    scm_protect_object
#define gc_unprotect  scm_unprotect_object
#endif

SCM
gi_gc_protect (SCM obj)
{
  return gc_protect (obj);
}

SCM
gi_gc_unprotect (SCM obj)
{
  return gc_unprotect (obj);
}

#if V19
#define mem2scm(len,ptr)  scm_from_locale_stringn (ptr, len)
#define mem02scm(ptr)     scm_from_locale_string (ptr)
#elif V17
#define mem2scm(len,ptr)  scm_mem2string (ptr, len)
#define mem02scm(ptr)     scm_makfrom0str (ptr)
#elif V15
#define mem2scm(len,ptr)  scm_makfromstr (ptr, len)
#define mem02scm(ptr)     scm_makfrom0str (ptr)
#else
#define mem2scm(len,ptr)  gh_str2scm (ptr, len)
#define mem02scm(ptr)     gh_str02scm (ptr)
#endif

SCM
gi_nstring2scm (size_t len, char const *s)
{
  return mem2scm (len, s);
}

SCM
gi_string2scm (char const * s)
{
  return mem02scm (s);
}

#if V19
#define symbol2scm  scm_from_locale_symbol
#elif V15
#define symbol2scm  scm_str2symbol
#else
#define symbol2scm  gh_symbol2scm
#endif

SCM
gi_symbol2scm (char const * name)
{
  return symbol2scm (name);
}

#if V19
#define integer2scm  scm_from_signed_integer
#define nnint2scm    scm_from_unsigned_integer
#elif V15
#define integer2scm  scm_long2num
#define nnint2scm    scm_ulong2num
#else
#define integer2scm  gh_long2scm
#define nnint2scm    gh_ulong2scm
#endif

SCM
gi_integer2scm (long int n)
{
  return integer2scm (n);
}

SCM
gi_nnint2scm (unsigned long int n)
{
  return nnint2scm (n);
}

#if V15
#define list_3(a1,a2,a3)  scm_list_n (a1, a2, a3, SCM_UNDEFINED)
#else
#define list_3(a1,a2,a3)  scm_listify (a1, a2, a3, SCM_UNDEFINED)
#endif

SCM
gi_list_3 (SCM a1, SCM a2, SCM a3)
{
  return list_3 (a1, a2, a3);
}

#if V15
#define list_5(a1,a2,a3,a4,a5)  scm_list_n (a1, a2, a3, a4, a5, SCM_UNDEFINED)
#else
#define list_5(a1,a2,a3,a4,a5)  scm_listify (a1, a2, a3, a4, a5, SCM_UNDEFINED)
#endif

SCM
gi_list_5 (SCM a1, SCM a2, SCM a3, SCM a4, SCM a5)
{
  return list_5 (a1, a2, a3, a4, a5);
}

extern SCM
gi_n_vector (size_t len, SCM fill)
{
#if V15
  return scm_c_make_vector (len, fill);
#else
  return scm_make_vector (integer2scm (len), fill);
#endif
}

#if V15
#define eval_string  scm_c_eval_string
#else
#define eval_string  gh_eval_str
#endif

SCM
gi_eval_string (char const *string)
{
  return eval_string (string);
}

SCM
gi_lookup (char const *string)
{
  SCM rv;

#if V19
  rv = scm_c_private_lookup ("guile-user", string);
  rv = scm_variable_ref (rv);
#elif V15
  rv = scm_sym2var (symbol2scm (string),
                    scm_current_module_lookup_closure (),
                    SCM_BOOL_F);
  rv = SCM_FALSEP (rv)
    ? SCM_UNDEFINED
    : scm_variable_ref (rv);
#else
  rv = gh_lookup (string);
#endif

  return rv;
}

int
gi_scm2int (SCM number)
{
#if V15
  return scm_to_int (number);
#else
  return gh_scm2int (number);
#endif
}

long
gi_scm2long (SCM number)
{
#if V15
  return scm_to_long (number);
#else
  return gh_scm2long (number);
#endif
}

unsigned long
gi_scm2ulong (SCM number)
{
#if V15
  return scm_to_ulong (number);
#else
  return gh_scm2ulong (number);
#endif
}

size_t
gi_string_length (SCM string)
{
  return gi_scm2ulong (scm_string_length (string));
}

int
gi_nfalsep (SCM obj)
{
#if V17
  return ! scm_is_false (obj);
#else
  return SCM_NFALSEP (obj);
#endif
}

int
gi_stringp (SCM obj)
{
#if V17
  return scm_is_string (obj);
#else
  return gi_nfalsep (scm_string_p (obj));
#endif
}

int
gi_symbolp (SCM obj)
{
#if V17
  return scm_is_symbol (obj);
#else
  return gi_nfalsep (scm_symbol_p (obj));
#endif
}

int
gi_exactp (SCM obj)
{
#ifdef SCM_EXACTP
  return SCM_EXACTP (obj);
#else
  return gi_nfalsep (scm_number_p (obj))
    && gi_nfalsep (scm_exact_p (obj));
#endif
}

int
gi_get_xrep (char *buf, size_t len, SCM symbol_or_string)
{
  SCM obj;
  size_t actual;

  /* You're joking, right?  */
  if (! len)
    return -1;

  obj = gi_symbolp (symbol_or_string)
    ? scm_symbol_to_string (symbol_or_string)
    : symbol_or_string;
  actual = gi_string_length (obj);
  if (len < actual + 1)
    return -1;

#if V17
  {
    size_t sanity;

    sanity = scm_to_locale_stringbuf (obj, buf, len);
    assert (sanity == actual);
  }
#else
  {
    int sanity;
    char *stage = gh_scm2newstr (obj, &sanity);

    assert (sanity == (int) actual);
    memcpy (buf, stage, actual);
    scm_must_free (stage);
  }
#endif

  /* Murphy was an optimist.  */
  buf[actual] = '\0';
  return actual;
}

void
gi_define (const char *name, SCM value)
{
#if V15
  scm_c_define (name, value);
#else
  gh_define (name, value);
#endif
}

SCM
gi_primitive_eval (SCM form)
{
#if V15
  return scm_primitive_eval_x (form);
#else
  return scm_eval_x (form);
#endif
}

SCM
gi_primitive_load (const char *filename)
{
#if V15
  return scm_c_primitive_load (filename);
#else
  return scm_primitive_load (gi_string2scm (filename));
#endif
}

/* Depending on the smob implementation of Guile we use different
   functions in order to create a new smob tag.  It is also necessary to
   apply a smob `free' function for older Guile versions because it is
   called unconditionally and has no reasonable default function.  */

svz_smob_tag_t
gi_make_tag (const char *description, size_t sz,
             const void *fn_free,
             const void *fn_print,
             const void *fn_equal)
{
  svz_smob_tag_t tag;

#ifdef SCM_SMOB_DATA

  tag = scm_make_smob_type (description, sz);
  scm_set_smob_free (tag, fn_free ? fn_free : scm_free0);
  scm_set_smob_print (tag, fn_print);
  if (fn_equal)
    scm_set_smob_equalp (tag, fn_equal);

#else  /* !defined SCM_SMOB_DATA */

  scm_smobfuns mfpe = {
    NULL,                               /* mark */
    fn_free ? fn_free : scm_free0,
    fn_print,
    fn_equal
  };

  tag = scm_newsmob (&mfpe);

#endif  /* !defined SCM_SMOB_DATA */

  return tag;
}

int
gi_smob_tagged_p (SCM obj, svz_smob_tag_t tag)
{
  return SCM_NIMP (obj)
    && tag == SCM_TYP16 (obj);
}

#ifndef SCM_NEWSMOB
#define SCM_NEWSMOB(value, tag, data)  do       \
    {                                           \
      SCM_NEWCELL (value);                      \
      SCM_SETCDR (value, data);                 \
      SCM_SETCAR (value, tag);                  \
    }                                           \
  while (0)
#endif
#ifndef SCM_RETURN_NEWSMOB
#define SCM_RETURN_NEWSMOB(tag, data)  do       \
    {                                           \
      SCM value;                                \
      SCM_NEWSMOB (value, tag, data);           \
      return value;                             \
    }                                           \
  while (0)
#endif

SCM
gi_make_smob (svz_smob_tag_t tag, void *data)
{
  SCM_RETURN_NEWSMOB (tag, data);
}

void *
gi_smob_data (SCM smob)
{
#ifndef SCM_SMOB_DATA
  return (void *) SCM_CDR (smob);
#else
  return (void *) SCM_SMOB_DATA (smob);
#endif
}

SCM
gi_hash_clear_x (SCM table)
{
#if V17
  return scm_hash_clear_x (table);
#else
  scm_vector_fill_x (table, SCM_EOL);
#endif
}

/* gi.c ends here */
