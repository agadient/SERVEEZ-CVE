/* gi.h --- buffer against libguile interface churn
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

#ifndef __GI_H__
#define __GI_H__ 1

#include "gi-sup/level.h"
#include "gi-sup/mkhash.h"

extern void *gi_malloc (size_t len, const char *name);
extern void *gi_realloc (void *mem, size_t olen, size_t nlen,
                         const char *name);
extern void gi_free (void *mem, size_t len, const char *name);
extern size_t gi_smob_free_rv (size_t len);

extern SCM gi_gc_protect (SCM object);
extern SCM gi_gc_unprotect (SCM object);

extern SCM gi_nstring2scm (size_t len, char const *s);
extern SCM gi_string2scm (char const * s);
extern SCM gi_symbol2scm (char const * name);
extern SCM gi_integer2scm (long int);
extern SCM gi_nnint2scm (unsigned long int n);

extern SCM gi_list_3 (SCM a1, SCM a2, SCM a3);
extern SCM gi_list_5 (SCM a1, SCM a2, SCM a3, SCM a4, SCM a5);

extern SCM gi_n_vector (size_t len, SCM fill);

extern SCM gi_eval_string (char const *);

extern SCM gi_lookup (char const *);

extern int gi_scm2int (SCM number);
extern long gi_scm2long (SCM number);
extern unsigned long gi_scm2ulong (SCM number);

size_t gi_string_length (SCM string);

extern int gi_nfalsep (SCM obj);
extern int gi_stringp (SCM obj);
extern int gi_symbolp (SCM obj);
extern int gi_exactp (SCM obj);

#define STRING_OR_SYMBOL_P(obj)                 \
  (gi_stringp (obj) || gi_symbolp (obj))

extern int gi_get_xrep (char *buf, size_t len, SCM symbol_or_string);

#define GI_GET_XREP(buf,obj)                    \
  gi_get_xrep (buf, sizeof buf, obj)

#define GI_GET_XREP_MAYBE(buf,obj)              \
  (STRING_OR_SYMBOL_P (obj)                     \
   && 0 < GI_GET_XREP (buf, obj))

extern void gi_define (const char *name, SCM value);

extern SCM gi_primitive_eval (SCM form);
extern SCM gi_primitive_load (const char *filename);

extern svz_smob_tag_t gi_make_tag (const char *description, size_t sz,
                                   const void *fn_free,
                                   const void *fn_print,
                                   const void *fn_equalp);

extern int gi_smob_tagged_p (SCM obj, svz_smob_tag_t tag);
extern SCM gi_make_smob (svz_smob_tag_t tag, void *data);
extern void *gi_smob_data (SCM smob);

#define gi_make_hash_table(size)   MAKE_HASH_TABLE (size)

extern SCM gi_hash_clear_x (SCM table);

/* Idioms.  */

#define BOUNDP(x)  (! SCM_UNBNDP (x))

#define PACK_POINTER(x)    (scm_object_address (PTR2SCM (x)))
#define UNPACK_POINTER(x)  ((void *) gi_scm2ulong (x))

#endif  /* !defined __GI_H__ */

/* gi.h ends here */
