/* humdrum.h --- old gh (et al) stuff
   serial 4

   Copyright (C) 2012, 2013 Thien-Thi Nguyen

   This is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this package; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA  02110-1301  USA.
*/

#if GI_LEVEL_1_8

#define NULLP             scm_is_null
#define PAIRP             scm_is_pair
#define SYMBOLP           scm_is_symbol
#define STRINGP           scm_is_string
#define NUMBERP           scm_is_number
#define INTEGERP          scm_is_integer
#define VECTORP           scm_is_vector
#define PROCEDUREP(x)     scm_is_true (scm_procedure_p (x))
#define BOOLEAN           scm_from_bool
#define NUM_INT           scm_from_int
#define NUM_LONG          scm_from_long
#define NUM_ULONG         scm_from_ulong
#define SYMBOL            scm_from_locale_symbol
#define SYMBOLN           scm_from_locale_symboln
#define STRING            scm_from_locale_string
#define BSTRING           scm_from_locale_stringn
#define C_BOOL            scm_to_bool
#define C_INT             scm_to_int
#define C_CHAR(c)         C_INT (scm_char_to_integer (c))
#define C_LONG            scm_to_long
#define C_ULONG           scm_to_ulong
#define C_DOUBLE          scm_to_double
#define VECTOR_LEN        scm_c_vector_length
#define VECTOR_REF        scm_c_vector_ref
#define EQ                scm_is_eq
#define CONS              scm_cons
#define CAR               scm_car
#define CDR               scm_cdr
#define APPLY             scm_apply_0
#define LISTIFY           scm_list_n
#define CALL0             scm_call_0
#define CALL1             scm_call_1
#define CALL2             scm_call_2
#define CALL3             scm_call_3
#define LIST1             scm_list_1
#define LIST2             scm_list_2
#define LIST3             scm_list_3
#define LIST4             scm_list_4
#define LIST5             scm_list_5
#define GC_PROTECT        scm_gc_protect_object
#define GC_UNPROTECT      scm_gc_unprotect_object
#define DEFINE_PUBLIC(name,value)  do           \
    {                                           \
      scm_c_define (name, value);               \
      scm_c_export (name, NULL);                \
    }                                           \
  while (0)
#define MODULE_LOOKUP(module, name)                     \
  scm_variable_ref (scm_c_module_lookup (module, name))

#else  /* !GI_LEVEL_1_8 */

#include <guile/gh.h>
#define NULLP             gh_null_p
#define PAIRP             gh_pair_p
#define SYMBOLP           gh_symbol_p
#define STRINGP           gh_string_p
#define NUMBERP           gh_number_p
#define INTEGERP(obj)     SCM_NFALSEP (scm_integer_p (obj))
#define VECTORP           gh_vector_p
#define PROCEDUREP        gh_procedure_p
#define BOOLEAN           gh_bool2scm
#define NUM_INT           gh_int2scm
#define NUM_LONG          gh_long2scm
#define NUM_ULONG         gh_ulong2scm
#define SYMBOL            gh_symbol2scm
#define SYMBOLN(p,len)    CAR (scm_intern (p, len))
#define STRING            gh_str02scm
#define BSTRING           gh_str2scm
#define C_BOOL            gh_scm2bool
#define C_INT             gh_scm2int
#define C_CHAR            gh_scm2char
#define C_LONG            gh_scm2long
#define C_ULONG           gh_scm2ulong
#define C_DOUBLE          gh_scm2double
#define VECTOR_LEN        gh_vector_length
#define VECTOR_REF(v,ix)  gh_vector_ref (v, NUM_ULONG (ix))
#define EQ                gh_eq_p
#define CONS              gh_cons
#define CAR               gh_car
#define CDR               gh_cdr
#define APPLY             gh_apply
#define LISTIFY           gh_list
#define CALL0             gh_call0
#define CALL1             gh_call1
#define CALL2             gh_call2
#define CALL3             gh_call3
#define LIST1             SCM_LIST1
#define LIST2             SCM_LIST2
#define LIST3             SCM_LIST3
#define LIST4             SCM_LIST4
#define LIST5             SCM_LIST5
#define GC_PROTECT        scm_protect_object
#define GC_UNPROTECT      scm_unprotect_object
#define DEFINE_PUBLIC     gh_define
#define MODULE_LOOKUP     gh_module_lookup

#endif  /* !GI_LEVEL_1_8 */

/* humdrum.h ends here */
