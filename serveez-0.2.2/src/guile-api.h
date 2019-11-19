/*
 * guile-api.h - compatibility and miscellaneous guile functionality
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

#ifndef __GUILE_API_H__
#define __GUILE_API_H__ 1

/* Compatibility definitions for various Guile versions.  These definitions
   are mainly due to the fact that the gh interface is deprecated in newer
   versions.  */
#ifndef SCM_PROCEDUREP
#define SCM_PROCEDUREP(obj) gi_nfalsep (scm_procedure_p (obj))
#endif
#ifndef SCM_POSITIVEP
#define SCM_POSITIVEP(obj) gi_nfalsep (scm_positive_p (obj))
#endif
#ifndef SCM_NEGATIVEP
#define SCM_NEGATIVEP(obj) gi_nfalsep (scm_negative_p (obj))
#endif
#ifndef SCM_PAIRP
#define SCM_PAIRP(obj) gi_nfalsep (scm_pair_p (obj))
#endif
#ifndef SCM_LISTP
#define SCM_LISTP(obj) gi_nfalsep (scm_list_p (obj))
#endif
#ifndef SCM_BOOLP
#define SCM_BOOLP(obj) gi_nfalsep (scm_boolean_p (obj))
#endif
#ifndef SCM_BOOL
#define SCM_BOOL(x) ((x) ? SCM_BOOL_T : SCM_BOOL_F)
#endif
#ifndef SCM_EQ_P
#define SCM_EQ_P(x, y) gi_nfalsep (scm_eq_p (x, y))
#endif
#ifndef SCM_CHARP
#define SCM_CHARP(obj) gi_nfalsep (scm_char_p (obj))
#endif
#ifndef SCM_CHAR
#define SCM_CHAR(x) SCM_ICHR (x)
#endif
#ifndef SCM_MAKE_CHAR
#define SCM_MAKE_CHAR(x) SCM_MAKICHR (x)
#endif
#ifndef SCM_OUT_OF_RANGE
#define SCM_OUT_OF_RANGE(pos, arg) \
    scm_out_of_range_pos (FUNC_NAME, arg, gi_integer2scm (pos))
#endif

/* Idioms.  */

#define ASSERT_EXACT(n,obj)                     \
  SCM_ASSERT_TYPE                               \
  (gi_exactp (obj), obj,                        \
   SCM_ARG ## n, FUNC_NAME, "exact")

#define ASSERT_STRING(n,obj)                    \
  SCM_ASSERT_TYPE                               \
  (gi_stringp (obj), obj,                       \
   SCM_ARG ## n, FUNC_NAME, "string")

#define BSMOB_WHAT  "svz-binary"
#define BDATA_WHAT  "svz-binary-data"

/* You are lucky: We have successfully resisted the urge
   to name this ‘BEEE_FREEEEEEEE_XXXXX_ONE’.  */
#define BFREE(what,ptr,len)  gi_free (ptr, len, B ## what ## _WHAT)

#endif /* not __GUILE_API_H__ */
