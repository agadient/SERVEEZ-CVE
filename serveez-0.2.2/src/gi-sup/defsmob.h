/* defsmob.h --- provide ‘DEFSMOB’, ‘GCMALLOC’, ‘CFREE’, ‘GCRV’
   serial 2

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

typedef scm_t_bits smob_tag_t;

static inline smob_tag_t
__guile_baux_other__defsmob (const char *name,
           SCM (*m) (SCM),
           size_t (*f) (SCM),
           int (*p) (SCM, SCM, scm_print_state*))
{
  smob_tag_t tag = scm_make_smob_type (name, 0);

  if (m) scm_set_smob_mark  (tag, m);
  if (f) scm_set_smob_free  (tag, f);
  if (p) scm_set_smob_print (tag, p);
  return tag;
}

#define DEFSMOB(tagvar,name,m,f,p)                      \
  tagvar = __guile_baux_other__defsmob (name, m, f, p)

#define GCMALLOC(sz,what)    scm_gc_malloc (sz, what)
#define GCFREE(ptr,what)     scm_gc_free (ptr, sizeof (*ptr), what)
#define GCRV(ptr)            0

#else  /* !GI_LEVEL_1_8 */

typedef long smob_tag_t;

#define DEFSMOB(tagvar,name,m,f,p)                              \
  tagvar = scm_make_smob_type_mfpe (name, 0, m, f, p, NULL)

#define GCMALLOC(sz,what)    scm_must_malloc (sz, what)
#define GCFREE(ptr,what)     scm_must_free (ptr)
#define GCRV(ptr)            sizeof (*ptr)

#endif  /* !GI_LEVEL_1_8 */

/* defsmob.h ends here */
