/* mkhash.h --- provide ‘MAKE_HASH_TABLE’
   serial 1

   Copyright (C) 2013 Thien-Thi Nguyen

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

#define MAKE_HASH_TABLE(size)                   \
  scm_c_make_hash_table (size)

#else  /* !GI_LEVEL_1_8 */

#define __REASONABLE_NBUCKETS(count)            \
  (((count) <= 23) ? 5                          \
   : (((count) <= 113) ? 23                     \
      : (((count) <= 211) ? 57                  \
         : 113)))

#define MAKE_HASH_TABLE(size)                   \
  (scm_make_vector                              \
   (NUM_INT (__REASONABLE_NBUCKETS (size)),     \
    SCM_EOL))

#endif /* !GI_LEVEL_1_8 */

/* mkhash.h ends here */
