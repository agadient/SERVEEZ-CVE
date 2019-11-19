/* fastint.h --- immediate numbers (aka fixnums)
   serial 2

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

#ifdef                        SCM_I_MAKINUM
#define NUM_FASTINT           SCM_I_MAKINUM
#define C_FASTINT             SCM_I_INUM
#else
#define NUM_FASTINT           NUM_INT
#define C_FASTINT             C_INT
#endif

#else  /* !GI_LEVEL_1_8 */

#define NUM_FASTINT           SCM_MAKINUM
#define C_FASTINT             SCM_INUM

#endif  /* !GI_LEVEL_1_8 */

/* fastint.h ends here */
