/* level.h --- define ‘GI_LEVEL’ and ‘GI_LEVEL_1_8’
   serial 1

   Copyright (C) 2012 Thien-Thi Nguyen

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

#ifndef SCM_MAJOR_VERSION
#define SCM_MAJOR_VERSION  1
#endif

#ifndef SCM_MINOR_VERSION
#define SCM_MINOR_VERSION  4
#endif

#define GI_LEVEL(major,minor)                   \
  (((major << 8)                                \
    + minor)                                    \
   <= ((SCM_MAJOR_VERSION << 8)                 \
       + SCM_MINOR_VERSION))

#define GI_LEVEL_1_8  GI_LEVEL (1, 8)

/* level.h ends here */
