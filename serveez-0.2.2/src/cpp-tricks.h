/* cpp-tricks.h --- using # and ## in various ways
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

#ifndef __CPP_TRICKS_H__
#define __CPP_TRICKS_H__ 1

/* See (info "(cpp-4.3) Stringification")
   or similar, for more info on this trick.  */
#define ___STR(s)   #s
#define ___XSTR(s)  ___STR (s)

/* This is useful for constructing format strings.  */
#define PERCENT_N_S(n)   "%" ___XSTR (n) "s"

#endif  /* !defined __CPP_TRICKS_H__ */
