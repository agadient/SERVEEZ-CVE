/* misc-macros.h
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2001 Raimund Jacob <raimi@cs.tu-berlin.de>
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

#ifndef __MISC_MACROS_H__
#define __MISC_MACROS_H__ 1

/*
 * Free the memory block pointed to by @var{lval}
 * and set it to @code{NULL} afterwards.
 */
#define svz_free_and_zero(lval)  do             \
    {                                           \
      svz_free (lval);                          \
      (lval) = NULL;                            \
    }                                           \
  while (0)

/*
 * Convert the integer value @var{n} into a pointer.
 */
#define SVZ_NUM2PTR(n)  ((void *) ((unsigned long) (n)))

/*
 * Convert the pointer @var{p} into a integer value.
 */
#define SVZ_PTR2NUM(p)  ((unsigned long) ((void *) (p)))

#endif  /* !defined __MISC_MACROS_H__ */
