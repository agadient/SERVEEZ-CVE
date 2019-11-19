/* woe-statpred.h --- macros S_IS{DIR,CHR,REG,BLK} for __MINGW32__
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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

#ifndef __WOE_STATPRED_H__
#define __WOE_STATPRED_H__ 1

/*
 * This little modification is necessary for the native Win32 compiler.
 * We do have these macros defined in the MinGW32 and Cygwin headers
 * but not within the native Win32 headers.
 */
#ifndef S_ISDIR
# ifndef S_IFBLK
#  define S_IFBLK 0x3000
# endif
# define S_ISDIR(Mode) (((Mode) & S_IFMT) == S_IFDIR)
# define S_ISCHR(Mode) (((Mode) & S_IFMT) == S_IFCHR)
# define S_ISREG(Mode) (((Mode) & S_IFMT) == S_IFREG)
# define S_ISBLK(Mode) (((Mode) & S_IFMT) == S_IFBLK)
#endif /* not S_ISDIR */

#endif  /* !defined __WOE_STATPRED_H__ */
