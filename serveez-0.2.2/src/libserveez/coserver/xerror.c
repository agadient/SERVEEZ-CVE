/*
 * xerror.c --- return a string describing a name/addr translation failure
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_NETDB_H
# include <netdb.h>
#endif
#include "libserveez/util.h"

#if !defined HAVE_DECL_H_ERRNO
#define h_errno  errno
#endif

#if !defined HAVE_HSTRERROR
#define hstrerror  strerror
#else
/* On some platforms ‘hstrerror’ can be linked but is not declared
   anywhere.  That is why we do it here by hand.  */
#if !defined HAVE_DECL_HSTRERROR
extern char * hstrerror (int);
#endif
#endif

const char *
xerror (void)
{
#if defined __MINGW32__
  return svz_net_strerror (void);
#else
  /* FIXME: Use ‘gai_strerror’ (on switch to get{name,addr}info).  */
  return hstrerror (h_errno);
#endif
}
