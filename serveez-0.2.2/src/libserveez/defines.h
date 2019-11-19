/*
 * defines.h - useful global definitions for portability
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __DEFINES_H__
#define __DEFINES_H__ 1

#include "svzconfig.h"  /* NB: See ../Makefile.am ‘install-data-hook’.  */

/* System headers: standard ones unconditional;
   the rest only ‘#ifdef HAVE_HEADER_H’ .. ‘#endif’.  */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#ifdef HAVE_WINSOCK2_H
#include <winsock2.h>
#endif

#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

/* begin svzint */
#if GCC_HAS_ATTRIBUTE_VISIBILITY
#define DSOPRIVATE  __attribute__ ((__visibility__ ("hidden")))
#else
#define DSOPRIVATE
#endif

/* Serveez build only.  */
#define SBO  DSOPRIVATE extern
/* end svzint */

/* ‘__BEGIN_DECLS’ should be used at the beginning of your declarations,
   so that C++ compilers don't mangle their names.  Use ‘__END_DECLS’ at
   the end of C declarations.  */

#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS
# define __END_DECLS
#endif

/* ‘SERVEEZ_API’ is a macro prepended to all function and data definitions
   which should be exported or imported in the resulting dynamic link
   library in the Win32 port.  */

#if defined (__SERVEEZ_IMPORT__)
# define SERVEEZ_API __declspec (dllimport) extern
#elif defined (__SERVEEZ_EXPORT__) || defined (DLL_EXPORT)
# define SERVEEZ_API __declspec (dllexport) extern
#else
# define SERVEEZ_API extern
#endif

/* begin svzint */

/* This structure holds all the private dynamic state of the library.  */
typedef struct
{
  char *client;
  /* The program using this library, specified to ‘svz_boot’,
     primarily for use by the SEGV handler.  */

  time_t boot;
  /* The time when ‘svz_boot’ was called.  */

  int nclient_max;
  /* Maxium number of clients allowed to connect.  */
} svz_private_t;

__BEGIN_DECLS
SBO svz_private_t *svz_private;
__END_DECLS

#define THE(x)  svz_private->x

/* end svzint */
#endif /* !__DEFINES_H__ */
