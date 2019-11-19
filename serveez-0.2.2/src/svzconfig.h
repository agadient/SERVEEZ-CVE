/* src/svzconfig.h.  Generated from svzconfig.h.in by configure.  */
/*
 * svzconfig.h.in - serveez core library definitions.
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
 *
 */

#ifndef __SVZCONFIG_H__
#define __SVZCONFIG_H__ 1

/* Define to 1 if you have the <winsock2.h> header file.  */
/* #undef HAVE_WINSOCK2_H */

/* Define to 1 if you have the <arpa/inet.h> header file.  */
#define HAVE_ARPA_INET_H 1

/* Define to 1 if you have the <netinet/in.h> header file.  */
#define HAVE_NETINET_IN_H 1

/* Define to 'int' if <winsock2.h> does not define 'HANDLE'.  */
#define svz_t_handle int

/* Define to 'int' if <winsock2.h> does not define 'SOCKET'.  */
#define svz_t_socket int

/* Make CygWin / MinGW32 use large FD sets.  */
/* #undef FD_SETSIZE */

/* Define for faster code generation.  */
/* #undef WIN32_LEAN_AND_MEAN */

/* Define if you are using Windows Socket-API (not CYGWIN).  */
/* #undef Win32_Winsock */

#endif /* !__SVZCONFIG_H__ */
