/*
 * util.h - utility function interface
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

#ifndef __UTIL_H__
#define __UTIL_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
/* end svzint */

/*
 * level of server's verbosity:
 * 0 - only fatal error messages
 * 1 - error messages
 * 2 - warnings
 * 3 - informational messages
 * 4 - debugging output
 * levels always imply numerically lesser levels
 */
#define SVZ_LOG_FATAL     0
#define SVZ_LOG_ERROR     1
#define SVZ_LOG_WARNING   2
#define SVZ_LOG_NOTICE    3
#define SVZ_LOG_DEBUG     4

__BEGIN_DECLS

/* begin svzint */
#ifdef __MINGW32__
SBO int svz_errno;
#else
#define svz_errno errno
#endif
/* end svzint */

SERVEEZ_API void svz_log (int, const char *, ...);
SERVEEZ_API void svz_log_setfile (FILE *);
SBO int svz_pton (const char *, void *);
SERVEEZ_API int svz_hexdump (FILE *, char *, int, char *, int, int);
SERVEEZ_API char *svz_itoa (unsigned int);
SERVEEZ_API unsigned int svz_atoi (char *);
SERVEEZ_API char *svz_getcwd (void);
SERVEEZ_API int svz_openfiles (int);
SERVEEZ_API char *svz_time (long);
SERVEEZ_API char *svz_tolower (char *);
SERVEEZ_API char *svz_sys_version (void);

SERVEEZ_API int svz_socket_unavailable_error_p (void);

SERVEEZ_API const char *svz_sys_strerror (void);
SBO const char *svz_net_strerror (void);
SERVEEZ_API void svz_log_sys_error (char const *, ...);
SERVEEZ_API void svz_log_net_error (char const *, ...);

/* begin svzint */
#ifndef __MINGW32__
#define INVALID_SOCKET  ((svz_t_socket) -1)
#endif

#ifdef __MINGW32__
# define SOCK_INPROGRESS   WSAEINPROGRESS
#else /* !__MINGW32__ */
# define SOCK_INPROGRESS   EINPROGRESS
#endif /* !__MINGW32__ */
/* end svzint */

SERVEEZ_API int svz_mingw_at_least_nt4_p (void);

__END_DECLS

#endif /* not __UTIL_H__ */
