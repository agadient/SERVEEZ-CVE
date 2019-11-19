/*
 * windoze.h - windows port interface
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __WINDOZE_H__
#define __WINDOZE_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
/* end svzint */

#ifdef __MINGW32__

__BEGIN_DECLS

SERVEEZ_API int svz_windoze_daemon_control (char *);
SERVEEZ_API WCHAR *svz_windoze_asc2uni (CHAR *asc);
SERVEEZ_API CHAR *svz_windoze_uni2asc (WCHAR *unicode);
SBO unsigned svz_windoze_get_reg_unsigned (HKEY, char *, char *, unsigned);
SBO void svz_windoze_set_reg_unsigned (HKEY, char *, char *, unsigned);
SBO char *svz_windoze_get_reg_string (HKEY, char *, char *, char *);
SBO void svz_windoze_set_reg_string (HKEY, char *, char *, char *);

__END_DECLS

#endif  /* __MINGW32__ */

#endif /* not __WINDOZE_H__ */
