/*
 * boot.h - configuration and boot declarations
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __BOOT_H__
#define __BOOT_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
/* end svzint */

/* Runtime parameters.  */
#define SVZ_RUNPARM_VERBOSITY    0
#define SVZ_RUNPARM_MAX_SOCKETS  1

__BEGIN_DECLS

SERVEEZ_API const char * const * svz_library_features (size_t *);
SERVEEZ_API void svz_boot (char const *);
SERVEEZ_API long svz_uptime (void);
SERVEEZ_API int svz_runparm (int, int);
SERVEEZ_API void svz_halt (void);

__END_DECLS

/**
 * Return the value of runtime parameter @var{nick}.
 */
#define SVZ_RUNPARM(nick)                       \
  svz_runparm (-1, SVZ_RUNPARM_ ## nick)

/**
 * Set the runtime paramater @var{nick}
 * to have value @var{val}, an integer.
 */
#define SVZ_RUNPARM_X(nick,val)                 \
  svz_runparm (SVZ_RUNPARM_ ## nick, (val))

#endif /* not __BOOT_H__ */
