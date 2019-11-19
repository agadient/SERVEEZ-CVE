/*
 * passthrough.h - pass through declarations
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

#ifndef __PASSTHROUGH_H__
#define __PASSTHROUGH_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
/* end svzint */

/* Structure containing a system independent environment.  */
typedef struct
{
  int size;     /* Number of environment entries.  */
  char **entry; /* Environment entries in the format "VAR=VALUE".  */
  char *block;  /* Temporary environment block.  */
}
svz_envblock_t;

/* Definitions for the @var{user} argument of @code{svz_sock_process}.  */
#define SVZ_PROCESS_NONE  ((char *) 0L)
#define SVZ_PROCESS_OWNER ((char *) ~0L)

__BEGIN_DECLS

SERVEEZ_API int svz_sock_process (svz_socket_t *, char *, char *,
                                  char **, svz_envblock_t *, int,
                                  char *);
#ifdef __MINGW32__
SERVEEZ_API int svz_mingw_child_dead_p (char *, svz_t_handle *);
#endif
SERVEEZ_API int svz_most_recent_dead_child_p (svz_t_handle);
SERVEEZ_API void svz_envblock_setup (void);
SERVEEZ_API svz_envblock_t *svz_envblock_create (void);
SERVEEZ_API int svz_envblock_default (svz_envblock_t *);
SERVEEZ_API int svz_envblock_add (svz_envblock_t *, char *, ...);
SERVEEZ_API void svz_envblock_destroy (svz_envblock_t *);
SERVEEZ_API void * svz_envblock_get (svz_envblock_t *);

__END_DECLS

#endif /* __PASSTHROUGH_H__ */
