/*
 * pipe-socket.h - pipes in socket structures header definitions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __PIPE_SOCKET_H__
#define __PIPE_SOCKET_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/socket.h"
/* end svzint */

#define SVZ_READ   0                    /* read pipe index */
#define SVZ_WRITE  1                    /* write pipe index */

/*
 * Definition of a named pipe.
 */
typedef struct svz_pipe
{
  char *name;        /* name of named pipe */
  mode_t perm;       /* user and group permissions */
  char *user;        /* user name */
  uid_t uid;         /* user id (calculated from user name) */
  gid_t pgid;        /* primary group id */
  char *group;       /* group name */
  gid_t gid;         /* group id (calculated from group name) */
}
svz_pipe_t;

__BEGIN_DECLS

SBO int svz_pipe_valid (svz_socket_t *);
SBO int svz_pipe_read_socket (svz_socket_t *);
SBO int svz_pipe_write_socket (svz_socket_t *);
SBO int svz_pipe_disconnect (svz_socket_t *);
SERVEEZ_API svz_socket_t *svz_pipe_create (svz_t_handle,
                                           svz_t_handle);
SERVEEZ_API int svz_pipe_create_pair (svz_t_handle pipe_desc[2]);
SERVEEZ_API svz_socket_t *svz_pipe_connect (svz_pipe_t *,
                                            svz_pipe_t *);
SBO int svz_pipe_listener (svz_socket_t *, svz_pipe_t *, svz_pipe_t *);
SBO int svz_pipe_check_user (svz_pipe_t *);
SBO int svz_pipe_check_group (svz_pipe_t *);

SERVEEZ_API void svz_invalidate_handle (svz_t_handle *);
SERVEEZ_API int svz_invalid_handle_p (svz_t_handle);
SERVEEZ_API int svz_closehandle (svz_t_handle);

__END_DECLS

#endif /* not __PIPE_SOCKET_H__ */
