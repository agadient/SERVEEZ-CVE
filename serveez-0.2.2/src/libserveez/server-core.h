/*
 * server-core.h - server management definition
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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

#ifndef __SERVER_CORE_H__
#define __SERVER_CORE_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/socket.h"
#include "libserveez/portcfg.h"
/* end svzint */

SBO svz_t_handle svz_child_died;
SBO int svz_nuke_happened;
SBO time_t svz_notify;
SBO svz_socket_t *svz_sock_root;

/* begin svzint */

/*
 * Go through each socket structure in the chained list.
 */
#define svz_sock_foreach(sock) \
  for ((sock) = svz_sock_root; (sock) != NULL; (sock) = (sock)->next)

/*
 * Goes through the chained list of socket structures and filters each
 * listener.
 */
#define svz_sock_foreach_listener(sock)                                \
  svz_sock_foreach (sock)                                              \
    if (((sock)->flags & SVZ_SOFLG_LISTENING) && (sock)->port != NULL)

/* end svzint */

typedef int (svz_socket_do_t) (svz_socket_t *, void *);

__BEGIN_DECLS
SBO int svz_sock_shutdown (svz_socket_t *);
SBO int svz_sock_check_access (svz_socket_t *, svz_socket_t *);
SBO void svz_sock_check_bogus (void);
SBO int svz_periodic_tasks (void);

SERVEEZ_API int svz_foreach_socket (svz_socket_do_t *, void *);
SERVEEZ_API svz_socket_t *svz_sock_find (int, int);
SERVEEZ_API int svz_sock_schedule_for_shutdown (svz_socket_t *);
SERVEEZ_API int svz_sock_enqueue (svz_socket_t *);
SERVEEZ_API void svz_sock_setparent (svz_socket_t *, svz_socket_t *);
SERVEEZ_API svz_socket_t *svz_sock_getparent (svz_socket_t *);
SERVEEZ_API void svz_sock_setreferrer (svz_socket_t *, svz_socket_t *);
SERVEEZ_API svz_socket_t *svz_sock_getreferrer (svz_socket_t *);
SERVEEZ_API svz_portcfg_t *svz_sock_portcfg (svz_socket_t *);

SERVEEZ_API int svz_shutting_down_p (void);
SERVEEZ_API void svz_loop_pre (void);
SERVEEZ_API void svz_loop_post (void);
SERVEEZ_API void svz_loop (void);
SERVEEZ_API void svz_loop_one (void);

__END_DECLS

#endif /* not __SERVER_CORE_H__ */
