/*
 * binding.h - server to port binding declarations and definitions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __BINDING_H__
#define __BINDING_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
/* end svzint */

/* begin svzint */
/*
 * A server can typically be bound to different port configurations.  This
 * structure hold the binding for a single listening socket structure of a
 * server.
 */
typedef struct svz_binding
{
  svz_server_t *server; /* The server structure.  */
  svz_portcfg_t *port;  /* The port configuration the server is bound to.  */
}
svz_binding_t;
/* end svzint */

__BEGIN_DECLS
SBO svz_array_t *svz_sock_bindings (const svz_socket_t *);
SBO void svz_sock_bindings_set (svz_socket_t *, svz_socket_t *);
SBO size_t svz_sock_bindings_zonk_server (svz_socket_t *, svz_server_t *);
SBO void svz_binding_destroy (svz_binding_t *);
SBO svz_array_t *svz_binding_filter (svz_socket_t *);

SERVEEZ_API int svz_server_bind (svz_server_t *, svz_portcfg_t *);
SERVEEZ_API svz_array_t *svz_server_portcfgs (svz_server_t *);
SERVEEZ_API svz_array_t *svz_server_listeners (svz_server_t *);
SERVEEZ_API svz_array_t *svz_sock_servers (svz_socket_t *);
SERVEEZ_API int svz_binding_contains_server (svz_socket_t *,
                                             svz_server_t *);
SERVEEZ_API size_t svz_pp_server_bindings (char *, size_t, svz_server_t *);

__END_DECLS

#endif /* not __BINDING_H__ */
