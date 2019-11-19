/*
 * server.h - generic server definitions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
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

#ifndef __SERVER_H__
#define __SERVER_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/array.h"
#include "libserveez/hash.h"
#include "libserveez/portcfg.h"
#include "libserveez/cfg.h"
/* end svzint */

typedef struct svz_servertype svz_servertype_t;
typedef struct svz_server svz_server_t;

/*
 * Each server instance gets such a structure.
 */
struct svz_server
{
  /* one of the PROTO_ flags defined in <core.h> */
  int proto;
  /* variable name in configuration language, used to identify it */
  char *name;
  /* server description */
  char *description;
  /* configuration structure for this instance */
  void *cfg;
  /* pointer to this server instances server type */
  svz_servertype_t *type;
  /* arbitrary data field */
  void *data;

  /* init of instance */
  int (* init) (svz_server_t *);
  /* protocol detection */
  int (* detect_proto) (svz_server_t *, svz_socket_t *);
  /* what to do if detected */
  int (* connect_socket) (svz_server_t *, svz_socket_t *);
  /* finalize this instance */
  int (* finalize) (svz_server_t *);
  /* return client info */
  char * (* info_client) (svz_server_t *, svz_socket_t *);
  /* return server info */
  char * (* info_server) (svz_server_t *);
  /* server timer */
  int (* notify) (svz_server_t *);
  /* server reset callback */
  int (* reset) (svz_server_t *);
  /* packet processing */
  int (* handle_request) (svz_socket_t *, char *, int);
};

/*
 * Every type (class) of server is completely defined by the following
 * structure.
 */
struct svz_servertype
{
  /* full descriptive name */
  char *description;
  /* variable prefix (short name) as used in configuration */
  char *prefix;

  /* run once per server definition */
  int (* global_init) (svz_servertype_t *);
  /* per server instance callback */
  int (* init) (svz_server_t *);
  /* protocol detection routine */
  int (* detect_proto) (svz_server_t *, svz_socket_t *);
  /* for accepting a client (tcp or pipe only) */
  int (* connect_socket) (svz_server_t *, svz_socket_t *);
  /* per instance */
  int (* finalize) (svz_server_t *);
  /* per server definition */
  int (* global_finalize) (svz_servertype_t *);
  /* return client info */
  char * (* info_client) (svz_server_t *, svz_socket_t *);
  /* return server info */
  char * (* info_server) (svz_server_t *);
  /* server timer */
  int (* notify) (svz_server_t *);
  /* server reset */
  int (* reset) (svz_server_t *);
  /* packet processing */
  int (* handle_request) (svz_socket_t *, char *, int);

  /* configuration prototype */
  svz_config_prototype_t config_prototype;
};

/* begin svzint */
typedef struct
{
  char *name;
  int (*instantiate) (char *, char *, void *, svz_config_accessor_t *,
                      size_t, char *);
}
svz_config_type_t;
/* end svzint */

typedef int (svz_servertype_do_t) (const svz_servertype_t *, void *);
typedef void (svz_server_do_t) (svz_server_t *, void *);

__BEGIN_DECLS
SBO svz_config_type_t svz_servertype_definition;

SERVEEZ_API int svz_foreach_servertype (svz_servertype_do_t *, void *);
SERVEEZ_API void svz_foreach_server (svz_server_do_t *, void *);
SERVEEZ_API svz_server_t *svz_server_get (char *);
SERVEEZ_API svz_server_t *svz_server_find (void *);
SERVEEZ_API svz_array_t *svz_server_clients (svz_server_t *);
SERVEEZ_API int svz_updn_all_servers (int);

SERVEEZ_API void svz_servertype_add (svz_servertype_t *);
SERVEEZ_API svz_servertype_t *svz_servertype_get (char *, int);
SERVEEZ_API svz_servertype_t *svz_servertype_find (svz_server_t *);

__END_DECLS

#endif /* not __SERVER_H__ */
