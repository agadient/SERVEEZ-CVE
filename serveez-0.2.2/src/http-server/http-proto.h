/*
 * http-proto.h - http protocol header file
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __HTTP_PROTO_H__
#define __HTTP_PROTO_H__

#include "http-cache.h"

/*
 * This is the http server configuration structure for one instance.
 */
typedef struct
{
  char *indexfile;      /* the standard index file */
  char *docs;           /* http document root */
  char *cgiurl;         /* cgi url (this is for its detection) */
  char *cgidir;         /* cgi directory where all cgi scripts are located */
  int cachesize;        /* maximum cache file size */
  int cacheentries;     /* maximum cache entries */
  int timeout;          /* timeout in seconds for keep-alive connections */
  int keepalive;        /* maximum amount of requests on a connection */
  char *default_type;   /* the default content type */
  char *type_file;      /* content type file (e.g "/etc/mime.types") */
  svz_hash_t *types;    /* content type hash */
  svz_hash_t *cgiapps;  /* cgi application associations */
  char *admin;          /* email address of server administrator */
  char *host;           /* host name of which is sent back to clients */
  char *userdir;        /* appended onto a user's home (~user request) */
  int nslookup;         /* enable reverse DNS lookups */
  int ident;            /* enable identd requests */
  char *logfile;        /* log file name */
  char *logformat;      /* custom log file format string */
  FILE *log;            /* log file stream */
}
http_config_t;

/* Export the http server definition to `server.c'.  */
extern svz_servertype_t http_server_definition;

/* server functions */
int http_init (svz_server_t *server);
int http_finalize (svz_server_t *server);
int http_global_init (svz_servertype_t *server);
int http_global_finalize (svz_servertype_t *server);

/* basic protocol functions */
int http_detect_proto (svz_server_t *server, svz_socket_t *sock);
int http_connect_socket (svz_server_t *server, svz_socket_t *sock);
char *http_info_client (svz_server_t *server, svz_socket_t *sock);
char *http_info_server (svz_server_t *server);

/* internal protocol functions */
int http_check_request (svz_socket_t *sock);
int http_default_write (svz_socket_t *sock);
int http_disconnect (svz_socket_t *sock);
void http_free_socket (svz_socket_t *sock);
int http_idle (svz_socket_t *sock);

/* http response functions including their flags */
int http_get_response (svz_socket_t *sock, char *request, int flags);
int http_head_response (svz_socket_t *sock, char *request, int flags);
int http_default_response (svz_socket_t *sock, char *request, int flags);

#endif /* __HTTP_PROTO_H__ */
