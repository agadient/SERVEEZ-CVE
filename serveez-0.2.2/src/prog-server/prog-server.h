/*
 * prog-server.h - passthrough server header
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __PROG_SERVER_H__
#define __PROG_SERVER_H__ 1

/*
 * Protocol server specific configuration.
 */
typedef struct
{
  char *bin;           /* Executable file.  */
  char *dir;           /* Working directory or NULL.  */
  char *user;          /* user[.group] or NULL.  */
  svz_array_t *argv;   /* Arguments for the executable.  Watch argv[0].  */
  int fork;            /* Flag: fork or shuffle for passthrough method.  */
  int single_threaded; /* Flag: single- or multi-threaded packet server.  */
  size_t frequency;    /* Maximum number of threads per minute.  */
  int (* check_request) (svz_socket_t *);
  svz_array_t *accepted;
}
prog_config_t;

/*
 * Server callback definitions.
 */
int prog_detect_proto (svz_server_t *server, svz_socket_t *sock);
int prog_connect_socket (svz_server_t *server, svz_socket_t *sock);
int prog_init (svz_server_t *server);
int prog_global_init (svz_servertype_t *server);
int prog_finalize (svz_server_t *server);
int prog_global_finalize (svz_servertype_t *server);
int prog_notify (svz_server_t *server);
char *prog_info_server (svz_server_t *server);
char *prog_info_client (svz_server_t *server, svz_socket_t *sock);
int prog_handle_request (svz_socket_t *sock, char *request, int len);
int prog_read_socket (svz_socket_t *sock);
int prog_check_request (svz_socket_t *sock);
int prog_child_died (svz_socket_t *sock);

/*
 * This server's definition.
 */
extern svz_servertype_t prog_server_definition;

#endif /* __PROG_SERVER_H__ */
