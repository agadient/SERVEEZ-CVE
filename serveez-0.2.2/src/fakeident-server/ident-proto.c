/*
 * ident-proto.c - fake ident server implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2002 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2001 Raimund Jacob <raimi@lkcc.org>
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

#include "config.h"

#include <stdio.h>
#include <ctype.h>
#include "networking-headers.h"
#include "libserveez.h"
#include "ident-proto.h"
#include "unused.h"

/*
 * Configuration for this server containing the default values.
 */
struct fakeident_config fakeident_config =
{
  "UNIX",
  NULL,
};

/*
 * The config prototype.  Register items that can be configured.
 */
svz_key_value_pair_t fakeident_config_prototype [] =
{
  SVZ_REGISTER_STR ("systemtype", fakeident_config.systemtype,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("username", fakeident_config.username,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_END ()
};

/*
 * Function forward declaration.
 */
int fakeident_init (svz_server_t *server);
int fakeident_connect_socket (svz_server_t *server, svz_socket_t *sock);
int fakeident_detect_proto (svz_server_t *server, svz_socket_t *sock);
int fakeident_handle_request (svz_socket_t *sock, char *request, int len);
char *fakeident_info_server (svz_server_t *server);

/*
 * The actual server definition.
 */
svz_servertype_t fakeident_server_definition =
{
  "simple fake ident server",
  "fakeident",
  NULL,                       /* global init */
  fakeident_init,
  fakeident_detect_proto,
  fakeident_connect_socket,
  NULL,                       /* local finalize */
  NULL,                       /* global finalize */
  NULL,
  fakeident_info_server,
  NULL,
  NULL,
  NULL,
  SVZ_CONFIG_DEFINE ("fakeident", fakeident_config, fakeident_config_prototype)
};

/*
 * Initialize a fakeident server instance
 */
int
fakeident_init (UNUSED svz_server_t *server)
{
  return 0;
}

/*
 * When we get a connection this callback is invoked.  set up more callbacks.
 */
int
fakeident_connect_socket (UNUSED svz_server_t *server,
                          svz_socket_t *sock)
{
  sock->boundary = "\n";
  sock->boundary_size = 1;
  sock->check_request = svz_sock_check_request;
  sock->handle_request = fakeident_handle_request;
  return 0;
}

/*
 * Try to find out of that line is meant for us.
 * A valid request for us is: "number[space]*,[space]*number[space]*\r\n
 */
int
fakeident_detect_proto (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  int retval = 0;
  char *p = sock->recv_buffer;
  char *end = sock->recv_buffer + sock->recv_buffer_fill;

  /* first a number */
  for (; p < end && isdigit ((int) *p); p++);
  if (p == end)
    goto out;

  /* spaces */
  for (; p < end && *p == ' '; p++);
  if (p == end)
    goto out;

  /* comma */
  if (*p == ',')
    p++;
  else
    goto out;

  /* spaces */
  for (; p < end && *p == ' '; p++);
  if (p == end)
    goto out;

  /* number */
  for (; p < end && isdigit ((int) *p); p++);
  if (p == end)
    goto out;

  /* spaces */
  for (; p < end && *p == ' '; p++);
  if (p == end)
    goto out;

  /* optional '\r' */
  if (p < end && *p == '\r')
    p++;

  /* now, if that is a '\n' we finally have it */
  if (p < end && *p == '\n')
    retval = -1;

 out:
  return retval;
}

/*
 * Handle a single request when an input line is recognized.
 */
int
fakeident_handle_request (svz_socket_t *sock,
                          UNUSED char *request, UNUSED int len)
{
  struct fakeident_config *cfg = (struct fakeident_config *) sock->cfg;
  int err = 0;
  char *p = sock->recv_buffer;

  /* isolate the number pair for the reply */
  for (; *p != '\r' && *p != '\n'; p++);
  *p = '\0';

  /*
   * This is the last line we send.  Let the runtime close the connection
   * when data is away.
   */
  sock->flags |= SVZ_SOFLG_FINAL_WRITE;

  /*
   * If a username is set we reply "systemtype : username"
   * or "ERROR : NO-USER" else.
   */
  if (cfg->username == NULL)
    {
      err = svz_sock_printf (sock, "%s : ERROR : NO-USER\r\n",
                             sock->recv_buffer);
    }
  else
    {
      err = svz_sock_printf (sock, "%s : USERID : %s : %s\r\n",
                             sock->recv_buffer,
                             cfg->systemtype, cfg->username);
    }

  return err;
}

/*
 * Info about server as seen in control protocol.
 */
char *
fakeident_info_server (svz_server_t *server)
{
  struct fakeident_config *cfg = server->cfg;
  static char info[80];

  if (cfg->username == NULL)
    {
      sprintf (info, " signaling ERROR : NO-USER");
    }
  else
    {
      sprintf (info, " reporting user `%s' running system type `%s'",
               cfg->username, cfg->systemtype);
    }

  return info;
}
