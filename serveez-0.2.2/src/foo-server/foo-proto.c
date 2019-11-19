/*
 * foo-proto.c - example server implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
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
#include <string.h>
#include "networking-headers.h"
#include "libserveez.h"
#include "foo-proto.h"
#include "unused.h"

/*
 * Packet specification for @code{check_request}.
 */
char *foo_packet_delim     = "\r\n";
int   foo_packet_delim_len = 2;

/* Default port configuration.  */
svz_portcfg_t foo_default_port;

/*
 * Demonstrate how our private configuration looks like and provide
 * default values.
 */
foo_config_t foo_config =
{
  -42,               /* dummy integer */
  NULL,              /* string messages */
  "Default reply",   /* reply string */
  NULL,              /* ports (integer array) */
  42,                /* bar */
  &foo_default_port, /* default port configuration */
  NULL,              /* assoc hash table */
  1,                 /* default truth value */
};

/*
 * Defining configuration file associations with key-value-pairs.
 */
svz_key_value_pair_t foo_config_prototype [] =
{
  SVZ_REGISTER_INT ("bar", foo_config.bar, SVZ_ITEM_NOTDEFAULTABLE),
  SVZ_REGISTER_STR ("reply", foo_config.reply, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STRARRAY ("messages", foo_config.messages,
                         SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INTARRAY ("ports", foo_config.ports, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_HASH ("assoc", foo_config.assoc, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_PORTCFG ("port", foo_config.port, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_BOOL ("truth", foo_config.truth, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_END ()
};

/*
 * Definition of this server.
 */
svz_servertype_t foo_server_definition =
{
  "foo example server",
  "foo",
  foo_global_init,
  foo_init,
  foo_detect_proto,
  foo_connect_socket,
  foo_finalize,
  foo_global_finalize,
  NULL,
  foo_info_server,
  NULL,
  NULL,
  NULL,
  SVZ_CONFIG_DEFINE ("foo", foo_config, foo_config_prototype)
};

/* ************* Networking functions ************************* */

/*
 * This callback is used when a coserver asynchronously resolved the
 * client's ip to a name.
 */
static int
foo_rdns_done (char *host, void *closure)
{
  svz_sock_iv_t *x = closure;
  svz_socket_t *sock = svz_sock_find (x->id, x->version);

  svz_free (x);
  if (host && sock)
    svz_sock_printf (sock, "You are `%s'\r\n", host);
  return 0;
}

/*
 * Handle a single request as found by the `sock_check_request ()'.
 */
int
foo_handle_request (svz_socket_t *sock, UNUSED char *request, int len)
{
  foo_config_t *cfg = sock->cfg;

  return svz_sock_printf (sock, "%s: %d\r\n", cfg->reply, len);
}

/*
 * This callback gets called whenever some unknown client connects and
 * sends some data.  We check for some string that identifies the foo
 * protocol.
 */
int
foo_detect_proto (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  /* see if the stream starts with our identification string */
  if (sock->recv_buffer_fill >= 5 &&
      !memcmp (sock->recv_buffer, "foo\r\n", 5))
    {

      /* it's us: forget the id string and signal success */
      svz_sock_reduce_recv (sock, 5);
      return -1;
    }

  /* not us...  */
  return 0;
}

#define ENQ_COSERVER_REQUEST(req,coserver)      \
  svz_coserver_ ## coserver ## _invoke          \
  (req, foo_ ## coserver ## _done,              \
   svz_make_sock_iv (sock))

/*
 * Our detect proto thinks that sock is a foo connection, so install
 * the callbacks we need.
 */
int
foo_connect_socket (svz_server_t *server, svz_socket_t *sock)
{
  foo_config_t *cfg = server->cfg;
  size_t i;
  int ret;
  char *msg;

  /*
   * we uses a default routine to split incoming data into packets
   * (which happen to be lines)
   */
  sock->boundary = foo_packet_delim;
  sock->boundary_size = foo_packet_delim_len;
  sock->check_request = svz_sock_check_request;
  sock->handle_request = foo_handle_request;

  svz_log (SVZ_LOG_NOTICE, "foo client detected\n");

  svz_array_foreach (cfg->messages, msg, i)
    {
      ret = svz_sock_printf (sock, "%s\r\n", msg);
      if (ret)
        return ret;
    }

  /*
   * Ask a coserver to resolve the client's ip
   */
  svz_sock_printf (sock, "Starting reverse lookup...\r\n");
  ENQ_COSERVER_REQUEST (sock->remote_addr, rdns);
  svz_sock_printf (sock, "Waiting...\r\n");
  return 0;
}

/* ************************** Initialization ************************** */

/*
 * Called once of the foo server type.  We use it to create the default
 * values.
 */
int
foo_global_init (UNUSED svz_servertype_t *server)
{
  char *strarray[] = {
    "Hello !", "This", "is", "a", "default", "string", "array."
  };
  int intarray[] = {
    1, 2, 3, 4
  };
  char *strhash[] = {
    "Grass", "green",
    "Milk",  "tasty",
    "Sun",   "light",
    "Moon",  "tide",
    "GNU",   "good"
  };
  svz_portcfg_t *d = &foo_default_port;

  /* Default port configuration.  */
  foo_default_port.proto = SVZ_PROTO_TCP;
  SVZ_CFG_TCP (d, port) = 42421;
  SVZ_CFG_TCP (d, ipaddr) = "*";

  /* Default string array.  */
  foo_config.messages = SVZ_COLLECT_STRARRAY (strarray);

  /* Default integer array.  */
  foo_config.ports = SVZ_COLLECT_INTARRAY (intarray);

  /* Default hash table.  */
  foo_config.assoc = SVZ_COLLECT_STRHASH (strhash);

  return 0;
}

/*
 * Called once for foo servers, free our default values.
 */
int
foo_global_finalize (UNUSED svz_servertype_t *server)
{
  svz_array_destroy (foo_config.ports);
  svz_array_destroy (foo_config.messages);
  svz_hash_destroy (foo_config.assoc);
  return 0;
}

/*
 * A single foo server instance gets destroyed.  Free the hash
 * unless it is the default hash.
 */
int
foo_finalize (svz_server_t *server)
{
  svz_log (SVZ_LOG_NOTICE, "destroying %s\n", server->name);
  return 0;
}

/*
 * Initialize a foo server instance.
 */
int
foo_init (UNUSED svz_server_t *server)
{
  return 0;
}

struct info_server_closure
{
  char *info;
  int i;
};

static void
info_server_internal (void *k, void *v, void *closure)
{
  char *key = k, *value = v;
  struct info_server_closure *x = closure;
  char text[80];

  sprintf (text, " assoc[%d] : `%s' => `%s'\r\n", (x->i)++, key, value);
  strcat (x->info, text);
}

/*
 * Server info callback.  We use it here to print the
 * whole configuration once.
 */
char *
foo_info_server (svz_server_t *server)
{
  foo_config_t *cfg = server->cfg;
  static char info[80*16], text[80];
  char *str;
  void *j;
  size_t i;
  svz_hash_t *h;

  sprintf (text,
           " reply : %s\r\n"
           " bar   : %d\r\n"
           " truth : %d\r\n",
           cfg->reply, cfg->bar, cfg->truth);
  strcpy (info, text);

  svz_array_foreach (cfg->messages, str, i)
    {
      sprintf (text, " messages[%zu] : %s\r\n", i, str);
      strcat (info, text);
    }

  svz_array_foreach (cfg->ports, j, i)
    {
      sprintf (text, " ports[%zu] : %d\r\n", i, (int) ((long) j));
      strcat (info, text);
    }

  if ((h = cfg->assoc) != NULL)
    {
      if (svz_hash_size (h))
        {
          struct info_server_closure x = { info, 0 };

          svz_hash_foreach (info_server_internal, h, &x);
        }
    }
  else
    {
      sprintf (text, " assoc : NULL\r\n");
      strcat (info, text);
    }

  return info;
}
