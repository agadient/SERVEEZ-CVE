/*
 * gnutella.c - gnutella protocol implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
#endif

#ifdef __MINGW32__
# include <io.h>
# include "woe-statpred.h"
#endif

#if HAVE_DIRECT_H
# include <direct.h>
#endif
#ifdef __MINGW32__
# define mkdir(path, mode) mkdir (path)
#endif

#include "networking-headers.h"
#include "libserveez.h"
#include "misc-macros.h"
#include "gnutella.h"
#include "nut-transfer.h"
#include "nut-route.h"
#include "nut-core.h"
#include "nut-hostlist.h"
#include "nut-request.h"
#include "unused.h"

/*
 * Default search patterns.
 */
char *nut_search_patterns[] =
{
  "Puppe3000",
  "Meret Becker"
};

/*
 * Default configuration hash for the gnutella spider.
 */
nut_config_t nut_config =
{
  0,                   /* if set we do not listen on any port cfg */
  NUT_MAX_TTL,         /* maximum ttl for a gnutella packet */
  NUT_TTL,             /* initial ttl for a gnutella packet */
  NULL,                /* array of initial hosts */
  NUT_GUID,            /* this servers GUID */
  NULL,                /* routing table */
  NULL,                /* connected hosts hash */
  NULL,                /* default search pattern */
  0,                   /* current search pattern index */
  30,                  /* limit amount of search reply records */
  NULL,                /* this servers created packets */
  0,                   /* routing errors */
  0,                   /* files within connected network */
  0,                   /* file size (in KB) */
  0,                   /* hosts within the connected network */
  "/tmp",              /* where to store downloaded files */
  "/tmp",              /* local search database path */
  0,                   /* concurrent downloads */
  4,                   /* maximum concurrent downloads */
  28,                  /* connection speed (KBit/s) */
  28,                  /* minimum connection speed for searching */
  NULL,                /* file extensions */
  NULL,                /* host catcher */
  4,                   /* number of connections to keep up */
  NULL,                /* force the local ip to this value */
  0,                   /* calculated from `force_ip' */
  0,                   /* force the local port to this value */
  0,                   /* calculated from `force_port' */
  NULL,                /* recent query hash */
  NULL,                /* reply hash for routing push requests */
  NULL,                /* push request hash */
  NULL,                /* shared file array */
  0,                   /* number of database files */
  0,                   /* size of database in KB */
  0,                   /* current number of uploads */
  4,                   /* maximum number of uploads */
  "gnutella-net",      /* configurable gnutella net url */
  NULL,                /* detection string for the above value */
};

/*
 * Defining configuration file associations with key-value-pairs.
 */
svz_key_value_pair_t nut_config_prototype[] =
{
  SVZ_REGISTER_STRARRAY ("hosts", nut_config.hosts, SVZ_ITEM_NOTDEFAULTABLE),
  SVZ_REGISTER_STRARRAY ("search", nut_config.search, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("search-limit", nut_config.search_limit,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("max-ttl", nut_config.max_ttl, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("ttl", nut_config.ttl, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("download-path", nut_config.save_path,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("share-path", nut_config.share_path, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("max-downloads", nut_config.max_dnloads,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("connection-speed", nut_config.speed,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("min-speed", nut_config.min_speed, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STRARRAY ("file-extensions", nut_config.extensions,
                         SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("connections", nut_config.connections,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("force-ip", nut_config.force_ip, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("force-port", nut_config.force_port, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("max-uploads", nut_config.max_uploads,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("net-url", nut_config.net_url, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_BOOL ("disable", nut_config.disable, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_END ()
};

/*
 * Definition of this server.
 */
svz_servertype_t nut_server_definition =
{
  "gnutella spider version " NUT_VERSION, /* long description */
  "nut",                                  /* instance description */
  nut_global_init,                        /* global initializer */
  nut_init,                               /* instance initializer */
  nut_detect_proto,                       /* protocol detection */
  nut_connect_socket,                     /* client connection callback */
  nut_finalize,                           /* instance destructor */
  nut_global_finalize,                    /* class destructor */
  nut_info_client,                        /* server info callback */
  nut_info_server,                        /* client info callback */
  nut_server_notify,                      /* server timer routine */
  NULL,                                   /* no reset callback */
  NULL,                                   /* no handle request callback */
  SVZ_CONFIG_DEFINE ("nut", nut_config, nut_config_prototype)
};

/*
 * The next three functions `nut_hash_keylen', `nut_hash_equals' and
 * `nut_hash_code' are the routing table hash callbacks to handle
 * GUIDs as keys instead of plain NULL terminated character strings.
 */
static size_t
nut_hash_keylen (UNUSED const char *id)
{
  return NUT_GUID_SIZE;
}

static int
nut_hash_equals (const char *id1, const char *id2)
{
  return memcmp (id1, id2, NUT_GUID_SIZE);
}

static unsigned long
nut_hash_code (const char *id)
{
  int n;
  unsigned long code = 0;

  for (n = 0; n < NUT_GUID_SIZE; n++)
    {
      code = (code << 2) ^ id[n];
    }

  return code;
}

static svz_hash_t *
make_nut_kce_hash_table (svz_free_func_t destroy)
{
  return svz_hash_configure (svz_hash_create (4, destroy),
                             nut_hash_keylen,
                             nut_hash_code,
                             nut_hash_equals);
}

/*
 * This is the default idle function for self connected gnutella hosts.
 * It simply returns an error if the socket was not connected in a
 * certain time.
 */
int
nut_connect_timeout (svz_socket_t *sock)
{
  /*
   * Did we try to connect to another host in order to download something,
   * but failed within a certain time?  Then we need to send a push request
   * to the host providing the original data.
   */
  if (sock->userflags & NUT_FLAG_DNLOAD)
    {
      nut_send_push (sock->cfg, sock->data);
    }
  return -1;
}

/*
 * The following function tries to connect to a given gnutella host specified
 * by @var{ip:port} both in network byte order.  It returns -1 on errors and
 * zero otherwise.
 */
static int
nut_connect_ip (nut_config_t *cfg, in_addr_t ip, in_port_t port)
{
  svz_socket_t *sock;
  svz_address_t *addr = svz_address_make (AF_INET, &ip);

  /* try to connect to this host */
  if ((sock = svz_tcp_connect (addr, port)) != NULL)
    {
      char buf[64];

      svz_log (SVZ_LOG_NOTICE, "nut: connecting %s\n",
               SVZ_PP_ADDR_PORT (buf, addr, port));
      sock->cfg = cfg;
      sock->flags |= SVZ_SOFLG_NOFLOOD;
      sock->check_request = nut_detect_connect;
      sock->idle_func = nut_connect_timeout;
      sock->idle_counter = NUT_CONNECT_TIMEOUT;
      svz_sock_printf (sock, NUT_CONNECT);
      svz_free (addr);
      return 0;
    }
  svz_free (addr);
  return -1;
}

struct dns_closure
{
  nut_config_t *cfg;
  in_port_t port;
};

/*
 * This routine gets called when the DNS coserver finally resolved a given
 * hostname to an IP address and tries to connect to the gnutella client.
 */
static int
nut_dns_done (char *host, void *closure)
{
  struct dns_closure *x = closure;
  nut_config_t *cfg = x->cfg;
  in_port_t port = x->port;
  struct sockaddr_in addr;

  svz_free (x);
  if (host != NULL)
    {
      if (svz_inet_aton (host, &addr) == -1)
        {
          svz_log (SVZ_LOG_WARNING, "nut: invalid IP address `%s'\n", host);
          return -1;
        }
      return nut_connect_ip (cfg, addr.sin_addr.s_addr, port);
    }
  return -1;
}

/*
 * The following routine tries to connect to a given gnutella host.  The
 * @var{host} argument can be either in dotted decimal form or a hostname.
 * It returns -1 on errors and zero otherwise.
 */
static int
nut_connect_host (nut_config_t *cfg, char *host)
{
  nut_host_t *client;
  in_addr_t ip;
  in_port_t port;
  char *dns = NULL;

  /* try getting ip address and port */
  if (nut_parse_addr (host, &ip, &port) == -1)
    {
      if ((dns = nut_parse_host (host, &port)) == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "nut: invalid host `%s'\n", host);
          return -1;
        }
    }

  /* try to connect to this host */
  if (dns != NULL)
    {
      struct dns_closure *x;

      /* first resolve the hostname and then connect */
      svz_log (SVZ_LOG_NOTICE, "nut: enqueuing %s\n", dns);
      x = svz_malloc (sizeof (struct dns_closure));
      x->cfg = cfg;
      x->port = port;
      svz_coserver_dns_invoke (dns, nut_dns_done, x);
      svz_free (dns);
    }
  else
    {
      /* try connecting immediately */
      if (nut_connect_ip (cfg, ip, port))
        {
          /* if we could not connect then delete the client from host catcher
             hash and free the client structure */
          if ((client = (nut_host_t *) svz_hash_get (cfg->net, host)) != NULL)
            {
              svz_hash_delete (cfg->net, host);
              svz_free (client);
            }
          return -1;
        }
    }
  return 0;
}

/*
 * When establishing a new connection to another gnutella server this
 * functions pings it to get all further servers behind this server.
 */
int
nut_init_ping (svz_socket_t *sock)
{
  nut_config_t *cfg = sock->cfg;
  nut_client_t *client = sock->data;
  nut_packet_t *pkt;
  nut_header_t hdr;
  uint8_t *header;

  /* create new gnutella header */
  nut_calc_guid (hdr.id);
  hdr.function = NUT_PING_REQ;
  hdr.ttl = (uint8_t) cfg->ttl;
  hdr.hop = 0;
  hdr.length = 0;
  header = nut_put_header (&hdr);

  /* put into sent packet hash */
  pkt = svz_malloc (sizeof (nut_packet_t));
  pkt->sock = sock;
  pkt->sent = time (NULL);
  svz_hash_put (cfg->packet, (char *) hdr.id, pkt);

  /* update client and server statistics */
  cfg->nodes -= client->nodes;
  cfg->files -= client->files;
  cfg->size -= client->size;
  client->nodes = 0;
  client->files = 0;
  client->size = 0;

  return svz_sock_write (sock, (char *) header, SIZEOF_NUT_HEADER);
}

/*
 * The gnutella servers global initializer.
 */
int
nut_global_init (UNUSED svz_servertype_t *server)
{
#ifdef __MINGW32__
  /* try getting M$'s GUID creation routine */
  if ((oleHandle = LoadLibrary ("ole32.dll")) != NULL)
    {
      CreateGuid = (CreateGuidProc)
        GetProcAddress (oleHandle, "CoCreateGuid");
    }
#endif /* __MINGW32__ */

  /* initialize random seed */
  srand (time (NULL));

  /* initialize configuration default values */
  nut_config.search = SVZ_COLLECT_STRARRAY (nut_search_patterns);

#if 0
  /* Print structure sizes.  */
  printf ("header     : %d\n", sizeof (nut_header_t));
  printf ("ping reply : %d\n", sizeof (nut_pong_t));
  printf ("query      : %d\n", sizeof (nut_query_t));
  printf ("record     : %d\n", sizeof (nut_record_t));
  printf ("reply      : %d\n", sizeof (nut_reply_t));
  printf ("push       : %d\n", sizeof (nut_push_t));
  printf ("host       : %d\n", sizeof (nut_host_t));
  printf ("client     : %d\n", sizeof (nut_client_t));
  printf ("packet     : %d\n", sizeof (nut_packet_t));
  printf ("push reply : %d\n", sizeof (nut_push_reply_t));
  printf ("file       : %d\n", sizeof (nut_file_t));
  printf ("config     : %d\n", sizeof (nut_config_t));
  printf ("transfer   : %d\n", sizeof (nut_transfer_t));
#endif
  return 0;
}

/*
 * Gnutella spider server's instance initializer.
 */
int
nut_init (svz_server_t *server)
{
  nut_config_t *cfg = server->cfg;
  size_t n = 0;
  struct stat buf;
  char *p;

  /* check the download and share path first */
  if (strlen (cfg->save_path) == 0 || strlen (cfg->share_path) == 0)
    {
      svz_log (SVZ_LOG_ERROR, "nut: no download/share path given\n");
      return -1;
    }
  p = cfg->save_path + strlen (cfg->save_path) - 1;
  if (*p == '/' || *p == '\\')
    *p = '\0';
  p = cfg->share_path + strlen (cfg->share_path) - 1;
  if (*p == '/' || *p == '\\')
    *p = '\0';

  /* check for existence and create them if necessary */
  if (cfg->save_path[0])
    {
      if (stat (cfg->save_path, &buf) == -1)
        {
          /* create the download directory */
          if (mkdir (cfg->save_path, 0755) == -1)
            {
              svz_log_sys_error ("nut: mkdir");
              return -1;
            }
        }
      /* check if the given path is a directory already */
      else if (!S_ISDIR (buf.st_mode))
        {
          svz_log (SVZ_LOG_ERROR, "nut: %s is not a directory\n",
                   cfg->save_path);
          return -1;
        }
    }

  /* read shared files */
  nut_read_database (cfg, cfg->share_path[0] ? cfg->share_path : "/");
  svz_log (SVZ_LOG_NOTICE, "nut: %d files in database\n", cfg->db_files);

  /* calculate forced local ip and port if necessary */
  if (cfg->force_ip)
    {
      cfg->ip = inet_addr (cfg->force_ip);
    }
  cfg->port = htons (cfg->force_port);

  /* create and modify packet hash */
  cfg->packet = make_nut_kce_hash_table (svz_free);

  /* create and modify reply hash */
  cfg->reply = make_nut_kce_hash_table (NULL);

  /* create current connection hash */
  cfg->conn = svz_hash_create (4, NULL);

  /* create host catcher hash */
  cfg->net = svz_hash_create (4, svz_free);

  /* create recent query hash */
  cfg->query = svz_hash_create (4, NULL);

  /* create push request hash */
  cfg->push = svz_hash_create (4, (svz_free_func_t) nut_free_transfer);

  /* create and modify the routing table hash */
  cfg->route = make_nut_kce_hash_table (NULL);

  /* calculate this server instance's GUID */
  nut_calc_guid (cfg->guid);

  /* create detection string for gnutella host list */
  cfg->net_detect = svz_malloc (strlen (NUT_HOSTS) +
                                strlen (cfg->net_url) + 1);
  sprintf (cfg->net_detect, NUT_HOSTS, cfg->net_url);

  /* go through all given hosts and try to connect to them */
  svz_array_foreach (cfg->hosts, p, n)
    nut_connect_host (cfg, p);

  return 0;
}

/*
 * Instance finalizer.
 */
int
nut_finalize (svz_server_t *server)
{
  nut_config_t *cfg = server->cfg;

  /* destroy sharing files */
  nut_destroy_database (cfg);

  svz_hash_destroy (cfg->conn);
  svz_hash_destroy (cfg->route);
  svz_hash_destroy (cfg->query);
  svz_hash_destroy (cfg->reply);

  /* destroy sent packet hash */
  svz_hash_destroy (cfg->packet);

  /* destroy host catcher hash */
  svz_hash_destroy (cfg->net);

  /* destroy push request hash */
  svz_hash_destroy (cfg->push);

  /* free detection string */
  svz_free (cfg->net_detect);

  return 0;
}

/*
 * Global gnutella finalizer.
 */
int
nut_global_finalize (UNUSED svz_servertype_t *server)
{
#ifdef __MINGW32__
  if (oleHandle)
    FreeLibrary (oleHandle);
#endif /* __MINGW32__ */

  /* destroy confguration defaults */
  svz_array_destroy (nut_config.search);

  return 0;
}

struct dead_packet
{
  char *key;
  nut_packet_t *pkt;
};

struct disconnect_closure
{
  svz_socket_t *sock;
  struct dead_packet d;
};

static void
disconnect_internal (void *k, void *v, void *closure)
{
  char *key = k;
  nut_packet_t *pkt = v;
  struct disconnect_closure *x = closure;

  if (x->d.key)
    return;

  if (pkt->sock == x->sock)
    {
      x->d.key = key;
      x->d.pkt = pkt;
    }
}

static char *
nut_sock_client_key (svz_socket_t *sock)
{
  in_addr_t v4addr;

  svz_address_to (&v4addr, sock->remote_addr);
  return nut_client_key (v4addr, sock->remote_port);
}

/*
 * This is the sock->disconnected_socket callback for gnutella
 * connections.
 */
int
nut_disconnect (svz_socket_t *sock)
{
  nut_config_t *cfg = sock->cfg;
  nut_host_t *host;
  uint8_t *id;
  char *key;
  nut_client_t *client = sock->data;

  /* delete all push request routing information for this connection */
  while ((id = (uint8_t *) svz_hash_contains (cfg->reply, sock)) != NULL)
    svz_hash_delete (cfg->reply, (char *) id);

  /* delete all routing information for this connection */
  while ((id = (uint8_t *) svz_hash_contains (cfg->route, sock)) != NULL)
    svz_hash_delete (cfg->route, (char *) id);

  /* drop all packet information for this connection */
  if (svz_hash_size (cfg->packet))
    {
      struct disconnect_closure x = { sock, { NULL, NULL } };

      svz_hash_foreach (disconnect_internal, cfg->packet, &x);
      if (x.d.key)
        {
          svz_hash_delete (cfg->packet, x.d.key);
          svz_free (x.d.pkt);
        }
    }

  /* remove this socket from the current connection hash */
  key = nut_sock_client_key (sock);
  svz_hash_delete (cfg->conn, key);

  /* remove the connection from the host catcher */
  if ((host = svz_hash_delete (cfg->net, key)) != NULL)
    svz_free (host);

  /* free client structure */
  if (client)
    {
      cfg->nodes -= client->nodes;
      cfg->files -= client->files;
      cfg->size -= client->size;
      svz_free (client);
      sock->data = NULL;
    }

  return 0;
}

struct server_notify_closure
{
  nut_config_t *cfg;
  time_t t;
  int connect;
  svz_array_t *dead;
};

static void
server_notify_net_internal (void *k, UNUSED void *v, void *closure)
{
  char *key = k;
  struct server_notify_closure *x = closure;
  nut_config_t *cfg = x->cfg;

  if (! x->connect)
    return;

  /* Check if we are not already connected.  */
  if (NULL == svz_hash_get (cfg->conn, key)
      && -1 != nut_connect_host (cfg, key))
    x->connect--;
}

static void
server_notify_packet_internal (void *k, void *v, void *closure)
{
  char *key = k;
  nut_packet_t *pkt = v;
  struct server_notify_closure *x = closure;

  if (x->t - pkt->sent > NUT_ENTRY_AGE)
    {
      struct dead_packet *d = svz_malloc (sizeof (struct dead_packet));

      d->key = key;
      d->pkt = pkt;
      svz_array_add (x->dead, d);
    }
}

static void
server_notify_query_internal (void *k, void *v, void *closure)
{
  char *key = k;
  time_t received = (time_t) SVZ_PTR2NUM (v);
  struct server_notify_closure *x = closure;

  if (x->t - received > NUT_ENTRY_AGE)
    svz_array_add (x->dead, key);
}

/*
 * This callback is regularly called in the `server_periodic_tasks'
 * routine.  Here we try connecting to more gnutella hosts.
 */
int
nut_server_notify (svz_server_t *server)
{
  nut_config_t *cfg = server->cfg;
  static int count = NUT_CONNECT_INTERVAL;
  size_t n;
  struct server_notify_closure x;

  /* go sleep if we still do not want to do something */
  if (count-- > 0)
    return 0;

  x.cfg = cfg;

  /* do we have enough connections?  */
  x.connect = cfg->connections - svz_hash_size (cfg->conn);
  if (x.connect > 0)
    {
      /* are there hosts in the host catcher hash?  */
      if (svz_hash_size (cfg->net))
        svz_hash_foreach (server_notify_net_internal, cfg->net, &x);
    }

  /* go through the sent packet hash and drop old entries */
  if (svz_hash_size (cfg->packet))
    {
      struct dead_packet *d;

      x.t = time (NULL);
      x.dead = svz_array_create (4, svz_free);
      svz_hash_foreach (server_notify_packet_internal, cfg->packet, &x);
      svz_array_foreach (x.dead, d, n)
        {
          svz_hash_delete (cfg->packet, d->key);
          svz_free (d->pkt);
        }
      svz_array_destroy (x.dead);
    }

  /* drop older entries from the recent query hash */
  if (svz_hash_size (cfg->query))
    {
      char *key;

      x.t = time (NULL);
      x.dead = svz_array_create (4, NULL);
      svz_hash_foreach (server_notify_query_internal, cfg->query, &x);
      svz_array_foreach (x.dead, key, n)
        svz_hash_delete (cfg->query, key);
      svz_array_destroy (x.dead);
    }

  /* wake up in a certain time */
  count = NUT_CONNECT_INTERVAL;
  return 0;
}

/*
 * Whenever there is data arriving for this socket we call this
 * routine.
 */
int
nut_check_request (svz_socket_t *sock)
{
  nut_client_t *client = sock->data;
  nut_header_t *hdr;
  uint8_t *packet;
  int len = strlen (NUT_OK);
  unsigned fill = sock->recv_buffer_fill;

  /* go through all packets in the receive queue */
  while ((fill = sock->recv_buffer_fill) >= SIZEOF_NUT_HEADER)
    {
      hdr = nut_get_header ((uint8_t *) sock->recv_buffer);

      /* is there enough data to fulfill a complete packet?  */
      if (fill >= SIZEOF_NUT_HEADER + hdr->length)
        {
          len = SIZEOF_NUT_HEADER + hdr->length;
          packet = (uint8_t *) sock->recv_buffer + SIZEOF_NUT_HEADER;
          client->packets++;
#if 0
          svz_hexdump (stdout, "gnutella packet", sock->sock_desc,
                       sock->recv_buffer, len, 0);
#endif

          /* try to route the packet */
          if (nut_route (sock, hdr, packet) == 0)
            {
              /* handle the packet */
              switch (hdr->function)
                {
                case NUT_PING_REQ:
                  nut_ping (sock, hdr, NULL);
                  break;
                case NUT_PING_ACK:
                  nut_pong (sock, hdr, packet);
                  break;
                case NUT_PUSH_REQ:
                  nut_push_request (sock, hdr, packet);
                  break;
                case NUT_SEARCH_REQ:
                  nut_query (sock, hdr, packet);
                  break;
                case NUT_SEARCH_ACK:
                  nut_reply (sock, hdr, packet);
                  break;
                }
            }
          else if (!(sock->flags & SVZ_SOFLG_KILLED))
            {
              client->dropped++;
            }

          /* return if this client connection has been killed */
          if (sock->flags & SVZ_SOFLG_KILLED)
            return -1;

          /* cut this packet from the send buffer queue */
          svz_sock_reduce_recv (sock, len);
        }
      else
        break;
    }
  return 0;
}

/*
 * This routine is the sock->idle_func callback for each gnutella
 * connection.  We will regularly search for specific files.
 */
int
nut_idle_searching (svz_socket_t *sock)
{
  nut_config_t *cfg = sock->cfg;
  nut_packet_t *pkt;
  nut_header_t hdr;
  nut_query_t query;
  uint8_t *header, *search;
  char *text;

  /* search strings given?  */
  if (cfg->search && svz_array_size (cfg->search) > 0)
    {
      /* get next search string */
      if (svz_array_size (cfg->search) > cfg->search_index)
        {
          text = svz_array_get (cfg->search, cfg->search_index);
          cfg->search_index++;
        }
      else
        {
          cfg->search_index = 0;
          text = svz_array_get (cfg->search, 0);
        }

      /* create new gnutella packet */
      nut_calc_guid (hdr.id);
      hdr.function = NUT_SEARCH_REQ;
      hdr.ttl = (uint8_t) cfg->ttl;
      hdr.hop = 0;
      hdr.length = SIZEOF_NUT_QUERY + strlen (text) + 1;
      query.speed = (uint16_t) cfg->min_speed;
      header = nut_put_header (&hdr);
      search = nut_put_query (&query);

      /* try sending this packet to this connection */
      if (svz_sock_write (sock, (char *) header, SIZEOF_NUT_HEADER) == -1 ||
          svz_sock_write (sock, (char *) search, SIZEOF_NUT_QUERY) == -1 ||
          svz_sock_write (sock, text, strlen (text) + 1) == -1)
        {
          return -1;
        }

      /* save this packet for later routing */
      pkt = svz_malloc (sizeof (nut_packet_t));
      pkt->sock = sock;
      pkt->sent = time (NULL);
      svz_hash_put (cfg->packet, (char *) hdr.id, pkt);
    }

  /* wake up in a certain time */
  sock->idle_counter = NUT_SEARCH_INTERVAL;
  return 0;
}

/*
 * Gnutella server info callback.
 */
char *
nut_info_server (svz_server_t *server)
{
  nut_config_t *cfg = server->cfg;
  char bindings[256];
  static char info[80 * 19];
  char *e, *ext = NULL;
  size_t n;

  /* create file extension list */
  if (cfg->extensions)
    {
      int len = 0;
      char *w;

      svz_array_foreach (cfg->extensions, e, n)
        len += strlen (e);
      len += svz_array_size (cfg->extensions) - 1; /* ';' sep */
      len++;                                       /* '\0' */
      w = ext = svz_malloc (len);
      svz_array_foreach (cfg->extensions, e, n)
        {
          if (n)
            *w++ = ';';
          len = strlen (e);
          memcpy (w, e, len);
          w += len;
        }
      *w = '\0';
    }

  svz_pp_server_bindings (bindings, 256, server);
  sprintf (info,
           " tcp bindings    : %s\r\n"
           " force ip        : %s\r\n"
           " force port      : %s\r\n"
           " maximum ttl     : %u\r\n"
           " default ttl     : %u\r\n"
           " speed           : %u KBit/s\r\n"
           " clientID128     : %s\r\n"
           " download path   : %s\r\n"
           " share path      : %s\r\n"
           " search pattern  : %s\r\n"
           " file extensions : %s\r\n"
           " routing table   : %zu entries\r\n"
           " connected hosts : %zu/%zu\r\n"
           " sent packets    : %zu\r\n"
           " routing errors  : %u\r\n"
           " hosts           : %zu gnutella clients seen\r\n"
           " data pool       : %u MB in %u files on %u hosts\r\n"
           " database        : %u MB in %u files\r\n"
           " downloads       : %u/%u\r\n"
           " uploads         : %u/%u\r\n"
           " recent queries  : %zu",
           bindings,
           cfg->ip ? svz_inet_ntoa (cfg->ip) : "no specified",
           cfg->port ? svz_itoa (ntohs (cfg->port)) : "no specified",
           cfg->max_ttl,
           cfg->ttl,
           cfg->speed,
           nut_print_guid (cfg->guid),
           cfg->save_path,
           cfg->share_path,
           cfg->search && svz_array_size (cfg->search) > 0 ?
           cfg->search_index < svz_array_size (cfg->search) ?
           (char *) svz_array_get (cfg->search, cfg->search_index) :
           (char *) svz_array_get (cfg->search, 0) :
           "none given",
           ext ? ext : "no extensions",
           svz_hash_size (cfg->route),
           svz_hash_size (cfg->conn), cfg->connections,
           svz_hash_size (cfg->packet),
           cfg->errors,
           svz_hash_size (cfg->net),
           cfg->size / 1024, cfg->files, cfg->nodes,
           cfg->db_size / 1024 / 1024, cfg->db_files,
           cfg->dnloads, cfg->max_dnloads,
           cfg->uploads, cfg->max_uploads,
           svz_hash_size (cfg->query));

  svz_free (ext);
  return info;
}

/*
 * Gnutella client info callback.
 */
char *
nut_info_client (UNUSED svz_server_t *server, svz_socket_t *sock)
{
#define INFO_SIZE  80 * 3
  static char info[INFO_SIZE];
  nut_transfer_t *transfer = sock->data;
  nut_client_t *client = sock->data;
  unsigned current, all, elapsed;
  char *crlf = "\r\n";
  int avail = INFO_SIZE;
  char *w = info;

#define MORE(fmt,...)  do                               \
    {                                                   \
      int one = snprintf (w, avail, fmt, __VA_ARGS__);  \
                                                        \
      avail -= one;                                     \
      w += one;                                         \
    }                                                   \
  while (0)

  MORE ("This is a gnutella spider client.%s%s", crlf, crlf);

  /* normal gnutella host */
  if (sock->userflags & NUT_FLAG_CLIENT)
    MORE ("  * usual gnutella host%s"
          "  * dropped packets : %u/%u%s"
          "  * invalid packets : %u%s"
          "  * data pool       : %u MB"
          " in %u files on %u hosts%s",
          crlf,
          client->dropped, client->packets, crlf,
          client->invalid, crlf,
          client->size / 1024,
          client->files, client->nodes, crlf);

  /* file upload and download */
  if (sock->userflags & (NUT_FLAG_UPLOAD | NUT_FLAG_DNLOAD))
    {
      current = transfer->original_size - transfer->size;
      all = transfer->original_size;
      elapsed = time (NULL) - transfer->start;
      if (!all)
        all++;
      if (!elapsed)
        elapsed++;
      MORE ("  * file : %s%s"
            "  * %s progress : %u/%u - %u.%u%% - %u.%u kb/sec%s",
            transfer->file, crlf,
            (sock->userflags & NUT_FLAG_DNLOAD
             ? "download"
             : "upload"),
            current, all,
            current * 100 / all, (current * 1000 / all) % 10,
            current / 1024 / elapsed,
            (current * 10 / 1024 / elapsed) % 10, crlf);
    }

  /* http header received?  */
  if (sock->userflags & NUT_FLAG_HDR)
    MORE ("  * header received%s", crlf);

  /* host list */
  if (sock->userflags & NUT_FLAG_HOSTS)
    MORE (w, avail, "  * sending host catcher list%s", crlf);

  *w = '\0';
  return info;
#undef MORE
#undef INFO_SIZE
}

/*
 * This is the protocol detection routine for self connected gnutella
 * hosts.  It is used for normal gnutella network connections and
 * push requests (download).
 */
int
nut_detect_connect (svz_socket_t *sock)
{
  nut_config_t *cfg = sock->cfg;
  int len = strlen (NUT_OK);

  /* check for self connected response of normal gnutella host */
  if (sock->recv_buffer_fill >= len &&
      !memcmp (sock->recv_buffer, NUT_OK, len))
    {
      char buf[64];

      sock->userflags |= (NUT_FLAG_CLIENT | NUT_FLAG_SELF);
      svz_log (SVZ_LOG_NOTICE, "nut: host %s connected\n",
               SVZ_PP_ADDR_PORT (buf, sock->remote_addr, sock->remote_port));
      svz_sock_reduce_recv (sock, len);

      if (nut_connect_socket (svz_server_find (cfg), sock) == -1)
        return -1;
      return sock->check_request (sock);
    }

  return 0;
}

/*
 * Incoming connections will be protocol checked.
 */
int
nut_detect_proto (svz_server_t *server, svz_socket_t *sock)
{
  nut_config_t *cfg = server->cfg;
  int len = strlen (NUT_CONNECT);

  /* detect normal connect (not if listener is disabled) */
  if (!cfg->disable)
    {
      len = strlen (NUT_CONNECT);
      if (sock->recv_buffer_fill >= len &&
          !memcmp (sock->recv_buffer, NUT_CONNECT, len))
        {
          sock->userflags |= NUT_FLAG_CLIENT;
          svz_log (SVZ_LOG_NOTICE, "gnutella protocol detected (client)\n");
          svz_sock_reduce_recv (sock, len);
          return -1;
        }
    }

  /* detect upload request */
  len = strlen (NUT_GET);
  if (sock->recv_buffer_fill >= len &&
      !memcmp (sock->recv_buffer, NUT_GET, len))
    {
      sock->userflags |= NUT_FLAG_UPLOAD;
      svz_log (SVZ_LOG_NOTICE, "gnutella protocol detected (upload)\n");
      return -1;
    }

  /* detect host catcher request */
  len = strlen (cfg->net_detect);
  if (sock->recv_buffer_fill >= len &&
      !memcmp (sock->recv_buffer, cfg->net_detect, len))
    {
      sock->userflags |= NUT_FLAG_HOSTS;
      svz_log (SVZ_LOG_NOTICE, "gnutella protocol detected (host list)\n");
      svz_sock_reduce_recv (sock, len);
      return -1;
    }

  /* check for push request reply */
  len = strlen (NUT_GIVE);
  if (sock->recv_buffer_fill >= len &&
      !memcmp (sock->recv_buffer, NUT_GIVE, len))
    {
      sock->userflags |= NUT_FLAG_GIVEN;
      svz_log (SVZ_LOG_NOTICE, "gnutella protocol detected (giving)\n");
      return -1;
    }

  return 0;
}

/*
 * This routine will be called when the detection routine return
 * success.
 */
int
nut_connect_socket (svz_server_t *server, svz_socket_t *sock)
{
  nut_config_t *cfg = server->cfg;

  /* assign download callbacks */
  if (sock->userflags & NUT_FLAG_GIVEN)
    {
      sock->check_request = nut_check_given;
      return 0;
    }

  /* assign host catcher request routines */
  if (sock->userflags & NUT_FLAG_HOSTS)
    {
      sock->check_request = nut_hosts_check;
      sock->write_socket = nut_hosts_write;
      svz_sock_resize_buffers (sock, NUT_SEND_BUFSIZE, sock->recv_buffer_size);
      return 0;
    }

  /* assign upload request routines */
  if (sock->userflags & NUT_FLAG_UPLOAD)
    {
      if (cfg->uploads <= cfg->max_uploads)
        {
          sock->check_request = nut_check_upload;
          return 0;
        }
      return -1;
    }

  /* assign normal gnutella request routines */
  if (sock->userflags & NUT_FLAG_CLIENT)
    {
      /* check if we got enough clients already */
      if (svz_hash_size (cfg->conn) > cfg->connections)
        return -1;

      /* send the first reply if necessary */
      if (!(sock->userflags & NUT_FLAG_SELF))
        if (svz_sock_printf (sock, NUT_OK) == -1)
          return -1;

      /* assign gnutella specific callbacks */
      sock->flags |= SVZ_SOFLG_NOFLOOD;
      sock->disconnected_socket = nut_disconnect;
      sock->check_request = nut_check_request;
      sock->idle_func = nut_idle_searching;
      sock->idle_counter = NUT_SEARCH_INTERVAL;
      sock->data = nut_create_client ();

      /* send initial ping */
      if (nut_init_ping (sock) == -1)
        return -1;

      /* put this client to the current connection hash */
      svz_hash_put (cfg->conn, nut_sock_client_key (sock), sock);

      return 0;
    }

  return -1;
}
