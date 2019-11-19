/*
 * irc-server.c - IRC server connection routines
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
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
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
# include <netdb.h>
#endif
#include "networking-headers.h"
#include "libserveez.h"
#include "irc-proto.h"
#include "irc-event.h"
#include "irc-server.h"
#include "unused.h"

#define DEFAULT_PORT 6667

irc_server_t *irc_server_list;  /* server list root */

#define MAX_HOST_LEN 256
#define MAX_PASS_LEN 256

/*
 * Parse one of the config lines in the IRC configuration.
 * This function has exactly the same syntax as ‘sscanf’ but
 * recognizes only %s and %d for string and integers.  Strings
 * will be parsed until the next character in the format string.
 * Another difference is that each %s in the format string
 * consumes two args, not just one.  The first in the pair is
 * an integer BUFSIZE that limits the scan to BUFSIZE - 1 bytes.
 */
int
irc_parse_line (char *line, char *fmt, ...)
{
  va_list args;
  int *i;
  char *s;
  int ret;

  va_start (args, fmt);
  ret = 0;

  while (*fmt && *line)
    {
      /* next arg */
      if (*fmt == '%')
        {
          /* check if this is a valid format identifier */
          if (!*++fmt)
            break;

          /* a decimal */
          if (*fmt == 'd')
            {
              i = va_arg (args, int *);
              *i = 0;
              fmt++;
              while (*line && *line >= '0' && *line <= '9')
                {
                  *i *= 10;
                  *i += (*line - '0');
                  line++;
                }
            }
          /* a string */
          else if (*fmt == 's')
            {
              size_t avail = va_arg (args, size_t);

              s = va_arg (args, char *);
              fmt++;
              while (*line && *line != *fmt && --avail)
                {
                  *s++ = *line++;
                }
              *s = 0;
            }
          ret++;
        }
      /* not an arg */
      else if (*fmt != *line)
        {
          break;
        }
      if (*fmt)
        fmt++;
      if (*line)
        line++;
    }

  va_end (args);
  return ret;
}

static void
dns_done_cl_internal (UNUSED void *k, void *v, void *closure)
{
  irc_client_t *cl = v;
  svz_socket_t *sock = closure;

#if ENABLE_TIMESTAMP
  irc_printf (sock, "NICK %s %d %d %s %s %s %s :%s\n",
              cl->nick, cl->hopcount, cl->since,
              irc_client_flag_string (cl),
              cl->user, cl->host, cl->server, "EFNet?");
#else /* not ENABLE_TIMESTAMP */
  irc_printf (sock, "NICK %s\n", cl->nick);
  irc_printf (sock, "USER %s %s %s %s\n",
              cl->user, cl->host, cl->server, cl->real);
  irc_printf (sock, "MODE %s %s\n",
              cl->nick, irc_client_flag_string (cl));
#endif /* not ENABLE_TIMESTAMP */
}

static void
dns_done_ch_internal (UNUSED void *k, void *v, void *closure)
{
  int n;
  irc_channel_t *ch = v;
  char *fs = irc_channel_flag_string (ch);
  svz_socket_t *sock = closure;

#if ENABLE_TIMESTAMP
  /* FIXME: Avoid ‘strcat’; honor ‘MAX_MSG_LEN’.  */
  char nicklist[MAX_MSG_LEN];

  /* create nick list */
  nicklist[0] = '\0';
  for (n = 0; n < ch->clients; n++)
    {
      if (ch->cflag[n] & MODE_OPERATOR)
        strcat (nicklist, "@");
      else if (ch->cflag[n] & MODE_VOICE)
        strcat (nicklist, "+");
      strcat (nicklist, ch->client[n]->nick);
      strcat (nicklist, " ");
    }
  /* propagate one channel in one request */
  irc_printf (sock, "SJOIN %d %s %s :%s\n",
              ch->since, ch->name, fs, nicklist);
#else /* not ENABLE_TIMESTAMP */
  for (n = 0; n < ch->clients; n++)
    irc_printf (sock, ":%s JOIN %s\n", ch->client[n], ch->name);
  irc_printf (sock, "MODE %s %s\n", ch->name, fs);
#endif /* not ENABLE_TIMESTAMP */
}

/*
 * This will be called if a DNS lookup for a remote irc server has
 * been done.  Here we connect to this server then.  Return non-zero on
 * errors.
 */
static int
irc_dns_done (char *ip, void *closure)
{
  irc_server_t *server = closure;
  irc_config_t *cfg = server->cfg;
  svz_socket_t *sock;
  svz_address_t *addr;

  /* check if dns lookup was successful */
  if (!ip)
    {
      svz_log (SVZ_LOG_ERROR, "irc: cannot connect to %s\n", server->realhost);
      return -1;
    }

  /* try connecting */
  server->addr = inet_addr (ip);
  addr = svz_address_make (AF_INET, &server->addr);
  sock = svz_tcp_connect (addr, server->port);
  svz_free (addr);
  if (sock == NULL)
    {
      return -1;
    }

  svz_log (SVZ_LOG_NOTICE, "irc: connecting to %s\n", server->realhost);
  sock->data = server;
  sock->cfg = cfg;
  server->id = sock->id;
  server->connected = 1;
  sock->userflags |= IRC_FLAG_SERVER;
  sock->check_request = irc_check_request;

  /* send initial requests introducing this server */
#ifndef ENABLE_TIMESTAMP
  irc_printf (sock, "PASS %s\n", server->pass);
#else /* ENABLE_TIMESTAMP */
  irc_printf (sock, "PASS %s %s\n", server->pass, TS_PASS);
#endif /* ENABLE_TIMESTAMP */
  irc_printf (sock, "SERVER %s 1 :%s\n", cfg->host, cfg->info);

#if ENABLE_TIMESTAMP
  irc_printf (sock, "SVINFO %d %d %d :%d\n",
              TS_CURRENT, TS_MIN, 0, time (NULL) + cfg->tsdelta);
#endif /* ENABLE_TIMESTAMP */

  /* now propagate user information to this server */
  if (svz_hash_size (cfg->clients))
    svz_hash_foreach (dns_done_cl_internal, cfg->clients, sock);

  /* propagate all channel information to the server */
  if (svz_hash_size (cfg->channels))
    svz_hash_foreach (dns_done_ch_internal, cfg->channels, sock);

  return 0;
}

/*
 * Add an IRC server to the server list.
 */
static irc_server_t *
irc_add_server (irc_config_t *cfg, irc_server_t *server)
{
  server->next = cfg->servers;
  cfg->servers = server;

  return cfg->servers;
}

/*
 * Go through all C lines in the IRC server configuration
 * and resolve all hosts.
 */
void
irc_connect_servers (irc_config_t *cfg)
{
  char realhost[MAX_HOST_LEN];
  char pass[MAX_PASS_LEN];
  char host[MAX_NAME_LEN];
  int class, port;
  irc_server_t *ircserver;
  char *cline;
  size_t n;

  /* any C lines at all?  */
  if (!cfg->CLine)
    return;

  /* go through all C lines */
  svz_array_foreach (cfg->CLine, cline, n)
    {
      /* scan the actual C line */
      irc_parse_line (cline, "C:%s:%s:%s:%d:%d",
                      MAX_HOST_LEN, realhost,
                      MAX_PASS_LEN, pass,
                      MAX_NAME_LEN, host,
                      &port, &class);

      /* create new IRC server structure */
      ircserver = svz_malloc (sizeof (irc_server_t));
      ircserver->port = htons (port);
      ircserver->class = class;
      ircserver->id = -1;
      ircserver->realhost = svz_malloc (strlen (realhost) + 1);
      strcpy (ircserver->realhost, realhost);
      ircserver->host = svz_malloc (strlen (host) + 1);
      strcpy (ircserver->host, host);
      ircserver->pass = svz_malloc (strlen (pass) + 1);
      strcpy (ircserver->pass, pass);
      ircserver->cfg = cfg;
      ircserver->next = NULL;
      ircserver->connected = 0;
      ircserver->connect = 1;

      /* add this server to the server list */
      svz_log (SVZ_LOG_NOTICE, "irc: enqueuing %s\n", ircserver->realhost);
      irc_add_server (cfg, ircserver);
      svz_coserver_dns_invoke (realhost, irc_dns_done, ircserver);
    }
}

/*
 * Delete an IRC server of the current list.
 */
static void
irc_del_server (irc_config_t *cfg, irc_server_t *server)
{
  irc_server_t *srv;
  irc_server_t *prev;

  prev = srv = cfg->servers;
  while (srv)
    {
      if (srv == server)
        {
          svz_free (server->realhost);
          svz_free (server->host);
          svz_free (server->pass);
          if (prev == srv)
            cfg->servers = server->next;
          else
            prev->next = server->next;
          svz_free (server);
          return;
        }
      prev = srv;
      srv = srv->next;
    }
}

/*
 * Delete all IRC servers.
 */
void
irc_delete_servers (irc_config_t *cfg)
{
  while (cfg->servers)
    {
      irc_del_server (cfg, cfg->servers);
    }
}

/*
 * Count the amount of currently connected IRC servers.
 */
int
irc_count_servers (irc_config_t *cfg)
{
  irc_server_t *server;
  int n = 0;

  for (server = cfg->servers; server; server = server->next)
    {
      if (server->connected)
        n++;
    }

  return n;
}

/*
 * Find an IRC server in the current list.
 */
irc_server_t *
irc_find_server (void)
{
  return NULL;
}
