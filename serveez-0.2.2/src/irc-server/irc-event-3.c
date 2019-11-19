/*
 * irc-event-3.c - IRC events -- Server queries and commands
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
#include <string.h>
#include <time.h>
#include <errno.h>
#include "networking-headers.h"
#include "libserveez.h"
#include "irc-core/irc-core.h"
#include "irc-proto.h"
#include "irc-event.h"
#include "irc-server.h"
#include "unused.h"

/*
 *         Command: ADMIN
 *      Parameters: [<server>]
 * Numeric Replies: ERR_NOSUCHSERVER
 *                  RPL_ADMINME       RPL_ADMINLOC1
 *                  RPL_ADMINLOC2     RPL_ADMINEMAIL
 */
int
irc_admin_callback (svz_socket_t *sock,
                    irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;

  /* server para is given */
  if (request->paras > 1)
    {
      /* check if the server para is valid */
      if (strcmp (request->para[1], cfg->host))
        {
          irc_printf (sock, ":%s %03d %s " ERR_NOSUCHSERVER_TEXT "\n",
                      cfg->host, ERR_NOSUCHSERVER, client->nick,
                      request->para[1]);
          return 0;
        }
    }

  irc_printf (sock, ":%s %03d %s " RPL_ADMINME_TEXT "\n",
              cfg->host, RPL_ADMINME, client->nick,
              cfg->host, cfg->admininfo);
  irc_printf (sock, ":%s %03d %s " RPL_ADMINLOC1_TEXT "\n",
              cfg->host, RPL_ADMINLOC1, client->nick, cfg->location1);
  irc_printf (sock, ":%s %03d %s " RPL_ADMINLOC2_TEXT "\n",
              cfg->host, RPL_ADMINLOC2, client->nick, cfg->location2);
  irc_printf (sock, ":%s %03d %s " RPL_ADMINEMAIL_TEXT "\n",
              cfg->host, RPL_ADMINEMAIL, client->nick, cfg->email);

  return 0;
}
/*
 *         Command: TIME
 *      Parameters: [<server>]
 * Numeric Replies: ERR_NOSUCHSERVER RPL_TIME
 */
int
irc_time_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;

  /* server para is given */
  if (request->paras > 1)
    {
      /* check if the server para is valid */
      if (strcmp (request->para[1], cfg->host))
        {
          irc_printf (sock, ":%s %03d %s " ERR_NOSUCHSERVER_TEXT "\n",
                      cfg->host, ERR_NOSUCHSERVER, client->nick,
                      request->para[1]);
          return 0;
        }
    }

  /* reply the local time */
  irc_printf (sock, ":%s %03d %s " RPL_TIME_TEXT "\n",
              cfg->host, RPL_TIME, client->nick,
              cfg->host, svz_time (time (NULL)));

  return 0;
}

/*
 *         Command: LUSERS
 *      Parameters:
 * Numeric Replies: RPL_LUSERCLIENT  RPL_LUSEROP
 *                  RPL_LUSERUNKNOWN RPL_LUSERCHANNELS
 *                  RPL_LUSERME
 */
int
irc_lusers_callback (svz_socket_t *sock,
                     irc_client_t *client,
                     UNUSED irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;

  /* send LUSER* replies */
  irc_printf (sock, ":%s %03d %s " RPL_LUSERCLIENT_TEXT "\n",
              cfg->host, RPL_LUSERCLIENT, client->nick,
              cfg->users, cfg->invisibles, irc_count_servers (cfg));

  irc_printf (sock, ":%s %03d %s " RPL_LUSEROP_TEXT "\n",
              cfg->host, RPL_LUSEROP, client->nick, cfg->operators);

  /* This will end up in a non standard welcome message !
  irc_printf (sock, ":%s %03d %s " RPL_LUSERUNKNOWN_TEXT "\n",
  cfg->host, RPL_LUSERUNKNOWN, client->nick,
  cfg->unknowns);
  */

  irc_printf (sock, ":%s %03d %s " RPL_LUSERCHANNELS_TEXT "\n",
              cfg->host, RPL_LUSERCHANNELS, client->nick,
              svz_hash_size (cfg->channels));

  irc_printf (sock, ":%s %03d %s " RPL_LUSERME_TEXT "\n",
              cfg->host, RPL_LUSERME, client->nick,
              svz_hash_size (cfg->clients), irc_count_servers (cfg));

  return 0;
}

/*
 *         Command: STATS
 *      Parameters: [<query> [<server>]]
 * Numeric Replies: ERR_NOSUCHSERVER
 *                  RPL_STATSCLINE     RPL_STATSNLINE
 *                  RPL_STATSILINE     RPL_STATSKLINE
 *                  RPL_STATSQLINE     RPL_STATSLLINE
 *                  RPL_STATSLINKINFO  RPL_STATSUPTIME
 *                  RPL_STATSCOMMANDS  RPL_STATSOLINE
 *                  RPL_STATSHLINE     RPL_ENDOFSTATS
 */
int
irc_stats_callback (svz_socket_t *sock,
                    irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  char stat;
  time_t t, sec, hour, min, day;
  int n;
  irc_server_t *server;
  irc_user_t *user;
  irc_class_t *class;
  irc_oper_t *oper;
  irc_kill_t *kill;

  /* no paras given */
  if (!request->paras)
    {
      irc_printf (sock, ":%s %03d %s " RPL_ENDOFSTATS_TEXT "\n",
                  cfg->host, RPL_ENDOFSTATS, client->nick, ' ');
      return 0;
    }

  /* server para is given */
  if (request->paras > 1)
    {
      /* check if the server para is valid */
      if (strcmp (request->para[1], cfg->host))
        {
          irc_printf (sock, ":%s %03d %s " ERR_NOSUCHSERVER_TEXT "\n",
                      cfg->host, ERR_NOSUCHSERVER, client->nick,
                      request->para[1]);
          return 0;
        }
    }

  stat = request->para[0][0];
  switch (stat)
    {
      /*
       * c - servers which the server may connect to or allow
       *     connections from
       */
    case 'c':
      for (server = cfg->servers; server; server = server->next)
        {
          /* actively connecting */
          if (server->connect)
            {
              irc_printf (sock, ":%s %03d %s " RPL_STATSCLINE_TEXT "\n",
                          cfg->host, RPL_STATSCLINE, client->nick,
                          server->realhost, server->host, server->port,
                          server->class);
            }
          /* is allowed to connect */
          else
            {
              irc_printf (sock, ":%s %03d %s " RPL_STATSNLINE_TEXT "\n",
                          cfg->host, RPL_STATSNLINE, client->nick,
                          server->realhost, server->host, server->port,
                          server->class);
            }
        }
      break;
      /*
       * h - returns a list of servers which are either forced to be
       *     treated as leaves or allowed to act as hubs
       */
    case 'h':
      break;
      /*
       * i - returns a list of hosts which the server allows a client
       *     to connect from
       */
    case 'i':
      for (user = cfg->user_auth; user; user = user->next)
        {
          irc_printf (sock, ":%s %03d %s " RPL_STATSILINE_TEXT "\n",
                      cfg->host, RPL_STATSILINE, client->nick,
                      user->user_ip,
                      user->ip ? "@" : "", user->ip ? user->ip : "",
                      user->user_host,
                      user->host ? "@" : "", user->host ? user->host : "",
                      cfg->port, user->class);
        }
      break;
      /*
       * k - returns a list of banned username/hostname combinations
       *     for that server
       */
    case 'k':
      for (kill = cfg->banned; kill; kill = kill->next)
        {
          irc_printf (sock, ":%s %03d %s "  RPL_STATSKLINE_TEXT "\n",
                      cfg->host,  RPL_STATSKLINE, client->nick,
                      kill->host, kill->user, cfg->port, 0);
        }
      break;
      /*
       * l - returns a list of the server's connections, showing how
       *     long each connection has been established and the traffic
       *     over that connection in bytes and messages for each
       *     direction
       */
    case 'l':
      break;
      /*
       * m - returns a list of commands supported by the server and
       *     the usage count for each if the usage count is non zero
       */
    case 'm':
      for (n = 0; irc_callback[n].func; n++)
        {
          if (irc_callback[n].count)
            {
              irc_printf (sock, ":%s %03d %s " RPL_STATSCOMMANDS_TEXT "\n",
                          cfg->host, RPL_STATSCOMMANDS, client->nick,
                          irc_callback[n].request, irc_callback[n].count);
            }
        }
      break;
      /*
       * o - returns a list of hosts from which normal clients may
       *     become operators
       */
    case 'o':
      for (oper = cfg->operator_auth; oper; oper = oper->next)
        {
          irc_printf (sock, ":%s %03d %s " RPL_STATSOLINE_TEXT "\n",
                      cfg->host, RPL_STATSOLINE, client->nick,
                      oper->host, oper->user);
        }
      break;
      /*
       * y - show Y (Class) lines from server's configuration file
       */
    case 'y':
      for (class = cfg->classes; class; class = class->next)
        {
          irc_printf (sock, ":%s %03d %s " RPL_STATSYLINE_TEXT "\n",
                      cfg->host, RPL_STATSYLINE, client->nick,
                      class->nr, class->ping_freq, class->connect_freq,
                      class->sendq_size);
        }
      break;
      /*
       * u - returns a string showing how long the server has been up
       */
    case 'u':
      t = svz_uptime ();
      sec = t % 60;
      t /= sec;
      min = t % 60;
      t /= 60;
      hour = t % 24;
      t /= 24;
      day = t % 24;
      irc_printf (sock, ":%s %03d %s " RPL_STATSUPTIME_TEXT "\n",
                  cfg->host, RPL_STATSUPTIME, client->nick,
                  day, hour, min, sec);
      break;
    default:
      break;
    }

  irc_printf (sock, ":%s %03d %s " RPL_ENDOFSTATS_TEXT "\n",
              cfg->host, RPL_ENDOFSTATS, client->nick, stat);
  return 0;
}

/*
 *         Command: VERSION
 *      Parameters: [<server>]
 * Numeric Replies: ERR_NOSUCHSERVER RPL_VERSION
 */
int
irc_version_callback (svz_socket_t *sock,
                      irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;

  /* no paras */
  if (!request->paras)
    {
      irc_printf (sock, ":%s %03d %s " RPL_VERSION_TEXT "\n",
                  cfg->host, RPL_VERSION, client->nick,
                  PACKAGE_VERSION, PACKAGE_TARNAME);
    }
  return 0;
}

/*
 *         Command: INFO
 *      Parameters: [<server>]
 * Numeric Replies: ERR_NOSUCHSERVER
 *                  RPL_INFO         RPL_ENDOFINFO
 */
int
irc_info_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  char *text;
  FILE *f;

  if (!request->paras)
    {
      if ((f = fopen (cfg->info_file, "r")) == NULL)
        {
          svz_log_sys_error ("irc: /INFO error");
          irc_printf (sock, ":%s %03d %s " ERR_FILEERROR_TEXT "\n",
                      cfg->host, ERR_FILEERROR, client->nick,
                      "open", cfg->info_file);
          return 0;
        }

      /* read every line (restrict line length) */
      text = svz_malloc (MOTD_LINE_LEN);
      while (fgets (text, MOTD_LINE_LEN, f) != NULL)
        {
          irc_printf (sock, ":%s %03d %s " RPL_INFO_TEXT "\n",
                      cfg->host, RPL_INFO, client->nick, text);
        }
      svz_free (text);
      fclose (f);

      irc_printf (sock, ":%s %03d %s " RPL_ENDOFINFO_TEXT "\n",
                  cfg->host, RPL_ENDOFINFO, client->nick, text);
    }
  return 0;
}
