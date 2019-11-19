/*
 * irc-event-7.c - IRC events -- OPTIONAL MESSAGES
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
#include <stdlib.h>
#include "networking-headers.h"
#include <libserveez.h>
#include "irc-core/irc-core.h"
#include "irc-proto.h"
#include "irc-event.h"
#include "unused.h"

/*
 *         Command: ISON
 *      Parameters: <nickname>{<space><nickname>}
 * Numeric Replies: RPL_ISON ERR_NEEDMOREPARAMS
 */
int
irc_ison_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  static char nicklist[MAX_MSG_LEN] = "";
  int n;

  /* do you have enough para's?  */
  if (irc_check_args (sock, client, cfg, request, 1))
    return 0;

  for (n = 0; n < request->paras; n++)
    {
      if (irc_find_nick (cfg, request->para[n]))
        {
          strcat (nicklist, request->para[n]);
          strcat (nicklist, " ");
        }
    }

  irc_printf (sock, ":%s %03d %s " RPL_ISON_TEXT "\n",
              cfg->host, RPL_ISON, client->nick, nicklist);
  return 0;
}

/*
 *         Command: USERHOST
 *      Parameters: <nickname>{<space><nickname>}
 * Numeric Replies: RPL_USERHOST ERR_NEEDMOREPARAMS
 */
int
irc_userhost_callback (svz_socket_t *sock,
                       irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  int n;
  irc_client_t *cl;
  char list[MAX_MSG_LEN] = ":";
  char text[MAX_MSG_LEN];

  /* complete parameter list?  */
  if (irc_check_args (sock, client, cfg, request, 1))
    return 0;

  /* go through all paras */
  for (n = 0; n < request->paras; n++)
    {
      if ((cl = irc_find_nick (cfg, request->para[n])) != NULL)
        {
          sprintf (text, "%s%s=%c%s@%s ",
                   cl->nick,
                   cl->flag & UMODE_OPERATOR ? "*" : "",
                   cl->flag & UMODE_AWAY ? '-' : '+', cl->user, cl->host);
          strcat (list, text);
        }
    }

  /* send the USERHOST reply */
  irc_printf (sock, ":%s %03d %s " RPL_USERHOST_TEXT "\n",
              cfg->host, RPL_USERHOST, client->nick, list);

  return 0;
}

/*
 *         Command: AWAY
 *      Parameters: [message]
 * Numeric Replies: RPL_UNAWAY RPL_NOWAWAY
 */
int
irc_away_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;

  /* this is UNAWAY */
  if (!request->paras)
    {
      irc_printf (sock, ":%s %03d %s " RPL_UNAWAY_TEXT "\n",
                  cfg->host, RPL_UNAWAY, client->nick);
      client->flag &= ~UMODE_AWAY;
    }
  /* set AWAY Message */
  else
    {
      irc_printf (sock, ":%s %03d %s " RPL_NOWAWAY_TEXT "\n",
                  cfg->host, RPL_NOWAWAY, client->nick);
      client->flag |= UMODE_AWAY;
      if (client->away)
        svz_free (client->away);
      client->away = svz_strdup (request->para[0]);
    }
  return 0;
}

struct users_cb_closure
{
  svz_socket_t *sock;
  irc_client_t *client;
  irc_config_t *cfg;
};

static void
users_cb_internal (UNUSED void *k, void *v, void *closure)
{
  irc_client_t *cl = v;
  struct users_cb_closure *x = closure;

  irc_printf (x->sock, ":%s %03d %s " RPL_USERS_TEXT "\n",
              x->cfg->host, RPL_USERS, x->client->nick,
              cl->nick, cl->user, cl->host);
}

/*
 *         Command: USERS
 *      Parameters: [<server>]
 * Numeric Replies: ERR_NOSUCHSERVER  ERR_FILEERROR
 *                  RPL_USERSSTART    RPL_USERS
 *                  RPL_NOUSERS       RPL_ENDOFUSERS
 *                  ERR_USERSDISABLED
 */
int
irc_users_callback (svz_socket_t *sock,
                    irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;

  /* Return a messages saying this feature has been disabled.  */
  if (cfg->users_disabled)
    {
      irc_printf (sock, ":%s %03d %s " ERR_USERSDISABLED_TEXT "\n",
                  cfg->host, ERR_USERSDISABLED, client->nick);
      return 0;
    }

  /*
   * If no parameter is given then return the local users
   * list of this server.
   */
  if (request->paras < 1)
    {
      if (svz_hash_size (cfg->clients))
        {
          struct users_cb_closure x = { sock, client, cfg };

          irc_printf (sock, ":%s %03d %s " RPL_USERSSTART_TEXT "\n",
                      cfg->host, RPL_USERSSTART, client->nick);
          svz_hash_foreach (users_cb_internal, cfg->clients, &x);
          irc_printf (sock, ":%s %03d %s " RPL_ENDOFUSERS_TEXT "\n",
                      cfg->host, RPL_ENDOFUSERS, client->nick);
        }
      else
        {
          irc_printf (sock, "%s %03d %s " RPL_NOUSERS_TEXT "\n",
                      cfg->host, RPL_NOUSERS, client->nick);
        }

    }
  /* Return the list of remote servers if possible.  */
  else
    {
      irc_printf (sock, ":%s %03d %s " ERR_NOSUCHSERVER_TEXT "\n",
                  cfg->host, ERR_NOSUCHSERVER, client->nick,
                  request->para[0]);
    }

  return 0;
}
