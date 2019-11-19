/*
 * irc-event-5.c - IRC events -- User-based queries
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
#include <time.h>
#include "networking-headers.h"
#include <libserveez.h>
#include "irc-core/irc-core.h"
#include "irc-proto.h"
#include "irc-event.h"
#include "unused.h"

/*         Command: WHOWAS
 *      Parameters: <nickname> [<count> [<server>]]
 * Numeric Replies: ERR_NONICKNAMEGIVEN ERR_WASNOSUCHNICK
 *                  RPL_WHOWASUSER      RPL_WHOISSERVER
 *                  RPL_ENDOFWHOWAS
 *
 * [NB: <server> handling not implemented. --ttn]
 */
int
irc_whowas_callback (svz_socket_t *sock,
                     irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_history_t *cl;
  char *nick;
  int n, i, found;

  /* check if there is a nick given */
  if (request->paras < 1)
    {
      irc_printf (sock, ":%s %03d %s " ERR_NONICKNAMEGIVEN_TEXT "\n",
                  cfg->host, ERR_NONICKNAMEGIVEN, client->nick);
    }

  nick = request->para[0];
  n = atoi (request->para[1]);

  /* look through the history list */
  cl = NULL;
  i = 0;
  found = 0;
  while ((cl = irc_find_nick_history (cfg, cl, nick)) != NULL &&
         (i < n || n <= 0))
    {
      found = 1;
      irc_printf (sock, ":%s %03d %s " RPL_WHOWASUSER_TEXT "\n",
                  cfg->host, RPL_WHOWASUSER, client->nick,
                  cl->nick, cl->user, cl->host, cl->real);
      i++;
    }
  /* did you found a nick in the history?  */
  if (found)
    {
      irc_printf (sock, ":%s %03d %s " RPL_ENDOFWHOWAS_TEXT "\n",
                  cfg->host, RPL_ENDOFWHOWAS, client->nick, nick);
    }
  else
    {
      irc_printf (sock, ":%s %03d %s " ERR_WASNOSUCHNICK_TEXT "\n",
                  cfg->host, ERR_WASNOSUCHNICK, client->nick, nick);
    }

  return 0;
}

/*
 * Check if a certain client is visible to another.
 */
static int
irc_client_visible (UNUSED irc_config_t *cfg,
                    irc_client_t *client,  /* who wants to know about */
                    irc_client_t *rclient) /* this client */
{
  irc_channel_t *channel;
  int n;

  /* invisible?  */
  if (rclient->flag & UMODE_INVISIBLE)
    return 0;

  /*
   * Visible, but not in a public (no secret or private) channel ?
   * The client is also visible if they share a channel.
   */
  for (n = 0; n < rclient->channels; n++)
    {
      channel = rclient->channel[n];

      /* public channel?  */
      if (!(channel->flag & (MODE_SECRET | MODE_PRIVATE)))
        return -1;
      else
        {
          /* no, but do they share it?  */
          if (irc_client_in_channel (NULL, client, channel) != -1)
            return -1;
        }
    }
  return -1;
}

/*
 * Send a user WHOIS reply.
 */
static void
irc_user_info (svz_socket_t *sock,   /* the socket for the client to send */
               irc_client_t *client, /* reply client */
               irc_client_t *cl)     /* info about this client */
{
  irc_config_t *cfg = sock->cfg;
  irc_channel_t *channel;
  svz_socket_t *xsock;
  char text[MAX_MSG_LEN];
  int n, i;

  if (!irc_client_visible (cfg, client, cl))
    return;

  irc_printf (sock, ":%s %03d %s " RPL_WHOISUSER_TEXT "\n",
              cfg->host, RPL_WHOISUSER, client->nick,
              cl->nick, cl->user, cl->host, cl->real);

  irc_printf (sock, ":%s %03d %s " RPL_WHOISSERVER_TEXT "\n",
              cfg->host, RPL_WHOISSERVER, client->nick,
              cl->nick, cl->server, cfg->info);

  /* operator?  */
  if (cl->flag & UMODE_OPERATOR)
    irc_printf (sock, ":%s %03d %s " RPL_WHOISOPERATOR_TEXT "\n",
                cfg->host, RPL_WHOISOPERATOR, client->nick, cl->nick);

  /* idle seconds */
  xsock = cl->sock;
  irc_printf (sock, ":%s %03d %s " RPL_WHOISIDLE_TEXT "\n",
              cfg->host, RPL_WHOISIDLE, client->nick,
              cl->nick, time (NULL) - xsock->last_send, cl->since);

  /* build channel list */
  for (text[0] = 0, i = 0; i < cl->channels; i++)
    {
      channel = cl->channel[i];
      n = irc_client_in_channel (NULL, cl, channel);
      if (channel->cflag[n] & MODE_OPERATOR)
        strcat (text, "@");
      else if (channel->cflag[n] & MODE_VOICE)
        strcat (text, "+");
      strcat (text, cl->channel[i]->name);
      strcat (text, " ");
    }

  /* send channel list */
  irc_printf (sock, ":%s %03d %s " RPL_WHOISCHANNELS_TEXT "\n",
              cfg->host, RPL_WHOISCHANNELS, client->nick, cl->nick, text);
}

/*
 *         Command: WHOIS
 *      Parameters: [<server>] <nickmask>[,<nickmask>[,...]]
 * Numeric Replies: ERR_NOSUCHSERVER   ERR_NONICKNAMEGIVEN
 *                  RPL_WHOISUSER      RPL_WHOISCHANNELS
 *                  RPL_WHOISCHANNELS  RPL_WHOISSERVER
 *                  RPL_AWAY           RPL_WHOISOPERATOR
 *                  RPL_WHOISIDLE      ERR_NOSUCHNICK
 *                  RPL_ENDOFWHOIS
 */
int
irc_whois_callback (svz_socket_t *sock,
                    irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t **cl, *rclient;
  char *nick;
  int n;

  /* enough paras?  */
  if (irc_check_args (sock, client, cfg, request, 1))
    return 0;

  /* server Mask?  */
  if (irc_string_regex (cfg->host, request->para[0]))
    irc_parse_target (request, 1);

  /* go through all targets */
  for (n = 0; n < request->targets; n++)
    {
      nick = request->target[n].nick;

      /* nick Mask?  */
      if (strstr (nick, "*") || strstr (nick, "?"))
        {
          if ((cl = irc_regex_nick (cfg, nick)) != NULL)
            {
              for (n = 0; cl[n]; n++)
                {
                  /* is this client away?  */
                  if (irc_client_absent (cl[n], client))
                    continue;
                  irc_user_info (sock, client, cl[n]);
                }
              svz_free (cl);
            }
        }
      /* is target a plain nick?  */
      else if ((rclient = irc_find_nick (cfg, nick)) != NULL)
        {
          /* is this client away?  */
          if (irc_client_absent (rclient, client))
            continue;
          irc_user_info (sock, client, rclient);
        }

      irc_printf (sock, ":%s %03d %s " RPL_ENDOFWHOIS_TEXT "\n",
                  cfg->host, RPL_ENDOFWHOIS, client->nick, nick);
    }
  return 0;
}

/*
 * Send a single WHO reply.
 */
static void
irc_client_info (svz_socket_t *sock,     /* this client's socket */
                 irc_client_t *client,   /* this client */
                 irc_client_t *cl,       /* reply this client's info */
                 irc_channel_t *channel) /* channel 'cl' is in */
{
  irc_config_t *cfg = sock->cfg;
  char text[MAX_MSG_LEN];
  char *flag = "";
  int n;

  n = irc_client_in_channel (NULL, cl, channel);
  if (channel->cflag[n] & MODE_OPERATOR)
    flag = "@";
  else if (channel->cflag[n] & MODE_VOICE)
    flag = "+";

  sprintf (text, RPL_WHOREPLY_TEXT,
           channel->name, cl->user, cl->host, cl->server, cl->nick,
           cl->flag & UMODE_AWAY ? 'G' : 'H',
           cl->flag & UMODE_OPERATOR ? "*" : "", flag, 0, cl->real);

  irc_printf (sock, ":%s %03d %s %s\n",
              cfg->host, RPL_WHOREPLY, client->nick, text);
}

/*
 *         Command: WHO
 *      Parameters: [<name> [<o>]]
 * Numeric Replies: ERR_NOSUCHSERVER
 *                  RPL_WHOREPLY     RPL_ENDOFWHO
 */
int
irc_who_callback (svz_socket_t *sock,
                  irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t **cl, *xcl;
  irc_channel_t **channel, *xch;
  char *name;
  int n, i;

  if (!request->paras)
    name = "*";
  else
    name = request->para[0];

  /* find all Matching channels */
  if ((channel = irc_regex_channel (cfg, name)) != NULL)
    {
      for (i = 0; channel[i]; i++)
        {
          for (n = 0; n < channel[i]->clients; n++)
            {
              xcl = channel[i]->client[n];
              irc_client_info (sock, client, xcl, channel[i]);
            }
        }
      irc_printf (sock, ":%s %03d %s " RPL_ENDOFWHO_TEXT "\n",
                  cfg->host, RPL_ENDOFWHO, client->nick, name);
      svz_free (channel);
      return 0;
    }

  /* find all Matching nicks */
  if ((cl = irc_regex_nick (cfg, name)) != NULL)
    {
      for (i = 0; cl[i]; i++)
        {
          for (n = 0; n < cl[i]->channels; n++)
            {
              xch = cl[i]->channel[n];
              irc_client_info (sock, client, cl[i], xch);
            }
        }
      irc_printf (sock, ":%s %03d %s " RPL_ENDOFWHO_TEXT "\n",
                  cfg->host, RPL_ENDOFWHO, client->nick, name);
      svz_free (cl);
    }

  return 0;
}
