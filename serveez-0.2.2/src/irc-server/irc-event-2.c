/*
 * irc-event-2.c - IRC events -- Channel operations
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

/*
 *         Command: PART
 *      Parameters: <channel>{,<channel>}
 * Numeric Replies: ERR_NEEDMOREPARAMS ERR_NOSUCHCHANNEL
 *                  ERR_NOTONCHANNEL
 */
int
irc_part_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *cl;
  irc_channel_t *channel;
  svz_socket_t *xsock;
  int n;

  /* do you have enough paras?  */
  if (irc_check_args (sock, client, cfg, request, 1))
    return 0;

  /* go through all targets in the first para */
  for (n = 0; n < request->targets; n++)
    {
      /* does the channel exist?  */
      if ((channel = irc_find_channel (cfg, request->target[n].channel)) ==
          NULL)
        {
          irc_printf (sock, ":%s %03d %s " ERR_NOSUCHCHANNEL_TEXT "\n",
                      cfg->host, ERR_NOSUCHCHANNEL, client->nick,
                      request->target[n].channel);
          return 0;
        }

      /* yes, look if the client is in it */
      if (irc_client_in_channel (sock, client, channel) == -1)
        return 0;

      /* send back the PART to all channel clients */
      for (n = 0; n < channel->clients; n++)
        {
          cl = channel->client[n];
          xsock = cl->sock;
          irc_printf (xsock, ":%s!%s@%s PART %s :%s\n",
                      client->nick, client->user, client->host,
                      channel->name, request->para[1]);
        }

      irc_leave_channel (cfg, client, channel);
    }
  return 0;
}

/*
 * This function returns no zero value if a given client matches
 * a given channel ban entry.
 */
static int
irc_client_banned (irc_client_t *client, irc_ban_t *ban)
{
  /* does the nick Match?  */
  if (!irc_string_regex (client->nick, ban->nick))
    return 0;

  /* does the user Match?  */
  if (!irc_string_regex (client->user, ban->user))
    return 0;

  /* does the host Match?  */
  if (!irc_string_regex (client->host, ban->host))
    return 0;

  return -1;
}

/*
 * Send the channel topic to an IRC client.
 */
static void
irc_channel_topic (svz_socket_t *sock,
                   irc_client_t *client, irc_channel_t *channel)
{
  irc_config_t *cfg = sock->cfg;

  /* send topic if there is one */
  if (channel->topic)
    {
      irc_printf (sock, ":%s %03d %s " RPL_TOPIC_TEXT "\n",
                  cfg->host, RPL_TOPIC, client->nick,
                  channel->name, channel->topic);

      /* send topic date and nick (this is not part of the RFC) */
      irc_printf (sock, ":%s %03d %s " RPL_TOPICSET_TEXT "\n",
                  cfg->host, RPL_TOPICSET, client->nick, channel->name,
                  channel->topic_by, channel->topic_since);
    }
  /* no topic set yet */
  else
    {
      irc_printf (sock, ":%s %03d %s " RPL_NOTOPIC_TEXT "\n",
                  cfg->host, RPL_NOTOPIC, client->nick, channel->name);
    }
}

/*
 * Send a channels users to a specified client.
 */
static void
irc_channel_users (svz_socket_t *sock,
                   irc_client_t *client,
                   irc_channel_t *channel)
{
  irc_config_t *cfg = sock->cfg;

  char nicklist[MAX_MSG_LEN];
  irc_client_t *cl;
  int n;

  /* prebuild the nicklist */
  for (nicklist[0] = 0, n = 0; n < channel->clients; n++)
    {
      /* check if the client is visible or not */
      cl = channel->client[n];
      if (!(cl->flag & UMODE_INVISIBLE) &&
          irc_client_in_channel (NULL, client, channel) != -1)
        {
          if (channel->cflag[n] & MODE_OPERATOR)
            strcat (nicklist, "@");
          else if (channel->cflag[n] & MODE_VOICE)
            strcat (nicklist, "+");
          strcat (nicklist, channel->client[n]->nick);
          strcat (nicklist, " ");
        }
    }

  /* send channel info */
  irc_printf (sock, ":%s %03d %s " RPL_NAMREPLY_TEXT "\n",
              cfg->host, RPL_NAMREPLY, client->nick,
              channel->flag & (MODE_PRIVATE | MODE_SECRET) ? '*' : '=',
              channel->name, nicklist);
}

/*
 *         Command: JOIN
 *      Parameters: <channel>{,<channel>} [<key>{,<key>}]
 * Numeric Replies: ERR_NEEDMOREPARAMS ERR_BANNEDFROMCHAN
 *                  ERR_INVITEONLYCHAN ERR_BADCHANNELKEY
 *                  ERR_CHANNELISFULL  ERR_BADCHANMASK
 *                  ERR_NOSUCHCHANNEL  ERR_TOOMANYCHANNELS
 *                  RPL_TOPIC
 */
int
irc_join_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *cl;
  irc_channel_t *channel;
  svz_socket_t *xsock;
  char *chan;
  int n, i;

  /* do you have enough paras?  */
  if (irc_check_args (sock, client, cfg, request, 1))
    return 0;

  /* go through all targets in the first para */
  for (n = 0; n < request->targets; n++)
    {
      chan = request->target[n].channel;
      if (*chan == '\0')
        {
          irc_printf (sock, ":%s %03d %s " ERR_NOSUCHCHANNEL_TEXT "\n",
                      cfg->host, ERR_NOSUCHCHANNEL, client->nick,
                      request->para[n]);
          return 0;
        }

      /* does the channel already exists?  */
      if ((channel = irc_find_channel (cfg, chan)) != NULL)
        {
          /* is a key set for this channel and is the given one ok?  */
          if (channel->flag & MODE_KEY &&
              strcmp (irc_get_target (request->para[1], n), channel->key))
            {
              irc_printf (sock,
                          ":%s %03d %s " ERR_BADCHANNELKEY_TEXT "\n",
                          cfg->host, ERR_BADCHANNELKEY, client->nick,
                          channel->name);
              return 0;
            }
          /* invite only?  */
          if (channel->flag & MODE_INVITE)
            {
              /* find the nick in the invite list of the channel */
              for (i = 0; i < channel->invites; i++)
                if (channel->invite[i] == client)
                  break;

              /* not in this list!  */
              if (i == channel->invites)
                {
                  irc_printf (sock,
                              ":%s %03d %s " ERR_INVITEONLYCHAN_TEXT "\n",
                              cfg->host, ERR_INVITEONLYCHAN, client->nick,
                              channel->name);
                  return 0;
                }
              /* clear this invite entry */
              else
                {
                  if (--channel->invites != 0)
                    {
                      channel->invite[i] = channel->invite[channel->invites];
                      channel->invite = svz_realloc (channel->invite,
                                                     sizeof (irc_client_t *) *
                                                     channel->invites);
                    }
                  else
                    {
                      svz_free (channel->invite);
                      channel->invite = NULL;
                    }
                }
            }
          /* is channel full?  */
          if (channel->flag & MODE_ULIMIT &&
              channel->clients >= channel->users)
            {
              irc_printf (sock,
                          ":%s %03d %s " ERR_CHANNELISFULL_TEXT "\n",
                          cfg->host, ERR_CHANNELISFULL, client->nick,
                          channel->name);
              return 0;
            }
          /* is this client banned?  */
          for (i = 0; i < channel->bans; i++)
            {
              if (irc_client_banned (client, channel->ban[i]))
                {
                  irc_printf (sock,
                              ":%s %03d %s " ERR_BANNEDFROMCHAN_TEXT "\n",
                              cfg->host, ERR_BANNEDFROMCHAN, client->nick,
                              channel->name);
                  return 0;
                }
            }
        }

      /* done, do not deny this channel!  */
      irc_join_channel (cfg, client, chan);
      if ((channel = irc_find_channel (cfg, chan)) == NULL)
        continue;

      /* send back the JOIN to all channel clients */
      for (i = 0; i < channel->clients; i++)
        {
          cl = channel->client[i];
          xsock = cl->sock;
          irc_printf (xsock, ":%s!%s@%s JOIN :%s\n",
                      client->nick, client->user, client->host, chan);
        }

      /* send topic */
      irc_channel_topic (sock, client, channel);

      /* send creation date and nick (this is not part of the RFC) */
      irc_printf (sock, ":%s %03d %s " RPL_CHANCREATED_TEXT "\n",
                  cfg->host, RPL_CHANCREATED, client->nick,
                  channel->name, channel->since);

      /* send nick list */
      irc_channel_users (sock, client, channel);
      irc_printf (sock, ":%s %03d %s " RPL_ENDOFNAMES_TEXT "\n",
                  cfg->host, RPL_ENDOFNAMES, client->nick, channel->name);
    }

  return 0;
}

/*
 * Create a ban mask string by a channels ban entry.
 */
static char *
irc_ban_string (irc_ban_t *ban)
{
  static char text[MAX_MSG_LEN] = "";

  if (ban->nick)
    {
      strcat (text, ban->nick);
      strcat (text, "!");
    }
  if (ban->user)
    {
      strcat (text, ban->user);
      strcat (text, "@");
    }
  strcat (text, ban->host);

  return text;
}

/*
 * Check if a given client is channel operator in its channel and
 * send an error about this if necessary.  The function returns a non
 * zero value if the client is an operator otherwise zero.
 */
static int
irc_client_oper (svz_socket_t *sock,
                 irc_client_t *client, irc_channel_t *channel, int flag)
{
  irc_config_t *cfg = sock->cfg;

  if (!(flag & MODE_OPERATOR))
    {
      irc_printf (sock, ":%s %03d %s " ERR_CHANOPRIVSNEEDED_TEXT "\n",
                  cfg->host, ERR_CHANOPRIVSNEEDED,
                  client->nick, channel->name);
      return 0;
    }
  return -1;
}

/*
 * Set or unset a channel flag for a user in the channel.
 */
static int
irc_client_flag (irc_client_t *client,   /* client changing the flag */
                 svz_socket_t *sock,     /* this clients connection */
                 int cflag,              /* this clients flags */
                 char *nick,             /* the nick the mode is for */
                 irc_channel_t *channel, /* mode change for this channel */
                 int flag,               /* flag to be set / unset */
                 char set)               /* set / unset */
{
  irc_client_t *cl;
  svz_socket_t *xsock;
  int i, n;
  unsigned l;
  static char *Modes = CHANNEL_MODES;
  char Mode = ' ';

  /* find Mode character */
  for (l = 0; l < strlen (Modes); l++)
    if (flag & (1 << l))
      {
        Mode = Modes[l];
        break;
      }

  if (!irc_client_oper (sock, client, channel, cflag))
    return 0;

  /* find nick in channel */
  for (i = 0; i < channel->clients; i++)
    if (!strcmp (channel->client[i]->nick, nick))
      {
        if (set)
          channel->cflag[i] |= flag;
        else
          channel->cflag[i] &= ~flag;
        break;
      }

  /* no such nick in channel!  */
  if (i == channel->clients)
    {
      irc_printf (sock, "%03d " ERR_NOSUCHNICK_TEXT "\n",
                  ERR_NOSUCHNICK, nick);
      return 0;
    }

  /* propagate Mode change to channel users */
  for (n = 0; n < channel->clients; n++)
    {
      cl = channel->client[n];
      xsock = cl->sock;
      irc_printf (xsock, ":%s!%s@%s MODE %s %c%c %s\n",
                  client->nick, client->user, client->host, channel->name,
                  set ? '+' : '-', Mode, channel->client[i]->nick);
    }

  return 0;
}

/*
 * Set or unset a flag for a user by itself.
 */
static int
irc_user_flag (irc_client_t *client,   /* client changing the flag */
               svz_socket_t *sock,     /* this clients connection */
               int flag,               /* flag to be set / unset */
               char set)               /* set / unset */
{
  irc_config_t *cfg = sock->cfg;
  unsigned l;
  static char *Modes = USER_MODES;
  char Mode = ' ';

  /* find Mode character */
  for (l = 0; l < strlen (Modes); l++)
    if (flag & (1 << l))
      {
        Mode = Modes[l];
        break;
      }

  /* is this client able to change the flag */
  if (set)
    {
      if (flag & UMODE_INVISIBLE && !(client->flag & UMODE_INVISIBLE))
        cfg->invisibles++;

      /* this flag cannot be set by anyone!  */
      if (!(flag & UMODE_OPERATOR))
        client->flag |= flag;
      else
        return 0;
    }
  else
    {
      if (flag & UMODE_INVISIBLE && client->flag & UMODE_INVISIBLE)
        cfg->invisibles--;

      client->flag &= ~flag;
    }

  /* propagate Mode change to this users */
  irc_printf (sock, ":%s!%s@%s MODE %s %c%c\n",
              client->nick, client->user, client->host,
              client->nick, set ? '+' : '-', Mode);

  return 0;
}

/*
 * Set or unset a channel flag.
 */
static int
irc_channel_flag (irc_client_t *client,   /* client changing the flag */
                  svz_socket_t *sock,     /* this clients connection */
                  int cflag,              /* this clients flags */
                  irc_channel_t *channel, /* mode change for this channel */
                  int flag,               /* flag to be set / unset */
                  char set)               /* set / unset */
{
  irc_client_t *cl;
  svz_socket_t *xsock;
  int n;
  unsigned l;
  char *Modes = CHANNEL_MODES;
  char Mode = ' ';

  /* find Mode character */
  for (l = 0; l < strlen (Modes); l++)
    if (flag & (1 << l))
      {
        Mode = Modes[l];
        break;
      }

  if (!irc_client_oper (sock, client, channel, cflag))
    return 0;

  if (set)
    channel->flag |= flag;
  else
    channel->flag &= ~flag;

  /* propagate Mode change to channel users */
  for (n = 0; n < channel->clients; n++)
    {
      cl = channel->client[n];
      xsock = cl->sock;
      irc_printf (xsock, ":%s!%s@%s MODE %s %c%c\n",
                  client->nick, client->user, client->host, channel->name,
                  set ? '+' : '-', Mode);
    }

  return 0;
}

/*
 * Destroy and ‘svz_free’ a given ban entry.
 */
void
irc_destroy_ban (irc_ban_t *ban)
{
  if (ban->nick)
    svz_free (ban->nick);
  if (ban->user)
    svz_free (ban->user);
  svz_free (ban->host);
  svz_free (ban->by);
  svz_free (ban);
}

/*
 * This function creates a ban entry.
 */
static irc_ban_t *
irc_create_ban (irc_client_t *client, char *request, int len)
{
  irc_ban_t *ban;
  char *p, *tmp;
  int n, size = 0;

  /* reserve and initialize buffer space */
  ban = svz_calloc (sizeof (irc_ban_t));
  tmp = svz_calloc (MAX_MSG_LEN);

  ban->since = time (NULL);
  sprintf (tmp, "%s!%s@%s", client->nick, client->user, client->host);
  ban->by = svz_strdup (tmp);

  p = request;
  n = 0;
  while (size < len && *p != '!' && *p != '@')
    {
      tmp[n++] = *p++;
      size++;
    }
  /* nick has been parsed */
  if (*p == '!')
    {
      tmp[n] = '\0';
      ban->nick = svz_strdup (tmp);
      p++;
      size++;
      n = 0;
      while (size < len && *p != '@')
        {
          tmp[n++] = *p++;
          size++;
        }
      /* user parsed */
      if (*p == '@')
        {
          tmp[n] = '\0';
          ban->user = svz_strdup (tmp);
          p++;
          size++;
          n = 0;
          while (size < len)
            {
              tmp[n++] = *p++;
              size++;
            }
          tmp[n] = '\0';
          ban->host = svz_strdup (tmp);
        }
    }
  /* here user parsed without nick */
  else if (*p == '@')
    {
      tmp[n] = '\0';
      ban->user = svz_strdup (tmp);
      p++;
      size++;
      n = 0;
      while (size < len)
        {
          tmp[n++] = *p++;
          size++;
        }
      tmp[n] = '\0';
      ban->host = svz_strdup (tmp);
    }
  /* parsed just a host without user and nick */
  else
    {
      tmp[n] = '\0';
      ban->host = svz_strdup (tmp);
    }

  svz_free (tmp);
  return ban;
}

/*
 * Set or unset a channel specific flag to a given argument.
 */
static int
irc_channel_arg (irc_client_t *client,   /* client changing the flag */
                 svz_socket_t *sock,     /* this clients connection */
                 int cflag,              /* this clients flags */
                 irc_channel_t *channel, /* mode change for this channel */
                 int flag,               /* flag to be set / unset */
                 char *arg,              /* the arg */
                 char set)               /* set / unset */
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *cl;
  svz_socket_t *xsock;
  int n;
  unsigned l;
  char *Modes = CHANNEL_MODES;
  char Mode = ' ';

  /* enough paras?  */
  if (set && arg[0] == 0)
    {
      irc_printf (sock, ":%s %03d %s " ERR_NEEDMOREPARAMS_TEXT "\n",
                  cfg->host, ERR_NEEDMOREPARAMS, client->nick, "MODE");
      return 0;
    }

  /* find Mode character */
  for (l = 0; l < strlen (Modes); l++)
    if (flag & (1 << l))
      {
        Mode = Modes[l];
        break;
      }

  if (!irc_client_oper (sock, client, channel, cflag))
    return 0;

  /* set / unset the actual flag */
  if (set)
    {
      switch (flag)
        {
        case MODE_KEY:
          /* key is set before...  */
          if (channel->flag & flag)
            {
              irc_printf (sock, ":%s %03d %s " ERR_KEYSET_TEXT "\n",
                          cfg->host, ERR_KEYSET, client->nick, channel->name);
              return 0;
            }
          if (channel->key)
            svz_free (channel->key);
          channel->key = svz_strdup (arg);
          channel->flag |= flag;
          break;
        case MODE_ULIMIT:
          channel->users = atoi (arg);
          channel->flag |= flag;
          break;
        case MODE_BAN:
          n = channel->bans;
          channel->ban = svz_realloc (channel->ban,
                                      sizeof (irc_ban_t *) * (n + 1));
          channel->ban[n] = irc_create_ban (client, arg, strlen (arg));
          channel->bans++;
          break;
        }
    }
  /* unset flag */
  else
    {
      /* look if you can delete a ban */
      if (flag & MODE_BAN)
        {
          for (n = 0; n < channel->bans; n++)
            {
              if (!strcmp (arg, irc_ban_string (channel->ban[n])))
                {
                  irc_destroy_ban (channel->ban[n]);
                  if (--channel->bans != 0)
                    {
                      channel->ban[n] = channel->ban[channel->bans];
                      channel->ban = svz_realloc (channel->ban,
                                                  sizeof (irc_ban_t *) *
                                                  channel->bans);
                    }
                  else
                    {
                      svz_free (channel->ban);
                      channel->ban = NULL;
                    }
                  break;
                }
            }
        }
      else
        {
          channel->flag &= ~flag;
        }
    }

  /* propagate Mode change to channel users */
  for (n = 0; n < channel->clients; n++)
    {
      cl = channel->client[n];
      xsock = cl->sock;
      irc_printf (xsock, ":%s!%s@%s MODE %s %c%c%s%s\n",
                  client->nick, client->user, client->host,
                  channel->name,
                  set ? '+' : '-', Mode, set ? " " : "", set ? arg : "");
    }

  return 0;
}

/*
 * Get a mode string by user modes.
 */
char *
irc_client_flag_string (irc_client_t *client)
{
  static char Mode[MAX_MSG_LEN];
  static char *Modes = USER_MODES;
  unsigned n, i;

  memset (Mode, 0, MAX_MSG_LEN);

  Mode[0] = '+';

  for (i = n = 0; n < strlen (Modes); n++)
    {
      if (client->flag & (1 << n))
        Mode[++i] = Modes[n];
    }

  return Mode;
}

/*
 * Get a flag string by a channels modes.
 */
char *
irc_channel_flag_string (irc_channel_t *channel)
{
  static char Mode[MAX_MODE_LEN];
  static char arg[MAX_MODE_LEN];
  static char *Modes = CHANNEL_MODES;
  unsigned n, i;

  memset (Mode, 0, MAX_MODE_LEN);
  Mode[0] = '+';

  /* go through all possible Mode flags (characters) */
  for (i = n = 0; n < strlen (Modes); n++)
    {
      if (channel->flag & (1 << n))
        Mode[++i] = Modes[n];
    }

  /* add flag para if necessary */
  if (channel->flag & MODE_ULIMIT)
    {
      sprintf (arg, " %d", channel->users);
      strcat (Mode, arg);
    }

  return Mode;
}

/*
 *         Command: MODE
 *   Channel modes:
 *      Parameters: <channel> {[+|-]|o|p|s|i|t|n|b|v} [<limit>] [<user>]
 *                  [<ban mask>]
 *      User modes:
 *      Parameters: <nickname> {[+|-]|i|w|s|o}
 * Numeric Replies: ERR_NEEDMOREPARAMS   RPL_CHANNELMODEIS
 *                  ERR_CHANOPRIVSNEEDED ERR_NOSUCHNICK
 *                  ERR_NOTONCHANNEL     ERR_KEYSET
 *                  RPL_BANLIST          RPL_ENDOFBANLIST
 *                  ERR_UNKNOWNMODE      ERR_NOSUCHCHANNEL
 *                  ERR_USERSDONTMATCH   RPL_UMODEIS
 *                  ERR_UMODEUNKNOWNFLAG
 */

#define FLAG_SET   1
#define FLAG_UNSET 0

int
irc_mode_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *cl;
  irc_channel_t *channel;
  char *chan, *nick, *req;
  char flag;
  int cflag;
  int n, para;

  /* enough paras, but at least one?  */
  if (irc_check_args (sock, client, cfg, request, 1))
    return 0;

  chan = nick = request->para[0];

  /* is target channel?  */
  if ((channel = irc_find_channel (cfg, chan)) != NULL)
    {
      /* this is a request only?  */
      if (request->paras < 2)
        {
          irc_printf (sock, ":%s %03d %s " RPL_CHANNELMODEIS_TEXT "\n",
                      cfg->host, RPL_CHANNELMODEIS, client->nick,
                      channel->name, irc_channel_flag_string (channel));
          return 0;
        }

      /* find client in the channel first */
      if ((n = irc_client_in_channel (sock, client, channel)) == -1)
        return 0;
      cflag = channel->cflag[n];

      /* go through all the Mode string and current arg is # 2 */
      req = request->para[1];
      para = 2;
      flag = FLAG_SET;

      while (*req)
        {
          switch (*req)
            {
            case '+':
              flag = FLAG_SET;
              break;
            case '-':
              flag = FLAG_UNSET;
              break;
              /* set or get ban Mask */
            case 'b':
              /* if no args, then reply the bans */
              if (para >= request->paras)
                {
                  for (n = 0; n < channel->bans; n++)
                    {
                      irc_printf (sock, ":%s %03d %s " RPL_BANLIST_TEXT "\n",
                                  cfg->host,
                                  RPL_BANLIST, client->nick, channel->name,
                                  irc_ban_string (channel->ban[n]),
                                  channel->ban[n]->by,
                                  channel->ban[n]->since);
                    }
                  irc_printf (sock, ":%s %03d %s " RPL_ENDOFBANLIST_TEXT "\n",
                              cfg->host, RPL_ENDOFBANLIST, client->nick,
                              channel->name);
                }
              /* yes there are args, so set or unset the ban */
              else
                {
                  irc_channel_arg (client, sock, cflag, channel, MODE_BAN,
                                   request->para[para], flag);
                  para++;
                }
              break;
              /* channel operator flag */
            case 'o':
              irc_client_flag (client, sock, cflag, request->para[para],
                               channel, MODE_OPERATOR, flag);
              para++;
              break;
              /* voice flag for this channel */
            case 'v':
              irc_client_flag (client, sock, cflag, request->para[para],
                               channel, MODE_VOICE, flag);
              para++;
              break;
              /* private flag of this channel */
            case 'p':
              irc_channel_flag (client, sock, cflag,
                                channel, MODE_PRIVATE, flag);
              break;
              /* secret flag */
            case 's':
              irc_channel_flag (client, sock, cflag,
                                channel, MODE_SECRET, flag);
              break;
              /* invite only flag */
            case 'i':
              irc_channel_flag (client, sock, cflag,
                                channel, MODE_INVITE, flag);
              break;
              /* topic setable by operator only */
            case 't':
              irc_channel_flag (client, sock, cflag,
                                channel, MODE_TOPIC, flag);
              break;
              /* no Message of outside flag */
            case 'n':
              irc_channel_flag (client, sock, cflag,
                                channel, MODE_MESSAGE, flag);
              break;
              /* Moderated channel */
            case 'm':
              irc_channel_flag (client, sock, cflag,
                                channel, MODE_MODERATE, flag);
              break;
              /* set the users limitation flag (taking one arg) */
            case 'l':
              irc_channel_arg (client, sock, cflag, channel, MODE_ULIMIT,
                               request->para[para], flag);
              if (flag)
                para++;
              break;
              /* set a key for this channel */
            case 'k':
              irc_channel_arg (client, sock, cflag, channel, MODE_KEY,
                               request->para[para], flag);
              if (flag)
                para++;
              break;
              /* unknown Mode flag */
            default:
              irc_printf (sock, ":%s %03d %s " ERR_UNKNOWNMODE_TEXT "\n",
                          cfg->host, ERR_UNKNOWNMODE, client->nick, *req);
              break;
            }
          req++;
        }
    }

  /* is target nick?  */
  else if ((cl = irc_find_nick (cfg, nick)) != NULL)
    {
      /* get and set user flag only by itself!  */
      if (cl != client)
        {
          irc_printf (sock, ":%s %03d %s " ERR_USERSDONTMATCH_TEXT "\n",
                      cfg->host, ERR_USERSDONTMATCH, client->nick);
          return 0;
        }
      /* this is a request only?  */
      if (request->paras < 2)
        {
          irc_printf (sock, ":%s %03d %s %s %s\n", cfg->host,
                      RPL_UMODEIS,  client->nick, client->nick,
                      irc_client_flag_string (client));
          return 0;
        }

      /* go through all the Mode string */
      req = request->para[1];
      flag = FLAG_SET;

      while (*req)
        {
          switch (*req)
            {
            case '+':
              flag = FLAG_SET;
              break;
            case '-':
              flag = FLAG_UNSET;
              break;
              /* invisible flag */
            case 'i':
              irc_user_flag (client, sock, UMODE_INVISIBLE, flag);
              break;
              /* get server Messages */
            case 's':
              irc_user_flag (client, sock, UMODE_SERVER, flag);
              break;
              /* get operator Messages */
            case 'w':
              irc_user_flag (client, sock, UMODE_WALLOP, flag);
              break;
              /* just deop yourself!  */
            case 'o':
              irc_user_flag (client, sock, UMODE_OPERATOR, flag);
              break;
              /* unknown Mode flag */
            default:
              irc_printf (sock, ":%s %03d %s " ERR_UNKNOWNMODE_TEXT "\n",
                          cfg->host, ERR_UNKNOWNMODE, client->nick, *req);
              break;
            }
          req++;
        }
    }

  return 0;
}

/*
 *         Command: TOPIC
 *      Parameters: <channel> [<topic>]
 * Numeric Replies: ERR_NEEDMOREPARAMS   ERR_NOTONCHANNEL
 *                  RPL_NOTOPIC          RPL_TOPIC
 *                  ERR_CHANOPRIVSNEEDED
 */
int
irc_topic_callback (svz_socket_t *sock,
                    irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *cl;
  irc_channel_t *channel;
  svz_socket_t *xsock;
  int n, i;

  /* enough paras?  */
  if (irc_check_args (sock, client, cfg, request, 1))
    return 0;

  /* get the channel target */
  if ((channel = irc_find_channel (cfg, request->target[0].channel)) != NULL)
    {
      if ((n = irc_client_in_channel (sock, client, channel)) != -1)
        {
          /* just send TOPIC back */
          if (request->paras < 2)
            {
              irc_channel_topic (sock, client, channel);
            }
          /* check if client can set the TOPIC */
          else
            {
              if ((channel->flag & MODE_TOPIC) &&
                  !irc_client_oper (sock, client, channel, channel->cflag[n]))
                return 0;

              /* change the topic */
              if (channel->topic)
                svz_free (channel->topic);
              channel->topic = svz_strdup (request->para[1]);
              if (channel->topic_by)
                svz_free (channel->topic_by);
              channel->topic_by = svz_strdup (client->nick);
              channel->topic_since = time (NULL);

              /* send topic to all clients in channel */
              for (i = 0; i < channel->clients; i++)
                {
                  cl = channel->client[i];
                  xsock = cl->sock;
                  irc_printf (xsock, ":%s!%s@%s TOPIC %s :%s\n",
                              client->nick, client->user, client->host,
                              channel->name, channel->topic);
                }
            }
        }
    }
  return 0;
}

struct names_cb_ch_closure
{
  irc_client_t *client;
  svz_socket_t *sock;
};

static void
names_cb_ch_internal (UNUSED void *k, void *v, void *closure)
{
  irc_channel_t *ch = v;
  struct names_cb_ch_closure *x = closure;
  irc_client_t *client = x->client;

  /*
   * You can't see secret and private channels,
   * except you are in it.
   */
  if (irc_client_in_channel (NULL, client, ch) == -1
      && (ch->flag & (MODE_SECRET | MODE_PRIVATE)))
    return;

  /* Send a line.  */
  irc_channel_users (x->sock, client, ch);
}

struct names_cb_cl_closure
{
  int n;
  char *text;
  irc_client_t *client;
};

static void
names_cb_cl_internal (UNUSED void *k, void *v, void *closure)
{
  irc_client_t *cl = v;
  struct names_cb_cl_closure *x = closure;
  char *text = x->text;
  irc_client_t *client = x->client;

  text[0] = '\0';

  /* Visible?  */
  if (!(cl->flag & UMODE_INVISIBLE))
    {
      int i;

      /* Go through all channels of the client.  */
      for (i = 0; i < cl->channels; i++)
        {
          irc_channel_t *channel = cl->channel[i];

          /* Is it public or a shared channel?  */
          if (!(channel->flag & (MODE_SECRET | MODE_PRIVATE))
              || -1 != irc_client_in_channel (NULL, client, channel))
            break;
        }
      /* Is the client in no channel or in no shared or public?  */
      if (x->n == cl->channels)
        {
          if (cl->flag & UMODE_OPERATOR)
            strcat (text, "@");
          strcat (text, cl->nick);
          strcat (text, " ");
        }
    }
}

/*
 *    Command: NAMES
 * Parameters: [<channel>{,<channel>}]
 *   Numerics: RPL_NAMREPLY  RPL_ENDOFNAMES
 */
int
irc_names_callback (svz_socket_t *sock,
                    irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_channel_t *channel;
  static char text[MAX_MSG_LEN];
  int n;

  /* list all channels and users?  */
  if (request->paras < 1)
    {
      /* go through all channels */
      if (svz_hash_size (cfg->channels))
        {
          struct names_cb_ch_closure x = { client, sock };

          svz_hash_foreach (names_cb_ch_internal, cfg->channels, &x);
        }

      /*
       * List all users not being in channels at all,
       * being visible, and are not in public channels.
       * Public channels are not secret or private or channels
       * clients share.
       */
      if (svz_hash_size (cfg->clients))
        {
          struct names_cb_cl_closure x = { 0, text, client };

          svz_hash_foreach (names_cb_cl_internal, cfg->clients, &x);
        }
      /* send the (*) reply */
      irc_printf (sock, ":%s %03d %s " RPL_NAMREPLY_TEXT "\n",
                  cfg->host, RPL_NAMREPLY, client->nick, '*', "*", text);

      /* send end of list */
      irc_printf (sock, ":%s %03d %s " RPL_ENDOFNAMES_TEXT "\n",
                  cfg->host, RPL_ENDOFNAMES, client->nick, "");
    }
  /* list only specified channels */
  else
    {
      for (n = 0; n < request->targets; n++)
        {
          if ((channel = irc_find_channel (cfg, request->target[n].channel))
              != NULL)
            {
              if (irc_client_in_channel (NULL, client, channel) == -1 &&
                  (channel->flag & (MODE_SECRET | MODE_PRIVATE)))
                continue;
              irc_channel_users (sock, client, channel);
            }
          irc_printf (sock, ":%s %03d %s " RPL_ENDOFNAMES_TEXT "\n",
                      cfg->host, RPL_ENDOFNAMES, client->nick, channel->name);
        }
    }

  return 0;
}

/*
 * Send a part of a channel list.
 */
static void
irc_channel_list (svz_socket_t *sock,
                  irc_client_t *client, irc_channel_t *channel)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *cl;
  int n;
  int visibles;

  /* hide secret channels */
  if (channel->flag & MODE_SECRET)
    return;

  /* hide private channels you're not in */
  if (channel->flag & MODE_PRIVATE &&
      irc_client_in_channel (NULL, client, channel) == -1)
    return;

  /* count visible clients in the channel */
  for (visibles = 0, n = 0; n < channel->clients; n++)
    {
      cl = channel->client[n];
      if (!(cl->flag & UMODE_INVISIBLE))
        visibles++;
    }

  /* send channel info */
  irc_printf (sock, ":%s %03d %s %s" RPL_LIST_TEXT "\n",
              cfg->host, RPL_LIST, client->nick,
              channel->flag & MODE_PRIVATE ? "* " : "",
              channel->name, visibles, channel->topic);
}

struct list_cb_closure
{
  svz_socket_t *sock;
  irc_client_t *client;
};

static void
list_cb_internal (UNUSED void *k, void *v, void *closure)
{
  irc_channel_t *ch = v;
  struct list_cb_closure *x = closure;

  irc_channel_list (x->sock, x->client, ch);
}

/*
 *         Command: LIST
 *      Parameters: [<channel>{,<channel>} [<server>]]
 * Numeric Replies: ERR_NOSUCHSERVER RPL_LISTSTART
 *                  RPL_LIST         RPL_LISTEND
 */
int
irc_list_callback (svz_socket_t *sock,
                   irc_client_t *client,
                   irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_channel_t *channel;
  int n;

  irc_printf (sock, ":%s %03d %s " RPL_LISTSTART_TEXT "\n",
              cfg->host, RPL_LISTSTART, client->nick);

  /* list all channels */
  if (request->paras < 1)
    {
      struct list_cb_closure x = { sock, client };

      svz_hash_foreach (list_cb_internal, cfg->channels, &x);
    }
  /* list specified channels */
  else
    {
      for (n = 0; n < request->targets; n++)
        {
          if ((channel = irc_find_channel (cfg, request->target[n].channel))
              != NULL)
            {
              irc_channel_list (sock, client, channel);
            }
        }
    }

  irc_printf (sock, ":%s %03d " RPL_LISTEND_TEXT "\n",
              cfg->host, RPL_LISTEND);

  return 0;
}

/*
 *         Command: INVITE
 *      Parameters: <nickname> <channel>
 * Numeric Replies: ERR_NEEDMOREPARAMS   ERR_NOSUCHNICK
 *                  ERR_NOTONCHANNEL     ERR_USERONCHANNEL
 *                  ERR_CHANOPRIVSNEEDED
 *                  RPL_INVITING         RPL_AWAY
 */
int
irc_invite_callback (svz_socket_t *sock,
                     irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *cl;
  irc_channel_t *ch;
  svz_socket_t *xsock;
  char *nick, *channel;
  int n, i;

  /* enough paras?  */
  if (irc_check_args (sock, client, cfg, request, 2))
    return 0;

  nick = request->para[0];
  channel = request->para[1];

  /* does the nick exist?  */
  if ((cl = irc_find_nick (cfg, nick)) == NULL)
    {
      irc_printf (sock, ":%s %03d %s " ERR_NOSUCHNICK_TEXT "\n",
                  cfg->host, ERR_NOSUCHNICK, client->nick, nick);
      return 0;
    }
  /* does the channel exist?  */
  if ((ch = irc_find_channel (cfg, channel)) == NULL)
    {
      irc_printf (sock, ":%s %03d %s " ERR_NOSUCHNICK_TEXT "\n",
                  cfg->host, ERR_NOSUCHNICK, client->nick, channel);
      return 0;
    }

  /* is the inviter in channel?  */
  if ((n = irc_client_in_channel (sock, client, ch)) == -1)
    return 0;

  /* is the user already in channel?  */
  if ((i = irc_client_in_channel (NULL, cl, ch)) != -1)
    {
      irc_printf (sock, ":%s %03d %s " ERR_USERONCHANNEL_TEXT "\n",
                  cfg->host, ERR_USERONCHANNEL, client->nick, nick, channel);
      return 0;
    }
  /* are you operator in this channel / can you invite?  */
  if (!irc_client_oper (sock, client, ch, ch->cflag[n]))
    return 0;

  /* is this client away?  */
  if (irc_client_absent (cl, client))
    return 0;

  /* send the invite Message */
  xsock = cl->sock;
  irc_printf (xsock, ":%s!%s@%s INVITE %s " RPL_INVITING_TEXT "\n",
              client->nick, client->user, client->host, cl->nick,
              ch->name, cl->nick);

  /* fill in the invited client into the channels invite array */
  n = ch->invites;
  ch->invite = svz_realloc (ch->invite, sizeof (irc_client_t *) * (n + 1));
  ch->invite[n] = cl;
  ch->invites++;

  return 0;
}

/*
 *         Command: KICK
 *      Parameters: <channel> <user> [<comment>]
 * Numeric Replies: ERR_NEEDMOREPARAMS  ERR_NOSUCHCHANNEL
 *                  ERR_BADCHANMASK     ERR_CHANOPRIVSNEEDED
 *                  ERR_NOTONCHANNEL
 */
int
irc_kick_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *cl;
  irc_client_t *victim;
  irc_channel_t *channel;
  svz_socket_t *xsock;
  int i, n;

  /* enough paras?  */
  if (irc_check_args (sock, client, cfg, request, 2))
    return 0;

  /* go through all targets (channels) */
  for (n = 0; n < request->targets; n++)
    {
      /* does the requested channel exist?  */
      if ((channel = irc_find_channel (cfg, request->target[n].channel))
          == NULL)
        {
          irc_printf (sock, ":%s %03d %s " ERR_NOSUCHCHANNEL_TEXT "\n",
                      cfg->host, ERR_NOSUCHCHANNEL, client->nick,
                      request->target[n].channel);
          continue;
        }

      /* yes there is such a channel, are you in it?  */
      if ((i = irc_client_in_channel (sock, client, channel)) == -1)
        continue;

      /* are you a channel operator?  */
      if (!irc_client_oper (sock, client, channel, channel->cflag[i]))
        continue;

      /* kick the requested user */
      if ((victim =
           irc_find_nick (cfg, irc_get_target (request->para[1], n))) == NULL)
        continue;

      /* is the victim in this channel?  */
      if (irc_client_in_channel (NULL, victim, channel) == -1)
        continue;

      /* tell all other clients about the kick */
      for (i = 0; i < channel->clients; i++)
        {
          cl = channel->client[i];
          xsock = cl->sock;
          irc_printf (xsock, ":%s!%s@%s KICK %s %s :%s\n",
                      client->nick, client->user,
                      client->host, channel->name, victim->nick,
                      request->para[2]);
        }

      irc_leave_channel (cfg, victim, channel);
    }

  return 0;
}
