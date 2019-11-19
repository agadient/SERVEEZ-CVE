/*
 * irc-proto.c - basic IRC protocol functions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
#include <time.h>

#if HAVE_STRINGS_H
# include <strings.h>
#endif

#ifndef __MINGW32__
# include <sys/types.h>
#endif

#include "networking-headers.h"
#include "libserveez.h"
#include "irc-core/irc-core.h"
#include "irc-proto.h"
#include "irc-event.h"
#include "irc-server.h"
#include "irc-config.h"
#include "unused.h"

/*
 * The IRC server instance default configuration,
 */
irc_config_t irc_config =
{
  0,                      /* logged in operators */
  0,                      /* logged in users */
  0,                      /* unknown connections */
  0,                      /* invisible users */
  NULL,                   /* virtual host name */
  NULL,                   /* read server host name */
  42424,                  /* listening tcp port */
  0,                      /* is USERS command disable?  */
  MAX_CHANNELS,           /* maximum number of channels per user */
#if ENABLE_TIMESTAMP
  0,                      /* delta value to UTC */
#endif
  { NULL },               /* message of the day */
  0,                      /* message of the day lines */
  0,                      /* motd last modified date */
  "../data/irc-MOTD.txt", /* file name of message of the day */
  NULL,                   /* MLine */
  NULL,                   /* ALine */
  NULL,                   /* YLines */
  NULL,                   /* ILines */
  NULL,                   /* OLines */
  NULL,                   /* oLines */
  NULL,                   /* CLines */
  NULL,                   /* NLines */
  NULL,                   /* KLines */
  NULL,                   /* server password */
  NULL,                   /* server info */
  NULL,                   /* email address of maintainers */
  NULL,                   /* admininfo */
  NULL,                   /* location1 */
  NULL,                   /* location2 */
  NULL,                   /* irc channel hash */
  NULL,                   /* irc client hash */
  NULL,                   /* irc server list root */
  NULL,                   /* client history list root */
  NULL,                   /* connection classes list */
  NULL,                   /* user authorizations */
  NULL,                   /* operator authorizations */
  NULL,                   /* banned users */
  NULL                    /* name of the /INFO file */
};

/*
 * Definition of the configuration items.
 */
svz_key_value_pair_t irc_config_prototype[] =
{
  SVZ_REGISTER_STR ("MOTD-file", irc_config.MOTD_file, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("INFO-file", irc_config.info_file, SVZ_ITEM_DEFAULTABLE),
#if ENABLE_TIMESTAMP
  SVZ_REGISTER_INT ("tsdelta", irc_config.tsdelta, SVZ_ITEM_DEFAULTABLE),
#endif
  SVZ_REGISTER_STR ("admininfo", irc_config.admininfo,
                    SVZ_ITEM_NOTDEFAULTABLE),
  SVZ_REGISTER_STR ("M-line", irc_config.MLine, SVZ_ITEM_NOTDEFAULTABLE),
  SVZ_REGISTER_STR ("A-line", irc_config.ALine, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STRARRAY ("Y-lines", irc_config.YLine, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STRARRAY ("I-lines", irc_config.ILine, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STRARRAY ("O-lines", irc_config.OLine, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STRARRAY ("o-lines", irc_config.oLine, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STRARRAY ("C-lines", irc_config.CLine, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STRARRAY ("N-lines", irc_config.NLine, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STRARRAY ("K-lines", irc_config.KLine, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("channels-per-user", irc_config.channels_per_user,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_END ()
};

/*
 * Definition of the IRC server.
 */
svz_servertype_t irc_server_definition =
{
  "irc server",        /* long description */
  "irc",               /* short description for instantiating */
  irc_global_init,     /* global initializer */
  irc_init,            /* instance initializer */
  irc_detect_proto,    /* detection routine */
  irc_connect_socket,  /* connection routine */
  irc_finalize,        /* instance finalizer */
  irc_global_finalize, /* global finalizer */
  NULL,                /* client info */
  NULL,                /* server info */
  NULL,                /* server timer */
  NULL,                /* server reset callback */
  NULL,                /* handle request callback */
  SVZ_CONFIG_DEFINE ("irc", irc_config, irc_config_prototype)
};

/* Static forward declarations.  */
static int irc_delete_channel (irc_config_t *cfg, irc_channel_t *);
static irc_channel_t *irc_add_channel (irc_config_t *cfg, char *channel);

/*
 * Global IRC server initializer.
 */
int
irc_global_init (UNUSED svz_servertype_t *server)
{
#if 0
  printf ("sizeof (socket_t)             = %d\n", sizeof (socket_data_t));
  printf ("sizeof (irc_ban_t)            = %d\n", sizeof (irc_ban_t));
  printf ("sizeof (irc_channel_t)        = %d\n", sizeof (irc_channel_t));
  printf ("sizeof (irc_client_t)         = %d\n", sizeof (irc_client_t));
  printf ("sizeof (irc_client_history_t) = %d\n",
          sizeof (irc_client_history_t));
  printf ("sizeof (irc_server_t)         = %d\n", sizeof (irc_server_t));
  printf ("sizeof (irc_user_t)           = %d\n", sizeof (irc_user_t));
  printf ("sizeof (irc_kill_t)           = %d\n", sizeof (irc_kill_t));
  printf ("sizeof (irc_oper_t)           = %d\n", sizeof (irc_oper_t));
  printf ("sizeof (irc_class_t)          = %d\n", sizeof (irc_class_t));
#endif

  irc_create_lcset ();
  return 0;
}

/*
 * Global IRC server finalizer.
 */
int
irc_global_finalize (UNUSED svz_servertype_t *server)
{
  return 0;
}

/*
 * Checks the TCP bindings of the given IRC server instance @var{server}.  The
 * routine is called from the initializer callback of the IRC server and
 * verifies that it is bound to TCP ports according to the `M-line' given in
 * its configuration.
 */
static int
irc_check_tcp_bindings (svz_server_t *server)
{
  irc_config_t *cfg = server->cfg;
  svz_array_t *ports;
  svz_portcfg_t *port;
  size_t n;
  int err = 0;

  /* Is the server bound at all?  */
  if ((ports = svz_server_portcfgs (server)) == NULL)
    {
      svz_log (SVZ_LOG_WARNING, "irc: `%s' is not bound yet\n", server->name);
      return -1;
    }

  /* Go through each binding.  */
  svz_array_foreach (ports, port, n)
    {
      /* Is it a TCP port configuration?  */
      if (!(port->proto & SVZ_PROTO_TCP))
        {
          svz_log (SVZ_LOG_WARNING, "irc: `%s' is bound to non-TCP port `%s'\n",
                   server->name, port->name);
          err = -1;
        }
      /* Does the `M-line' entry clash with this port configuration?  */
      else if (SVZ_CFG_TCP (port, port) != cfg->port)
        {
          svz_log (SVZ_LOG_WARNING, "irc: port TCP:%u in M line clashes `%s'\n",
                   cfg->port, port->name);
          cfg->port = SVZ_CFG_TCP (port, port);
          err = -1;
        }
    }
  svz_array_destroy (ports);
  return err;
}

static svz_hash_t *
make_irc_e_hash_table (void)
{
  return svz_hash_configure (svz_hash_create (4, NULL),
                             NULL, NULL, irc_string_equal);
}

#define MAX_TMP_STRLEN  256

/*
 * Initialization of the IRC server instance.
 */
int
irc_init (svz_server_t *server)
{
  irc_config_t *cfg = server->cfg;
  char tmp[3][MAX_TMP_STRLEN];

#define TB(i)                                           \
  /* Note: This is deliberately two expressions,        \
     separated by a comma.  Do not parenthesize!  */    \
  MAX_TMP_STRLEN, tmp[i]

  /* scan the M line (server configuration) */
  if (!cfg->MLine ||
      irc_parse_line (cfg->MLine, "M:%s:%s:%s:%d",
                      TB (0), TB (1), TB (2), &cfg->port) != 4)
    {
      svz_log (SVZ_LOG_ERROR, "irc: invalid M line: %s\n",
               cfg->MLine ? cfg->MLine : "(nil)");
      return -1;
    }

  cfg->host = svz_strdup (tmp[0]);
  cfg->realhost = svz_strdup (tmp[1]);
  cfg->info = svz_strdup (tmp[2]);

  /* check the bindings of this server */
  irc_check_tcp_bindings (server);

  /* scan the A line (administrative information) */
  if (!cfg->ALine ||
      irc_parse_line (cfg->ALine, "A:%s:%s:%s", TB (0), TB (1), TB (2)) != 3)
    {
      svz_log (SVZ_LOG_ERROR, "irc: invalid A line: %s\n",
               cfg->ALine ? cfg->ALine : "(nil)");
      return -1;
    }
  cfg->location1 = svz_strdup (tmp[0]);
  cfg->location2 = svz_strdup (tmp[1]);
  cfg->email = svz_strdup (tmp[2]);

  /* initialize hashes and lists */
  cfg->clients = make_irc_e_hash_table ();
  cfg->channels = make_irc_e_hash_table ();
  cfg->servers = NULL;
  cfg->history = NULL;

  irc_parse_config_lines (cfg);
  irc_connect_servers (cfg);

  return 0;

#undef TB
}

/*
 * IRC server instance finalizer.
 */
int
irc_finalize (svz_server_t *server)
{
  irc_config_t *cfg = server->cfg;
  int n;

  /* free the MOTD */
  for (n = 0; n < cfg->MOTDs; n++)
    svz_free (cfg->MOTD[n]);

  /* free configuration hash variables */
  svz_free (cfg->host);
  svz_free (cfg->info);
  svz_free (cfg->realhost);
  svz_free (cfg->location1);
  svz_free (cfg->location2);
  svz_free (cfg->email);

  irc_free_config_lines (cfg);

  /* free the client history */
  irc_delete_client_history (cfg);

  /* free the server IRC list */
  irc_delete_servers (cfg);

  svz_hash_destroy (cfg->clients);
  svz_hash_destroy (cfg->channels);

  return 0;
}

/*
 * Check if a certain client is in a channel.  Return -1 if not and
 * return the client's position in the channel list.  This is useful to
 * to detect its channel flags.  If you passed a valid socket the
 * ERR_NOTONCHANNEL reply is sent to the socket.
 */
int
irc_client_in_channel (svz_socket_t *sock,     /* client's socket */
                       irc_client_t *client,   /* the client structure */
                       irc_channel_t *channel) /* the channel to search */
{
  irc_config_t *cfg;
  int n;

  /* find client in the channel, return position if found */
  for (n = 0; n < channel->clients; n++)
    if (channel->client[n] == client)
      return n;

  /* not in channel!  */
  if (sock)
    {
      cfg = sock->cfg;
      irc_printf (sock, ":%s %03d %s " ERR_NOTONCHANNEL_TEXT "\n",
                  cfg->host, ERR_NOTONCHANNEL, client->nick, channel->name);
    }
  return -1;
}

/*
 * Add a nick to a certain channel.
 */
int
irc_join_channel (irc_config_t *cfg, irc_client_t *client, char *chan)
{
  irc_channel_t *channel;
  int n;

  /* does the channel exist locally?  */
  if ((channel = irc_find_channel (cfg, chan)) != NULL)
    {
      /* is the nick already in the channel?  */
      for (n = 0; n < channel->clients; n++)
        if (channel->client[n] == client)
          break;

      /* no, add nick to channel */
      if (n == channel->clients)
        {
          /* joined just too many channels?  */
          if (client->channels >= cfg->channels_per_user)
            {
              irc_printf (client->sock,
                          ":%s %03d %s " ERR_TOOMANYCHANNELS_TEXT "\n",
                          cfg->host, ERR_TOOMANYCHANNELS, client->nick,
                          channel->name);
            }
          else
            {
              channel->client = svz_realloc (channel->client,
                                             sizeof (irc_client_t *) *
                                             (n + 1));
              channel->cflag = svz_realloc (channel->cflag,
                                            sizeof (int) * (n + 1));
              channel->client[n] = client;
              channel->cflag[n] = 0;
              channel->clients++;
#if ENABLE_DEBUG
              svz_log (SVZ_LOG_DEBUG, "irc: %s joined channel %s\n",
                       client->nick, channel->name);
#endif
              n = client->channels;
              client->channel = svz_realloc (client->channel,
                                             sizeof (irc_channel_t *) *
                                             (n + 1));
              client->channel[n] = channel;
              client->channels++;
            }
        }
    }

  /* no, the channel does not exists, yet */
  else
    {
      /* check if the client has not joined too many channels */
      if (client->channels >= cfg->channels_per_user)
        {
          irc_printf (client->sock,
                      ":%s %03d %s " ERR_TOOMANYCHANNELS_TEXT "\n",
                      cfg->host, ERR_TOOMANYCHANNELS, client->nick, chan);
          return 0;
        }

      /* create one and set the first client as operator */
      channel = irc_add_channel (cfg, chan);
      channel->client = svz_malloc (sizeof (irc_client_t *));
      channel->cflag = svz_malloc (sizeof (int));
      channel->client[0] = client;
      channel->cflag[0] = MODE_OPERATOR;
      channel->clients = 1;
      channel->by = svz_strdup (client->nick);
      channel->since = time (NULL);
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "irc: channel %s created\n", channel->name);
      svz_log (SVZ_LOG_DEBUG, "irc: %s joined channel %s\n",
               client->nick, channel->name);
#endif
      n = client->channels;
      client->channel = svz_realloc (client->channel,
                                     sizeof (irc_channel_t *) * (n + 1));
      client->channel[n] = channel;
      client->channels++;
    }

  return 0;
}

/*
 * Delete a client of a given channel.
 */
int
irc_leave_channel (irc_config_t *cfg,
                   irc_client_t *client, irc_channel_t *channel)
{
  int n, i, last;

  /* delete the client of this channel */
  last = channel->clients - 1;
  for (n = 0; n < channel->clients; n++)
    if (channel->client[n] == client)
      {
        channel->clients--;
        if (last)
          {
            channel->client[n] = channel->client[last];
            channel->cflag[n] = channel->cflag[last];
            channel->client = svz_realloc (channel->client,
                                           sizeof (irc_client_t *) * last);
            channel->cflag = svz_realloc (channel->cflag,
                                          sizeof (int) * last);
          }
        else
          {
            svz_free (channel->client);
            channel->client = NULL;
            svz_free (channel->cflag);
            channel->cflag = NULL;
          }
#if ENABLE_DEBUG
        svz_log (SVZ_LOG_DEBUG, "irc: %s left channel %s\n",
                 client->nick, channel->name);
#endif
        /* clear this channel of client's list */
        last = client->channels - 1;
        for (i = 0; i < client->channels; i++)
          if (client->channel[i] == channel)
            {
              if (--client->channels != 0)
                {
                  client->channel[i] = client->channel[last];
                  client->channel = svz_realloc (client->channel,
                                                 sizeof (irc_channel_t *) *
                                                 client->channels);
                }
              else
                {
                  svz_free (client->channel);
                  client->channel = NULL;
                }
              break;
            }
        break;
      }
  /* no client in channel?  */
  if (channel->clients == 0)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "irc: channel %s destroyed\n", channel->name);
#endif
      irc_delete_channel (cfg, channel);
    }

  return 0;
}

/*
 * Send an error message if there are not enough arguments given.
 * Return non zero if there are less than necessary.
 */
int
irc_check_args (svz_socket_t *sock,      /* the client's socket */
                irc_client_t *client,    /* the irc client itself */
                irc_config_t *conf,      /* config hash */
                irc_request_t *request,  /* the request */
                int n)                   /* necessary arguments */
{
  if (request->paras < n)
    {
      irc_printf (sock, ":%s %03d %s " ERR_NEEDMOREPARAMS_TEXT "\n",
                  conf->host, ERR_NEEDMOREPARAMS, client->nick,
                  request->request);
      return -1;
    }
  return 0;
}

/*
 * This routine checks if a given client is away or not, then
 * sends this clients away message back to the client which requested
 * this.  Return non-zero if away.
 */
int
irc_client_absent (irc_client_t *client,  /* requested client */
                   irc_client_t *rclient) /* who want to know about */
{
  irc_config_t *cfg;
  svz_socket_t *sock;

  if (client->flag & UMODE_AWAY)
    {
      sock = rclient->sock;
      cfg = sock->cfg;
      irc_printf (sock, ":%s %03d %s " RPL_AWAY_TEXT "\n",
                  cfg->host, RPL_AWAY, rclient->nick, client->nick,
                  client->away);
      return -1;
    }
  return 0;
}

/*
 * This routine erases a client from all channels by a quit reason,
 * then the client is deleted itself.
 */
int
irc_leave_all_channels (irc_config_t *cfg,
                        irc_client_t *client, char *reason)
{
  irc_channel_t *channel;
  irc_client_t *cl;
  svz_socket_t *sock, *xsock;
  int i;

  sock = client->sock;

  /* go through all channels */
  while (client->channels)
    {
      channel = client->channel[0];

      /* tell all clients in the channel about disconnecting */
      for (i = 0; i < channel->clients; i++)
        {
          if (channel->client[i] == client)
            continue;

          cl = channel->client[i];
          xsock = cl->sock;
          irc_printf (xsock, ":%s!%s@%s QUIT :%s\n",
                      client->nick, client->user, client->host, reason);
        }

      /* delete this client of channel */
      irc_leave_channel (cfg, client, channel);
    }

  /* send last error Message */
  sock->flags &= ~SVZ_SOFLG_KILLED;
  irc_printf (sock, "ERROR :" IRC_CLOSING_LINK "\n", client->host, reason);
  sock->flags |= SVZ_SOFLG_KILLED;

  /* delete this client */
  irc_delete_client (cfg, client);
  sock->data = NULL;

  return 0;
}

/*
 * This function is the default callback if the connection of
 * an IRC client gets lost.
 */
int
irc_disconnect (svz_socket_t *sock)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *client = sock->data;

  /* is it a valid IRC connection?  */
  if (client)
    {
      irc_leave_all_channels (cfg, client, IRC_CONNECTION_LOST);
    }
  return 0;
}

/*
 * This is the idle callback for serveez.  Here the IRC server could
 * send a PING to a IRC client.
 */
int
irc_idle (svz_socket_t *sock)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *client = sock->data;

  if (!client->registered)
    {
      if (irc_register_client (sock, client, cfg))
        return -1;
      sock->idle_counter = 1;
      return 0;
    }

  /*
   * Shutdown this connection if the client did not respond
   * within a certain period of time.
   */
  if (client->ping > 0)
    {
      return -1;
    }

  /*
   * Ping a client connection if necessary.
   */
  if ((time (NULL) - sock->last_recv) >= IRC_PING_INTERVAL)
    {
      irc_printf (sock, "PING %s\n", cfg->host);
      client->ping++;
    }

  sock->idle_counter = IRC_PING_INTERVAL;
  return 0;
}

/*
 * This structures field contains all the callback routines necessary
 * to react like an IRC server.  The actual routines are defined in the
 * irc-event.h and implemented in the irc-event-?.c files.
 */
irc_callback_t irc_callback[] =
{
  { 0, "OPER",     irc_oper_callback     },
  { 0, "INFO",     irc_info_callback     },
  { 0, "KILL",     irc_kill_callback     },
  { 0, "ERROR",    irc_error_callback    },
  { 0, "WHOWAS",   irc_whowas_callback   },
  { 0, "ADMIN",    irc_admin_callback    },
  { 0, "TIME",     irc_time_callback     },
  { 0, "LUSERS",   irc_lusers_callback   },
  { 0, "STATS",    irc_stats_callback    },
  { 0, "PING",     irc_ping_callback     },
  { 0, "PONG",     irc_pong_callback     },
  { 0, "VERSION",  irc_version_callback  },
  { 0, "KICK",     irc_kick_callback     },
  { 0, "AWAY",     irc_away_callback     },
  { 0, "WHO",      irc_who_callback      },
  { 0, "WHOIS",    irc_whois_callback    },
  { 0, "MOTD",     irc_motd_callback     },
  { 0, "INVITE",   irc_invite_callback   },
  { 0, "LIST",     irc_list_callback     },
  { 0, "NAMES",    irc_names_callback    },
  { 0, "NOTICE",   irc_note_callback     },
  { 0, "TOPIC",    irc_topic_callback    },
  { 0, "MODE",     irc_mode_callback     },
  { 0, "PRIVMSG",  irc_priv_callback     },
  { 0, "USERHOST", irc_userhost_callback },
  { 0, "ISON",     irc_ison_callback     },
  { 0, "USERS",    irc_users_callback    },
  { 0, "PART",     irc_part_callback     },
  { 0, "QUIT",     irc_quit_callback     },
  { 0, "JOIN",     irc_join_callback     },
  { 0, "PASS",     irc_pass_callback     },
  { 0, "USER",     irc_user_callback     },
  { 0, "NICK",     irc_nick_callback     },
  { 0, NULL,       NULL                  }
};

int
irc_handle_request (svz_socket_t *sock, char *request, int len)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *client = sock->data;
  int n;
  size_t rlen;

  irc_parse_request (request, len);
  rlen = strlen (irc_request.request);

  /*
   * FIXME: server handling not yet done.
   * Should be alike: get irc client by message prefix, otherwise special
   *                  server request handling
   */
  if (sock->userflags & IRC_FLAG_SERVER)
    return 0;

  for (n = 0; irc_callback[n].request; n++)
    {
      if (!strncasecmp (irc_callback[n].request, irc_request.request, rlen))
        {
          irc_callback[n].count++;
          client->recv_bytes += len;
          client->recv_packets++;
          return irc_callback[n].func (sock, client, &irc_request);
        }
    }

  irc_printf (sock, ":%s %03d %s " ERR_UNKNOWNCOMMAND_TEXT "\n",
              cfg->host, ERR_UNKNOWNCOMMAND,
              client->nick ? client->nick : "", irc_request.request);

  return 0;
}

/*
 * Delete a channel from the channel list.  Returns -1 if there was no
 * appropriate channel.
 */
static int
irc_delete_channel (irc_config_t *cfg, irc_channel_t *channel)
{
  int n;

  if (svz_hash_contains (cfg->channels, channel))
    {
      /* ‘svz_free’ all the channel ban entries */
      for (n = 0; n < channel->bans; n++)
        irc_destroy_ban (channel->ban[n]);
      if (channel->ban)
        svz_free (channel->ban);

      svz_hash_delete (cfg->channels, channel->name);

      if (channel->topic)
        svz_free (channel->topic);
      if (channel->topic_by)
        svz_free (channel->topic_by);
      if (channel->key)
        svz_free (channel->key);
      if (channel->invite)
        svz_free (channel->invite);

      svz_free (channel->by);
      svz_free (channel->name);
      svz_free (channel);
      return 0;
    }
  return -1;
}

/*
 * Find a channel within the current channel list.  Return NULL if
 * the channel has not been found.
 */
irc_channel_t *
irc_find_channel (irc_config_t *cfg, char *channel)
{
  irc_channel_t *chan;

  chan = svz_hash_get (cfg->channels, channel);
  return chan;
}

struct regex_channel_closure
{
  char *regex;
  irc_channel_t **fchannel;
  int found;
};

static void
regex_channel_internal (UNUSED void *k, void *v, void *closure)
{
  irc_channel_t *ch = v;
  struct regex_channel_closure *x = closure;

  if (irc_string_regex (ch->name, x->regex))
    x->fchannel[(x->found)++] = ch;
}

/*
 * Find all matching channels in the current channel list.  Return NULL if
 * no channel has not been found.  You MUST ‘svz_free’ this list if non-NULL.
 * The delivered array is NULL terminated.
 */
irc_channel_t **
irc_regex_channel (irc_config_t *cfg, char *regex)
{
  size_t size;

  if ((size = svz_hash_size (cfg->channels)))
    {
      irc_channel_t **fchannel = svz_malloc (sizeof (irc_channel_t *)
                                             * (size + 1));
      struct regex_channel_closure x = { regex, fchannel, 0 };

      svz_hash_foreach (regex_channel_internal, cfg->channels, &x);

      /* return NULL if there is not channel */
      if (!x.found)
        {
          svz_free (x.fchannel);
          return NULL;
        }

      fchannel[x.found++] = NULL;
      fchannel = svz_realloc (fchannel, sizeof (irc_channel_t *) * x.found);
      return fchannel;
    }
  return NULL;
}

/*
 * Add a new channel to the channel list.
 */
static irc_channel_t *
irc_add_channel (irc_config_t *cfg, char *name)
{
  irc_channel_t *channel;

  if (irc_find_channel (cfg, name))
    return NULL;

  channel = svz_calloc (sizeof (irc_channel_t));
  channel->name = svz_strdup (name);
  svz_hash_put (cfg->channels, name, channel);
  return channel;
}

/*
 * Add a client to the client history list.
 */
void
irc_add_client_history (irc_config_t *cfg, irc_client_t *cl)
{
  irc_client_history_t *client;

  client = svz_malloc (sizeof (irc_client_history_t));
  client->nick = svz_strdup (cl->nick);
  client->user = svz_strdup (cl->user);
  client->host = svz_strdup (cl->host);
  client->real = svz_strdup (cl->real);
  client->next = cfg->history;
  cfg->history = client;
}

/*
 * Find a nick in the history client list.  Return NULL if
 * no nick has not been found.  Otherwise the first client found within
 * the history list.
 */
irc_client_history_t *
irc_find_nick_history (irc_config_t *cfg,
                       irc_client_history_t *cl, char *nick)
{
  irc_client_history_t *client;

  client = cl ? cl->next : cfg->history;
  for (; client; client = client->next)
    {
      if (!irc_string_equal (client->nick, nick))
        {
          return client;
        }
    }
  return NULL;
}

/*
 * Delete all the client history.
 */
void
irc_delete_client_history (irc_config_t *cfg)
{
  irc_client_history_t *client;
  irc_client_history_t *old;

  for (client = cfg->history; client; client = old)
    {
      old = client->next;
      svz_free (client->nick);
      svz_free (client->user);
      svz_free (client->host);
      svz_free (client->real);
      svz_free (client);
    }
  cfg->history = NULL;
}

/*
 * Delete a client from the client list.  Returns -1 if there was no
 * appropriate client.
 */
int
irc_delete_client (irc_config_t *cfg, irc_client_t *client)
{
  int ret = 0;

  if (client->nick)
    {
      /* put this client into the history list */
      irc_add_client_history (cfg, client);
      if (svz_hash_delete (cfg->clients, client->nick) == NULL)
        ret = -1;
      svz_free (client->nick);
    }

  /* free all client properties */
  if (client->real)
    svz_free (client->real);
  if (client->user)
    svz_free (client->user);
  if (client->host)
    svz_free (client->host);
  if (client->server)
    svz_free (client->server);
  if (client->channel)
    svz_free (client->channel);
  if (client->pass)
    svz_free (client->pass);
  if (client->away)
    svz_free (client->away);
  svz_free (client);
  cfg->users--;

  return ret;
}

struct find_userhost_closure
{
  char *user;
  char *host;
  irc_client_t *found;
};

static void
find_userhost_internal (UNUSED void *k, void *v, void *closure)
{
  irc_client_t *client = v;
  struct find_userhost_closure *x = closure;

  if (x->found)
    return;

  if (!strcmp (client->user, x->user) &&
      !strcmp (client->host, x->host))
    x->found = client;
}

/*
 * Find a user@host within the current client list.  Return NULL if
 * no client has not been found.
 */
irc_client_t *
irc_find_userhost (irc_config_t *cfg, char *user, char *host)
{
  if (svz_hash_size (cfg->clients))
    {
      struct find_userhost_closure x = { user, host, NULL };

      svz_hash_foreach (find_userhost_internal, cfg->clients, &x);
      return x.found;
    }
  return NULL;
}

/*
 * Find a nick within the current client list.  Return NULL if
 * the nick has not been found.
 */
irc_client_t *
irc_find_nick (irc_config_t *cfg, char *nick)
{
  irc_client_t *client;

  if ((client = svz_hash_get (cfg->clients, nick)) != NULL)
    {
      return client;
    }
  return NULL;
}

struct regex_nick_closure
{
  char *regex;
  irc_client_t **fclient;
  int found;
};

static void
regex_nick_internal (UNUSED void *k, void *v, void *closure)
{
  irc_client_t *client = v;
  struct regex_nick_closure *x = closure;

  if (irc_string_regex (client->nick, x->regex))
    x->fclient[x->found++] = client;
}

/*
 * Find all matching nicks in the current client list.  Return NULL if
 * no nick has not been found.  You MUST ‘svz_free’ this array if it is
 * non-NULL.  The delivered clients are NULL terminated.
 */
irc_client_t **
irc_regex_nick (irc_config_t *cfg, char *regex)
{
  size_t size;

  if ((size = svz_hash_size (cfg->clients)))
    {
      irc_client_t **fclient = svz_malloc (sizeof (irc_client_t *)
                                           * (size + 1));
      struct regex_nick_closure x = { regex, fclient, 0 };

      svz_hash_foreach (regex_nick_internal, cfg->clients, &x);

      /* return NULL if there is not client */
      if (!x.found)
        {
          svz_free (fclient);
          return NULL;
        }

      fclient[x.found++] = NULL;
      fclient = svz_realloc (fclient, sizeof (irc_client_t *) * x.found);
      return fclient;
    }
  return NULL;
}

/*
 * Add a new client to the client list.
 */
irc_client_t *
irc_add_client (irc_config_t *cfg, irc_client_t *client)
{
  if (irc_find_nick (cfg, client->nick))
    return NULL;

  svz_hash_put (cfg->clients, client->nick, client);
  return client;
}

/*
 * Create a new IRC client structure.  This will be stored within the
 * miscellaneous data field in the socket structure (sock->data).
 */
irc_client_t *
irc_create_client (irc_config_t *cfg)
{
  irc_client_t *client;

  client = svz_calloc (sizeof (irc_client_t));
  cfg->users++;
  return client;
}

#define VSNPRINTF_BUF_SIZE  2048

/*
 * Print a formatted string to the socket SOCK.
 */
int
irc_printf (svz_socket_t *sock, const char *fmt, ...)
{
  va_list args;
  static char buffer[VSNPRINTF_BUF_SIZE];
  unsigned len;

  if (sock->flags & SVZ_SOFLG_KILLED)
    return 0;

  va_start (args, fmt);
  len = vsnprintf (buffer, VSNPRINTF_BUF_SIZE, fmt, args);
  va_end (args);

  /* Just to be sure...  */
  if (len > sizeof (buffer))
    len = sizeof (buffer);

  if ((len = svz_sock_write (sock, buffer, len)) != 0)
    {
      sock->flags |= SVZ_SOFLG_KILLED;
    }
  return len;
}
