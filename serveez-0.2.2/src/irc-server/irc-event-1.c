/*
 * irc-event-1.c - IRC events -- Connection Registration
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2003 Stefan Jahn <stefan@lkcc.org>
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
#include <errno.h>
#include <sys/stat.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_CRYPT_H
# include <crypt.h>
#endif
#include "networking-headers.h"
#include "libserveez.h"
#include "irc-core/irc-core.h"
#include "irc-proto.h"
#include "irc-crypt.h"
#include "irc-event.h"
#include "irc-config.h"
#include "unused.h"

#include "timestamp.c"                  /* for ‘irc_send_init_block’ */

/*
 *    Command: QUIT
 * Parameters: [<Quit message>]
 */
int
irc_quit_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  /* delete the client */
  irc_leave_all_channels (sock->cfg, client, request->para[0]);

  return -1;
}

/*
 *         Command: PASS
 *      Parameters: <password>
 * Numeric Replies: ERR_NEEDMOREPARAMS ERR_ALREADYREGISTRED
 */
int
irc_pass_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;

  /* enough paras?  reject the client if not given */
  if (irc_check_args (sock, client, cfg, request, 1))
    return -1;

  if (client->pass)
    svz_free (client->pass);
  client->pass = svz_strdup (request->para[0]);
  client->key = irc_gen_key (client->pass);
  client->flag |= UMODE_PASS;

  /* check it!  */
  if (cfg->pass)
    {
#if defined HAVE_CRYPT
      if (strcmp (crypt (request->para[0], cfg->pass), cfg->pass))
#else
      if (strcmp (request->para[0], cfg->pass))
#endif
        {
          irc_delete_client (cfg, client);
          return -1;
        }
    }

  return 0;
}

/*
 * Send the initial messages to a new IRC client.
 */
static void
irc_send_init_block (svz_socket_t *sock, irc_client_t *client)
{
  irc_config_t *cfg = sock->cfg;

  /* send initial messages */
  irc_printf (sock, ":%s %03d %s :" RPL_WELCOME_TEXT "\n",
              cfg->host, RPL_WELCOME, client->nick, client->nick);

  irc_printf (sock, ":%s %03d %s :" RPL_YOURHOST_TEXT "\n",
              cfg->host, RPL_YOURHOST, client->nick,
              cfg->host, PACKAGE_TARNAME, PACKAGE_VERSION);

  irc_printf (sock, "NOTICE %s :*** " RPL_YOURHOST_TEXT "\n",
              client->nick, cfg->host, PACKAGE_TARNAME, PACKAGE_VERSION);

  irc_printf (sock, ":%s %03d %s :" RPL_CREATED_TEXT "\n",
              cfg->host, RPL_CREATED, client->nick, created);

  irc_printf (sock, ":%s %03d %s " RPL_MYINFO_TEXT "\n",
              cfg->host, RPL_MYINFO, client->nick,
              cfg->host, PACKAGE_TARNAME, PACKAGE_VERSION,
              USER_MODES, CHANNEL_MODES);

  /* send LUSER* replies */
  irc_lusers_callback (sock, client, NULL);

  /* send the "Message of the Day" */
  irc_motd_callback (sock, client, NULL);
}

/*
 * If a client has been fully registered and identified it gets
 * validated by the config lines.
 */
int
irc_register_client (svz_socket_t *sock,
                     irc_client_t *client, irc_config_t *cfg)
{
  if ((client->flag & UMODE_REGISTERED) == UMODE_REGISTERED)
    {
      if (!irc_client_valid (client, cfg))
        return -1;
      irc_add_client (cfg, client);
      irc_send_init_block (sock, client);
      client->registered = 1;
    }
  return 0;
}

/*
 * This function extracts a valid nick of a given text.  It returns
 * the length of it, otherwise zero.
 */
static int
irc_get_nick (char *nick)
{
  char *p;
  int n;

  p = nick;

  if ((*p >= '0' && *p <= '9') || *p == '-')
    return 0;

  for (n = 0; *p && n < MAX_NICK_LEN; n++, p++)
    if (!((*p >= 'A' && *p <= '~') ||  (*p >= '0' && *p <= '9') ||
          (*p == '_') || (*p == '-')))
      break;
  *p = 0;

  return p - nick;
}

/*
 *         Command: NICK
 *      Parameters: <nickname> [ <hopcount> ]
 * Numeric Replies: ERR_NONICKNAMEGIVEN ERR_ERRONEUSNICKNAME
 *                  ERR_NICKNAMEINUSE   ERR_NICKCOLLISION
 */
int
irc_nick_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *cl;
  irc_channel_t *channel;
  svz_socket_t *xsock;
  char *nick;
  int n, i;

  /* enough para's?  */
  if (request->paras < 1)
    {
      irc_printf (sock, ":%s %03d " ERR_NONICKNAMEGIVEN_TEXT "\n",
                  cfg->host, ERR_NONICKNAMEGIVEN);
      return 0;
    }

  /* is the given nick valid?  */
  nick = request->para[0];
  if (!irc_get_nick (nick))
    {
      irc_printf (sock, ":%s %03d * " ERR_ERRONEUSNICKNAME_TEXT "\n",
                  cfg->host, ERR_ERRONEUSNICKNAME, nick);
      return 0;
    }

  /* nick already in use?  */
  if ((cl = irc_find_nick (cfg, nick)) != NULL)
    {
      /* did the client tried to change to equal nicks?  then ignore */
      if (cl == client)
        return 0;
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "irc: nick %s is already in use\n", cl->nick);
#endif
      irc_printf (sock, ":%s %03d * " ERR_NICKNAMEINUSE_TEXT "\n",
                  cfg->host, ERR_NICKNAMEINUSE, cl->nick);
      return 0;
    }

  /* do you have already specified a valid nick?  */
  if (client->flag & UMODE_NICK)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "irc: %s changed nick to %s\n",
               client->nick, nick);
#endif
      /* is the client fully registered?  */
      if (client->registered)
        {
          /* go through all channels this client is in */
          for (n = 0; n < client->channels; n++)
            {
              /* propagate this to all clients in channel */
              channel = client->channel[n];
              for (i = 0; i < channel->clients; i++)
                {
                  cl = channel->client[i];
                  xsock = cl->sock;
                  irc_printf (xsock, ":%s!%s@%s NICK :%s\n",
                              client->nick, client->user, client->host, nick);
                }
            }
          /* replace nick in client hash */
          if (svz_hash_delete (cfg->clients, client->nick) != client)
            {
              svz_log (SVZ_LOG_ERROR, "irc: client hash inconsistence\n");
            }
          svz_hash_put (cfg->clients, nick, client);
        }

      svz_free (client->nick);
    }

  /* this is the first nick you specified!  */
  client->nick = svz_strdup (nick);
  client->flag |= UMODE_NICK;

  return 0;
}

/*
 *         Command: USER
 *      Parameters: <username> <hostname> <servername> <realname>
 * Numeric Replies: ERR_NEEDMOREPARAMS ERR_ALREADYREGISTRED
 */
int
irc_user_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;

  /* complete parameter list?  */
  if (irc_check_args (sock, client, cfg, request, 4))
    return 0;

  /* is this client already fully registered?  */
  if (client->flag & UMODE_USER)
    {
      irc_printf (sock, ":%s %03d %s " ERR_ALREADYREGISTRED_TEXT "\n",
                  cfg->host, ERR_ALREADYREGISTRED, client->nick);
      return 0;
    }

  /* store paras in client structure if not done by AUTH-callbacks */
  if (!client->user)
    {
      client->user = svz_malloc (strlen (request->para[0]) + 2);
      sprintf (client->user, "~%s", request->para[0]);
    }
  if (!client->host)
    client->host = svz_strdup (request->para[1]);
  if (!client->server)
    client->server = svz_strdup (request->para[2]);
  if (!client->real)
    client->real = svz_strdup (request->para[3]);
  client->flag |= UMODE_USER;

  return 0;
}

/*
 *         Command: MOTD
 *      Parameters:
 * Numeric Replies: ERR_NOMOTD    RPL_MOTDSTART
 *                  RPL_ENDOFMOTD RPL_MOTD
 */
int
irc_motd_callback (svz_socket_t *sock,
                   irc_client_t *client,
                   UNUSED irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  FILE *f;
  struct stat buf;
  int n;

  /* try requesting the file */
  if (stat (cfg->MOTD_file, &buf) == -1)
    {
      svz_log_sys_error ("irc: /MOTD error (%s)", cfg->MOTD_file);
      irc_printf (sock, ":%s %03d %s " ERR_NOMOTD_TEXT "\n",
                  cfg->host, ERR_NOMOTD, client->nick);
      return 0;
    }

  /* has the file been changed?  then read it */
  if (cfg->MOTD_lastModified <  buf.st_mtime)
    {
      cfg->MOTD_lastModified =  buf.st_mtime;
      if ((f = fopen (cfg->MOTD_file, "r")) == NULL)
        {
          svz_log_sys_error ("irc: /MOTD error");
          return 0;
        }

      /* free the previous MOTD content */
      for (n = 0; n < cfg->MOTDs; n++)
        svz_free (cfg->MOTD[n]);

      /* read every line (restrict line length) */
      n = 0;
      cfg->MOTD[n] = svz_malloc (MOTD_LINE_LEN);
      while (fgets (cfg->MOTD[n], MOTD_LINE_LEN, f) && n < MAX_MOTD_LINES)
        {
          cfg->MOTD[n][strlen (cfg->MOTD[n]) - 1] = '\0';
          n++;
          cfg->MOTD[n] = svz_malloc (MOTD_LINE_LEN);
        }
      svz_free (cfg->MOTD[n]);
      cfg->MOTDs = n;
      fclose (f);
    }

  /* send the "Message of the Day" if necessary */
  if (cfg->MOTDs)
    {
      /* start */
      irc_printf (sock,
                  "NOTICE %s :*** The MOTD file was last modified at %s\n",
                  client->nick, svz_time (cfg->MOTD_lastModified));

      irc_printf (sock, ":%s %03d %s " RPL_MOTDSTART_TEXT "\n",
                  cfg->host, RPL_MOTDSTART, client->nick, cfg->host);

      /* go through all lines */
      for (n = 0; n < cfg->MOTDs; n++)
        {
          irc_printf (sock, ":%s %03d %s " RPL_MOTD_TEXT "\n",
                      cfg->host, RPL_MOTD, client->nick, cfg->MOTD[n]);
        }

      /* end */
      irc_printf (sock, ":%s %03d %s " RPL_ENDOFMOTD_TEXT "\n",
                  cfg->host, RPL_ENDOFMOTD, client->nick, cfg->host);
    }

  return 0;
}

/*
 *         Command: OPER
 *      Parameters: <user> <password>
 * Numeric Replies: ERR_NEEDMOREPARAMS RPL_YOUREOPER
 *                  ERR_NOOPERHOST     ERR_PASSWDMISMATCH
 */
int
irc_oper_callback (svz_socket_t *sock,
                   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;

  /* did the client send a complete parameter list?  */
  if (irc_check_args (sock, client, cfg, request, 2))
    return 0;

  /* copy both parameters into client structure */
  strcpy (client->pass, request->para[1]);
  if (!client->user[0])
    strcpy (client->user, request->para[0]);

  /* check if this client may be an IRC operator */
  if (irc_oper_valid (client, cfg))
    {
      cfg->operators++;
      client->flag |= UMODE_OPERATOR;
      irc_printf (sock, ":%s %03d %s " RPL_YOUREOPER_TEXT "\n",
                  cfg->host, RPL_YOUREOPER, client->nick);
    }
  else
    {
      irc_printf (sock, ":%s %03d %s " ERR_NOOPERHOST_TEXT "\n",
                  cfg->host, ERR_NOOPERHOST, client->nick);
    }
  return 0;
}
