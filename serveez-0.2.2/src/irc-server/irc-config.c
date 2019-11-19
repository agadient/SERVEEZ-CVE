/*
 * irc-config.c - IRC server configuration routines
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
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef HAVE_CRYPT_H
# include <crypt.h>
#endif
#include "networking-headers.h"
#include <libserveez.h>
#include "irc-proto.h"
#include "irc-server.h"
#include "irc-config.h"

/*
 * This routine will parse all of the x-lines of the IRC server
 * configuration and store the information within the appropriate
 * lists.
 */

#define MAX_TMP_ARRAY  4
#define MAX_TMP_STRLEN 256
#define PARSE_TILL_AT(str) \
  p = str; \
  while (*p && *p != '@') p++; \
  if (*p) { *p = '\0'; p++; }

void
irc_parse_config_lines (irc_config_t *cfg)
{
  size_t n;
  irc_class_t *class;
  irc_user_t *user;
  irc_oper_t *oper;
  irc_kill_t *kill;
  char *line, *p;
  char *tmp[MAX_TMP_ARRAY];

  /* reserve some buffer space */
  for (n = 0; n < MAX_TMP_ARRAY; n++)
    tmp[n] = svz_malloc (MAX_TMP_STRLEN);

#define TB(i)                                           \
  /* Note: This is deliberately two expressions,        \
     separated by a comma.  Do not parenthesize!  */    \
  MAX_TMP_STRLEN, tmp[i]

  /* parse connection classes */
  svz_array_foreach (cfg->YLine, line, n)
    {
      class = svz_malloc (sizeof (irc_class_t));
      if (5 != irc_parse_line (line, "Y:%d:%d:%d:%d:%d",
                               &class->nr, &class->ping_freq,
                               &class->connect_freq, &class->max_links,
                               &class->sendq_size))
        {
          svz_log (SVZ_LOG_ERROR, "irc: invalid Y line: %s\n", line);
          svz_free (class);
        }
      else
        {
          class->links = 0;
          class->line = line;
          class->next = cfg->classes;
          cfg->classes = class;
        }
    }

  /* parse user authorization lines */
  svz_array_foreach (cfg->ILine, line, n)
    {
      user = svz_malloc (sizeof (irc_user_t));
      if (5 != irc_parse_line (line, "I:%s:%s:%s:%s:%d",
                               TB (0), TB (1), TB (2), TB (3),
                               &user->class))
        {
          svz_log (SVZ_LOG_ERROR, "irc: invalid I line: %s\n", line);
          svz_free (user);
        }
      else
        {
          user->line = line;
          PARSE_TILL_AT (tmp[0]);
          user->user_ip = svz_strdup (tmp[0]);
          user->ip = svz_strdup (p);
          user->password = svz_strdup (tmp[1]);
          PARSE_TILL_AT (tmp[2]);
          user->user_host = svz_strdup (tmp[2]);
          user->host = svz_strdup (p);
          if (!user->password)
            user->password = svz_strdup (tmp[3]);
          user->next = cfg->user_auth;
          cfg->user_auth = user;
        }
    }

  /* parse operator authorization lines (local and network wide) */
  svz_array_foreach (cfg->OLine, line, n)
    {
      oper = svz_malloc (sizeof (irc_oper_t));
      if (4 != irc_parse_line (line, "O:%s:%s:%s::%d",
                               TB (0), TB (1), TB (2), &oper->class))
        {
          svz_log (SVZ_LOG_ERROR, "irc: invalid O line: %s\n", line);
          svz_free (oper);
        }
      else
        {
          oper->line = line;
          PARSE_TILL_AT (tmp[0]);
          oper->user = svz_strdup (tmp[0]);
          oper->host = svz_strdup (p);
          oper->password = svz_strdup (tmp[1]);
          oper->nick = svz_strdup (tmp[2]);
          oper->local = 0;
          oper->next = cfg->operator_auth;
          cfg->operator_auth = oper;
        }
    }

  svz_array_foreach (cfg->oLine, line, n)
    {
      oper = svz_malloc (sizeof (irc_oper_t));
      if (4 != irc_parse_line (line, "O:%s:%s:%s::%d",
                               TB (0), TB (1), TB (2), &oper->class))
        {
          svz_log (SVZ_LOG_ERROR, "irc: invalid o line: %s\n", line);
          svz_free (oper);
        }
      else
        {
          oper->line = line;
          PARSE_TILL_AT (tmp[0]);
          oper->user = svz_strdup (tmp[0]);
          oper->host = svz_strdup (p);
          oper->password = svz_strdup (tmp[1]);
          oper->nick = svz_strdup (tmp[2]);
          oper->local = 1;
          oper->next = cfg->operator_auth;
          cfg->operator_auth = oper;
        }
    }

  /* parse banned clients */
  svz_array_foreach (cfg->KLine, line, n)
    {
      kill = svz_malloc (sizeof (irc_kill_t));
      if (4 != irc_parse_line (line, "O:%s:%d-%d:%s",
                               TB (0), &kill->start, &kill->end, TB (1)))
        {
          svz_log (SVZ_LOG_ERROR, "irc: invalid K line: %s\n", line);
          svz_free (kill);
        }
      else
        {
          kill->line = line;
          kill->host = svz_strdup (tmp[0]);
          kill->user = svz_strdup (tmp[1]);
          kill->next = cfg->banned;
          cfg->banned = kill;
        }
    }

  /* free the previously allocated buffer space */
  for (n = 0; n < MAX_TMP_ARRAY; n++)
    svz_free (tmp[n]);

#undef TB
}

/*
 * Free all the stuff reserved in the routine above.
 */
void
irc_free_config_lines (irc_config_t *cfg)
{
  irc_user_t *user;
  irc_class_t *class;
  irc_oper_t *oper;
  irc_kill_t *kill;

  /* free connection classes */
  while ((class = cfg->classes) != NULL)
    {
      cfg->classes = class->next;
      svz_free (class);
    }

  /* free user authorization list */
  while ((user = cfg->user_auth) != NULL)
    {
      cfg->user_auth = user->next;
      if (user->user_ip)
        svz_free (user->user_ip);
      if (user->ip)
        svz_free (user->ip);
      if (user->user_host)
        svz_free (user->user_host);
      if (user->host)
        svz_free (user->host);
      if (user->password)
        svz_free (user->password);
      svz_free (user);
    }

  /* free operator authorization list */
  while ((oper = cfg->operator_auth) != NULL)
    {
      cfg->operator_auth = oper->next;
      if (oper->nick)
        svz_free (oper->nick);
      if (oper->user)
        svz_free (oper->user);
      if (oper->host)
        svz_free (oper->host);
      if (oper->password)
        svz_free (oper->password);
      svz_free (oper);
    }

  /* free banned user list */
  while ((kill = cfg->banned) != NULL)
    {
      cfg->banned = kill->next;
      if (kill->user)
        svz_free (kill->user);
      if (kill->host)
        svz_free (kill->host);
      svz_free (kill);
    }
}

/*
 * The following routine checks whether it is still possible to connect
 * via a given connection class number.  It returns zero on success or if
 * there no such connection class, otherwise non-zero.
 */
static int
irc_check_class (irc_config_t *cfg, int class_nr)
{
  irc_class_t *class;

  for (class = cfg->classes; class; class = class->next)
    {
      if (class->nr == class_nr)
        {
          if (class->links++ < class->max_links)
            return 0;
          else
            {
#if ENABLE_DEBUG
              svz_log (SVZ_LOG_DEBUG, "irc: %d/%d links reached in class %d\n",
                       class->links, class->max_links, class->nr);
#endif
              return -1;
            }
        }
    }
  return 0;
}

/*
 * This functions goes through all the K lines to check whether a given
 * client is unwanted.  Return non-zero if so.
 */
static int
irc_client_killed (irc_client_t *client, irc_config_t *cfg)
{
  irc_kill_t *kill;
  time_t t;
  struct tm *tm;
  int ts;

  for (kill = cfg->banned; kill; kill = kill->next)
    {
      if (irc_string_regex (client->user, kill->user) &&
          irc_string_regex (client->host, kill->host))
        {
          t = time (NULL);
          tm = localtime (&t);
          ts = (tm->tm_hour + 1) * 100 + tm->tm_min;
          if (ts >= kill->start && ts <= kill->end)
            {
#if ENABLE_DEBUG
              svz_log (SVZ_LOG_DEBUG, "irc: %s@%s is K lined: %s\n",
                       client->user, client->host, kill->line);
              return -1;
#endif
            }
        }
    }
  return 0;
}

/*
 * This routine checks whether a given client is wanted to connect.
 * If it is not return zero, otherwise non-zero.
 */
int
irc_client_valid (irc_client_t *client, irc_config_t *cfg)
{
  char buf[64];
  irc_user_t *user;

  /* first we check whether this client is within the K lines */
  if (irc_client_killed (client, cfg))
    {
      irc_printf (client->sock, "%s %03d %s " ERR_YOUREBANNEDCREEP_TEXT "\n",
                  cfg->host, ERR_YOUREBANNEDCREEP, client->nick);
      return 0;
    }

  /* have a look at the user authorization list (I lines) */
  for (user = cfg->user_auth; user; user = user->next)
    {
      if ((irc_string_regex (client->user, user->user_ip) &&
           irc_string_regex (SVZ_PP_ADDR (buf, client->sock->remote_addr),
                             user->ip)) ||
          (irc_string_regex (client->user, user->user_host) &&
           irc_string_regex (client->host, user->host)))
        {
          /* test the given password for that I line */
          if (user->password)
            {
#if defined HAVE_CRYPT
              if (strcmp (crypt (client->pass, user->password),
                          user->password))
#else
              if (strcmp (client->pass, user->password))
#endif
                {
                  irc_printf (client->sock, ":%s %03d %s "
                              ERR_PASSWDMISMATCH_TEXT "\n",
                              cfg->host, ERR_PASSWDMISMATCH, client->nick);
                  return 0;
                }
            }

          /* now we have a look at the connection classes */
          if (irc_check_class (cfg, user->class))
            continue;

#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "irc: valid client: %s\n", user->line);
#endif
          return 1;
        }
    }
#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "irc: not a valid client (%s@%s)\n",
           client->user, client->host);
#endif

  return 0;
}

/*
 * The following function performs a check for the given client
 * being able to get IRC operator.  Return NULL on errors.
 */
int
irc_oper_valid (irc_client_t *client, irc_config_t *cfg)
{
  irc_oper_t *oper;

  for (oper = cfg->operator_auth; oper; oper = oper->next)
    {
      if (irc_string_regex (client->user, oper->user) &&
          irc_string_regex (client->host, oper->host) &&
          irc_string_regex (client->nick, oper->nick) &&
#if defined HAVE_CRYPT
          !strcmp (crypt (client->pass, oper->password), oper->password))
#else
          !strcmp (client->pass, oper->password))
#endif
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "irc: valid operator: %s\n", oper->line);
#endif
          return -1;
        }
    }

#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "irc: not a valid operator (%s@%s)\n",
           client->user, client->host);
#endif

  return 0;
}
