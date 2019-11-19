/*
 * irc-core.c - IRC core protocol functions
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
#include <ctype.h>
#include <time.h>
#include "networking-headers.h"
#include "libserveez.h"
#include "irc-core.h"
#include "irc-server/irc-proto.h"
#include "unused.h"

irc_request_t irc_request; /* single IRC request */
char irc_lcset[256];       /* lower case character set */

/*
 * Gets called when a nslookup coserver has resolved a IP address
 * for socket SOCK.
 */
static int
irc_rdns_done (char *host, void *closure)
{
  irc_client_t *client;
  svz_sock_iv_t *x = closure;
  svz_socket_t *sock = svz_sock_find (x->id, x->version);

  svz_free (x);
  if (sock)
    {
      client = sock->data;
      client->flag |= UMODE_DNS;
      if (host)
        {
          if (client->host)
            svz_free (client->host);
          client->host = svz_strdup (host);
          irc_printf (sock, "NOTICE AUTH :%s\n", IRC_DNS_DONE);
        }
      else
        {
          irc_printf (sock, "NOTICE AUTH :%s\n", IRC_DNS_NOREPLY);
        }
      return 0;
    }
  return -1;
}

/*
 * Gets called when an ident coserver has got a reply
 * for socket SOCK.
 */
static int
irc_ident_done (char *user, void *closure)
{
  irc_client_t *client;
  svz_sock_iv_t *x = closure;
  svz_socket_t *sock = svz_sock_find (x->id, x->version);

  svz_free (x);
  if (sock)
    {
      client = sock->data;
      client->flag |= UMODE_IDENT;
      if (user)
        {
          if (client->user)
            svz_free (client->user);
          client->user = svz_strdup (user);
          irc_printf (sock, "NOTICE AUTH :%s\n", IRC_IDENT_DONE);
        }
      else
        {
          irc_printf (sock, "NOTICE AUTH :%s\n", IRC_IDENT_NOREPLY);
        }
      return 0;
    }
  return -1;
}

#define ENQ_COSERVER_REQUEST(req,coserver)      \
  svz_coserver_ ## coserver ## _invoke          \
  (req, irc_ ## coserver ## _done,              \
   svz_make_sock_iv (sock))

/*
 * Initialization of the authentication (DNS and IDENT) for an
 * IRC client.
 */
static void
irc_start_auth (svz_socket_t *sock)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *client;

  /*
   * Create and initialize a local IRC client!  This is not yet within the
   * actual client hash.
   */
  client = irc_create_client (cfg);
  client->server = svz_strdup (cfg->host);
  client->since = time (NULL);
  client->sock = sock;
  sock->data = client;

  /* Set password flag, if there is not server password defined.  */
  if (!cfg->pass)
    client->flag |= UMODE_PASS;

  /* Start here the reverse-dns and ident lookup.  */
  ENQ_COSERVER_REQUEST (sock->remote_addr, rdns);
  irc_printf (sock, "NOTICE AUTH :" IRC_DNS_INIT "\n");

  ENQ_COSERVER_REQUEST (sock, ident);
  irc_printf (sock, "NOTICE AUTH :" IRC_IDENT_INIT "\n");
}

/*
 * Detection routine for the IRC protocol.  Returns no-zero if an
 * IRC connection has been detected.  Otherwise zero.
 */
int
irc_detect_proto (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  int ret = 0;

  if (sock->recv_buffer_fill >= 1 && sock->recv_buffer[0] == ':')
    {
      ret = 1;
    }
  else if (sock->recv_buffer_fill >= 4 &&
           (!memcmp (sock->recv_buffer, "PASS", 4) ||
            !memcmp (sock->recv_buffer, "NICK", 4) ||
            !memcmp (sock->recv_buffer, "USER", 4)))
    {
      ret = 4;
    }

  if (ret)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "irc protocol detected\n");
#endif
      return -1;
    }

  return 0;
}

/*
 * When a client connection has been identified by IRC_DETECT_PROTO
 * this routine is called to setup this socket for an IRC connection.
 */
int
irc_connect_socket (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  sock->check_request = irc_check_request;
  sock->disconnected_socket = irc_disconnect;
  sock->idle_func = irc_idle;
  sock->idle_counter = 1;
  irc_start_auth (sock);

  return 0;
}

/*
 * The CHECK_REQUEST looks through the receive buffer of the
 * IRC connection for complete messages and calls then the
 * HANDLE_REQUEST function.
 */
int
irc_check_request (svz_socket_t *sock)
{
  int retval = 0;
  int request_len = 0;
  char *p, *packet;

  p = sock->recv_buffer;
  packet = p;

  do
    {
      while (p < sock->recv_buffer + sock->recv_buffer_fill && *p != '\n')
        p++;

      if (*p == '\n' && p < sock->recv_buffer + sock->recv_buffer_fill)
        {
          p++;
          request_len += (p - packet);
#if 0
          svz_hexdump (stdout, "irc packet", sock->sock_desc,
                       packet, p - packet, 0);
#endif
          retval = irc_handle_request (sock, packet, p - packet -
                                       ((*(p - 2) == '\r') ? 2 : 1));
          packet = p;
        }
    }
  while (p < sock->recv_buffer + sock->recv_buffer_fill && !retval);

  if (request_len > 0 && request_len < sock->recv_buffer_fill)
    {
      memmove (sock->recv_buffer, packet,
               sock->recv_buffer_fill - request_len);
    }
  sock->recv_buffer_fill -= request_len;

  return retval;
}

/*
 * Parse the 'nr'th string (IRC targets could be channels, nicks, etc.)
 * by a given IRC parameter string.  All these strings should be separated
 * by colons (',').
 */
char *
irc_get_target (char *para, int nr)
{
  static char target[MAX_NAME_LEN];
  char *p;
  int n;

  target[0] = '\0';
  p = para;
  for (n = 0; *p && n < nr; n++)
    while (*p && *p != ',')
      p++;

  /* got a key (first or any ',' separated) */
  if (*p == ',' || p == para)
    {
      n = 0;
      if (*p == ',')
        p++;
      while (*p && *p != ',')
        {
          target[n++] = *p++;
        }
      target[n + 1] = 0;
    }

  return target;
}

/*
 * This routine parses a complete IRC message and fills in
 * all the information into the request structure.
 */
int
irc_parse_request (char *request, int len)
{
  char *p;
  int n, paras;
  int size = 0;

  memset (&irc_request, 0, sizeof (irc_request_t));

  p = request;

  /* parse message origin if necessary */
  if (*p == ':')
    {
      n = 0;
      /* get server or nick */
      while (*p != '!' && *p != '@' && *p != ' ' && size < len)
        {
          irc_request.server[n] = *p;
          irc_request.nick[n++] = *p++;
          size++;
        }
      /* user follows */
      if (*p == '!')
        {
          n = 0;
          p++;
          size++;
          while (*p != '@' && *p != ' ' && size < len)
            {
              irc_request.user[n++] = *p++;
              size++;
            }
        }
      /* host follows */
      if (*p == '@')
        {
          n = 0;
          p++;
          size++;
          while (*p != ' ' && size < len)
            {
              irc_request.host[n++] = *p++;
              size++;
            }
        }
      /* skip whitespace(s) */
      while (*p == ' ' && size < len)
        {
          size++;
          p++;
        }
    }

  /* no message origin, command follow */
  n = 0;
  while (*p != ' ' && size < len)
    {
      irc_request.request[n++] = *p++;
      size++;
    }
  /* get parameter(s) */
  paras = 0;
  while (size < len)
    {
      /* skip whitespace(s) */
      while (*p == ' ' && size < len)
        {
          size++;
          p++;
        }
      if (size == len)
        break;

      /* get next parameter */
      n = 0;

      /* trailing parameter?  */
      if (*p == ':')
        {
          p++;
          size++;
          while (size < len)
            {
              irc_request.para[paras][n++] = *p++;
              size++;
            }
        }

      /* normal parameter */
      else
        {
          while (*p != ' ' && size < len)
            {
              irc_request.para[paras][n++] = *p++;
              size++;
            }
        }
      paras++;
    }

  if (paras > 0 && irc_request.para[paras - 1][0] == '\0')
    paras--;
  irc_request.paras = paras;
  irc_parse_target (&irc_request, 0);

  return 0;
}

/*
 * This function parses one of the given requests paras
 * and stores all targets in its targets.
 */
void
irc_parse_target (irc_request_t *request, int para)
{
  int i, size, n, len;
  char *p;

  request->targets = 0;

  /* is there a para?  */
  if (request->paras <= para)
    return;

  /* yes, start parsing */
  i = 0;
  size = 0;
  p = request->para[para];
  len = strlen (request->para[para]);

  while (size < len)
    {
      /* local channel */
      if (*p == '&')
        {
          n = 0;
          while (*p != ',' && size < len)
            {
              request->target[i].channel[n++] = *p++;
              size++;
            }
        }
      /* mask */
      else if (*p == '$')
        {
          n = 0;
          while (*p != ',' && size < len)
            {
              request->target[i].mask[n++] = *p++;
              size++;
            }
        }
      /* channel or mask */
      else if (*p == '#')
        {
          n = 0;
          while (*p != ',' && size < len)
            {
              request->target[i].mask[n] = *p;
              request->target[i].channel[n++] = *p++;
              size++;
            }
        }
      /* nick or user@host */
      else
        {
          n = 0;
          while (*p != ',' && *p != '@' && size < len)
            {
              request->target[i].user[n] = *p;
              request->target[i].nick[n++] = *p++;
              size++;
            }
          /* host */
          if (*p == '@')
            {
              p++;
              size++;
              n = 0;
              memset (request->target[i].nick, 0, MAX_NICK_LEN);
              while (*p != ',' && size < len)
                {
                  request->target[i].host[n++] = *p++;
                  size++;
                }
            }
        }
      if (*p == ',')
        {
          size++;
          p++;
        }
      i++;
    }
  request->targets = i;
}

/*
 * This routine just tries to match two strings.  It returns non zero
 * if it does.  Because IRC is case insensitive we use the lower case
 * character set for comparisons.
 */
int
irc_string_regex (const char *text, const char *regex)
{
  const char *p;

  /* parse until end of both strings */
  while (*regex && *text)
    {
      /* find end of strings or '?' or '*' */
      while (*regex != '*' && *regex != '?' && *regex && *text)
        {
          /* return no match if so */
          if (irc_lcset[(unsigned) *text] != irc_lcset[(unsigned) *regex])
            return 0;
          text++;
          regex++;
        }
      /* single free character */
      if (*regex == '?')
        {
          if (!(*text))
            return 0;
          text++;
          regex++;
        }
      /* free characters */
      else if (*regex == '*')
        {
          regex++;
          /* skip useless '?'s after '*'s */
          while (*regex == '?')
            regex++;
          /* skip all characters until next character in pattern found */
          while (*text &&
                 irc_lcset[(unsigned) *regex] != irc_lcset[(unsigned) *text])
            text++;
          /* next character in pattern found */
          if (*text)
            {
              /* find the last occurrence of this character in the text */
              p = text + strlen (text);
              while (irc_lcset[(unsigned) *p] != irc_lcset[(unsigned) *text])
                p--;
              /* continue parsing at this character */
              text = p;
            }
        }
    }

  /* is the text longer than the regex?  */
  if (!*text && !*regex)
    return -1;
  return 0;
}

/*
 * Create the lowercase character set for string comparisons.
 */
void
irc_create_lcset (void)
{
  int n;

  for (n = 0; n < 256; n++)
    {
      irc_lcset[n] = (char) tolower (n);
    }
  irc_lcset['['] = '{';
  irc_lcset[']'] = '}';
  irc_lcset['|'] = '\\';
}

/*
 * Make a case insensitive string compare.  Return zero if both
 * strings are equal.
 */
int
irc_string_equal (const char *str1, const char *str2)
{
  const char *p1, *p2;

  if (str1 == str2)
    return 0;

  p1 = str1;
  p2 = str2;

  while (*p1 && *p2)
    {
      if (irc_lcset[(unsigned) *p1] != irc_lcset[(unsigned) *p2])
        return -1;
      p1++;
      p2++;
    }

  if (!*p1 && !*p2)
    return 0;
  return -1;
}
