/*
 * irc-core.h - IRC core protocol header
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

#ifndef __IRC_CORE_H__
#define __IRC_CORE_H__

/* some IRC protocol limitation definitions */
#define MAX_NICK_LEN 16     /* maximum nick name length */
#define MAX_NAME_LEN 256    /* maximum name length */
#define MAX_PARAMS   16     /* parameters */
#define MAX_TARGETS  64     /* maximum amount of targets */
#define MAX_MSG_LEN  512    /* length of an IRC message */
#define MAX_CHAN_LEN 200    /* maximum channel name length */

/* IRC server protocol flags */
#define IRC_FLAG_SERVER 0x0001
#define IRC_FLAG_CLIENT 0x0002

/*
 * This structure defines an IRC target.
 */
typedef struct
{
  char channel[MAX_NAME_LEN]; /* channel (initiated by '#' or '&') */
  char user[MAX_NAME_LEN];    /* user name */
  char host[MAX_NAME_LEN];    /* host name (user@host) */
  char nick[MAX_NICK_LEN];    /* nick name */
  char mask[MAX_NAME_LEN];    /* mask (initiated by '#' or '$' ) */
}
irc_target_t;

/*
 * Here is a fully IRC request structure defined.  This is very useful
 * when parsing any request and propagating it afterwards.
 */
typedef struct
{
  /* message origin (optional), initiated by ':' */
  char server[MAX_NAME_LEN];     /* server destination */
  char nick[MAX_NICK_LEN];       /* nick name */
  char user[MAX_NAME_LEN];       /* user name */
  char host[MAX_NAME_LEN];       /* host name (!user@host) */

  /* the irc command (letters or 3 digits) */
  char request[MAX_NAME_LEN];    /* request (numeric or word) */

  /*
   * parameter list, separated by space(s), initiated by ':' is last
   * one also containing space(s)
   */
  char para[MAX_PARAMS][MAX_MSG_LEN];
  int paras;

  /* irc targets (used to be within the first parameters) */
  irc_target_t target[MAX_TARGETS];
  int targets;
}
irc_request_t;

extern irc_request_t irc_request;  /* single IRC request */

/* authentification strings */
#define IRC_DNS_INIT      "*** Starting DNS lookup ..."
#define IRC_DNS_DONE      "*** Successful DNS lookup (cached)."
#define IRC_DNS_NOREPLY   "*** No DNS response."
#define IRC_IDENT_INIT    "*** Checking Ident ..."
#define IRC_IDENT_DONE    "*** Successful Identification."
#define IRC_IDENT_NOREPLY "*** No Ident response."

/* Some useful function for parsing masks.  */
int irc_string_equal (const char *str1, const char *str2);
int irc_string_regex (const char *text, const char *regex);

/*
 * We need this for a lower case character set, because
 * nick names and channel names in IRC are case insensitive.
 */
void irc_create_lcset (void);

/* Parsing routines for an IRC request.  */
int irc_parse_request (char *request, int len);
void irc_parse_target (irc_request_t *request, int para);
char *irc_get_target (char *para, int nr);

/* The standard routine for IRC detection.  */
int irc_detect_proto (svz_server_t *server, svz_socket_t *sock);
int irc_connect_socket (svz_server_t *server, svz_socket_t *sock);
int irc_check_request (svz_socket_t *sock);

#endif /* __IRC_CORE_H__ */
