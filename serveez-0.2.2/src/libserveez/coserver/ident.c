/*
 * ident.c - ident coserver implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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
#include <errno.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#include "networking-headers.h"
#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
# include <netdb.h>
#endif

#include "libserveez/core.h"
#include "libserveez/socket.h"
#include "libserveez/util.h"
#include "libserveez/coserver/coserver.h"
#include "libserveez/coserver/ident.h"

#define IDENT_PORT 113 /* the identd port */

/*
 * The following routine takes the input buffer in the format "%s:%u:%u"
 * which is the remote internet address and the remote and local network
 * port.  Then it is processing an ident request and parses the respond to
 * gain the users name.
 */
char *
ident_handle_request (char *inbuf)
{
  svz_t_socket sock;
  struct sockaddr_in server;
  in_addr_t addr;
  unsigned lport, rport;
  static char ident_response[COSERVER_BUFSIZE];
  char *p_end;
  char user[64];
  char *p, *u;
  int r;
  char *rp;

  /* Parse internet address first.  */
  p = inbuf;
  while (*p && *p != ':')
    p++;
  if (!*p)
    {
      svz_log (SVZ_LOG_ERROR, "ident: invalid request `%s'\n", inbuf);
      return NULL;
    }
  *p = '\0';
  p++;
  svz_pton (inbuf, &addr);

  /* Parse remote and local port afterwards.  */
  if (2 != sscanf (p, "%u:%u", &rport, &lport))
    {
      svz_log (SVZ_LOG_ERROR, "ident: invalid request `%s'\n", inbuf);
      return NULL;
    }

  /* Create a socket for communication with the ident server.  */
  if ((sock = socket (AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
    {
      svz_log_net_error ("ident: socket");
      return NULL;
    }

  /* Connect to the server.  */
  memset (&server, 0, sizeof (server));
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = addr;
  server.sin_port = htons (IDENT_PORT);
  if (connect (sock, (struct sockaddr *) &server, sizeof (server)) == -1)
    {
      svz_log_net_error ("ident: connect");
      svz_closesocket (sock);
      return NULL;
    }

  /* Send the request to the server and receive response.  */
  sprintf (ident_response, "%d , %d\r\n", rport, lport);
  send (sock, ident_response, strlen (ident_response), 0);

  r = 1;
  rp = ident_response;
  while (rp < ident_response + COSERVER_BUFSIZE && r != 0)
    {
      if ((r = recv (sock, rp,
                     COSERVER_BUFSIZE - (rp - ident_response), 0)) < 0)
        {
          svz_log_net_error ("ident: recv");
          svz_closesocket (sock);
          return NULL;
        }
      rp += r;
    }

  /* Now close the socket and notify the response.  */
  if (shutdown (sock, 2) == -1)
    svz_log_net_error ("ident: shutdown");
  if (svz_closesocket (sock) < 0)
    svz_log_net_error ("ident: close");

  svz_log (SVZ_LOG_NOTICE, "ident: %s", ident_response);

  p = ident_response;
  p_end = p + strlen (p);

  /* Parse client port.  */
  if (p >= p_end || !(*p >= '0' && *p <= '9'))
    return NULL;
  while (p < p_end && *p >= '0' && *p <= '9')
    p++;

  /* Skip whitespace and separating comma.  */
  while (p < p_end && *p == ' ')
    p++;
  if (p >= p_end || *p != ',')
    return NULL;
  p++;
  while (p < p_end && *p == ' ')
    p++;

  /* Parse server port.  */
  if (p >= p_end || !(*p >= '0' && *p <= '9'))
    return NULL;
  while (p < p_end && *p >= '0' && *p <= '9')
    p++;

  /* Skip whitespace and separating colon.  */
  while (p < p_end && *p == ' ')
    p++;
  if (p >= p_end || *p != ':')
    return NULL;
  p++;
  while (p < p_end && *p == ' ')
    p++;

  /* Parse response type.  (USERID or ERROR possible) */
  if (memcmp (p, "USERID", 6))
    return NULL;
  while (p < p_end && *p != ' ')
    p++;

  /* Skip whitespace and separating colon.  */
  while (p < p_end && *p == ' ')
    p++;
  if (p >= p_end || *p != ':')
    return NULL;
  p++;
  while (p < p_end && *p == ' ')
    p++;
  if (p >= p_end)
    return NULL;

  /* Parse OS type.  */
  while (p < p_end && *p != ' ')
    p++;

  /* Skip whitespace and separating colon.  */
  while (p < p_end && *p == ' ')
    p++;
  if (p >= p_end || *p != ':')
    return NULL;
  p++;
  while (p < p_end && *p == ' ')
    p++;

  /* Finally parse the user name.  */
  u = user;
  while (p < p_end && *p != '\0' && *p != '\n' && *p != '\r')
    {
      if (u < user + (sizeof (user) - 1))
        *u++ = *p;
      p++;
    }
  *u = '\0';

#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "ident: received identified user `%s'\n", user);
#endif

  sprintf (ident_response, "%s", user);
  return ident_response;
}
