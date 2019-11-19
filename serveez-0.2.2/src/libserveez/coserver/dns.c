/*
 * dns.c - DNS lookup coserver implementation
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
#include <stdlib.h>
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

#include "cpp-tricks.h"

#include "libserveez/util.h"
#include "libserveez/core.h"
#include "libserveez/coserver/coserver.h"
#include "libserveez/coserver/dns.h"
#include "libserveez/coserver/xerror.h"

/*
 * Proceed a single DNS lookup.
 */
char *
dns_handle_request (char *inbuf)
{
  in_addr_t addr;
  struct hostent *host;
  static char resolved[COSERVER_BUFSIZE];

  if ((1 == sscanf (inbuf, PERCENT_N_S (COSERVER_BUFSIZE), resolved)))
    {
      /* find the host by its name */
      if ((host = gethostbyname (resolved)) == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "dns: gethostbyname: %s (%s)\n",
                   xerror (), resolved);
          return NULL;
        }

      /* get the inet address in network byte order */
      if (host->h_addrtype == AF_INET)
        {
          memcpy (&addr, host->h_addr_list[0], host->h_length);

#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "dns: %s is %s\n",
                   host->h_name, svz_inet_ntoa (addr));
#endif /* ENABLE_DEBUG */
          sprintf (resolved, "%s", svz_inet_ntoa (addr));
          return resolved;
        }
    }
  else
    {
      svz_log (SVZ_LOG_ERROR, "dns: protocol error\n");
      return NULL;
    }

  return NULL;
}
