/*
 * reverse-dns.c - reverse DNS lookup coserver implementation
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
#include "libserveez/coserver/coserver.h"
#include "libserveez/coserver/reverse-dns.h"
#include "libserveez/coserver/xerror.h"

#define MAX_CACHE_ENTRIES 1024 /* nslookup cache entries */

/*
 * Reverse DNS lookup cache structure.
 */
typedef struct
{
  int entries;
  in_addr_t ip[MAX_CACHE_ENTRIES];
  char resolved[COSERVER_BUFSIZE][MAX_CACHE_ENTRIES];
}
reverse_dns_cache_t;

static reverse_dns_cache_t cache;

/*
 * Initialize the cache structure.
 */
void
reverse_dns_init (void)
{
  cache.entries = 0;
}

#define MAX_IP_STRING_LENGTH  15        /* www.xxx.yyy.zzz */

/*
 * Proceed a reverse DNS lookup.
 */
char *
reverse_dns_handle_request (char *inbuf)
{
  char ip[1 + MAX_IP_STRING_LENGTH];
  in_addr_t addr[2];
  struct hostent *host;
  static char resolved[COSERVER_BUFSIZE];
  int n;

  if ((1 == sscanf (inbuf, PERCENT_N_S (MAX_IP_STRING_LENGTH), ip)))
    {
      svz_pton (ip, &addr[0]);
      addr[1] = 0;

      /*
       * look up the ip->host cache first
       */
      for (n = 0; n < cache.entries; n++)
        {
          if (cache.ip[n] == addr[0])
            {
              sprintf (resolved, "%s", cache.resolved[n]);
              return resolved;
            }
        }

      if ((host = gethostbyaddr ((char *) addr, sizeof (addr[0]), AF_INET))
          == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "reverse dns: gethostbyaddr: %s (%s)\n",
                   xerror (), ip);
          return NULL;
        }
      else
        {
          if (n < MAX_CACHE_ENTRIES)
            {
              strcpy (cache.resolved[n], host->h_name);
              cache.ip[n] = addr[0];
              cache.entries++;
            }

#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "reverse dns: %s is %s\n", ip, host->h_name);
#endif /* ENABLE_DEBUG */
          sprintf (resolved, "%s", host->h_name);
          return resolved;
        }
    }
  else
    {
      svz_log (SVZ_LOG_ERROR, "reverse dns: protocol error\n");
      return NULL;
    }
}
