/*
 * nut-hostlist.c - gnutella host list implementation
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
#include <errno.h>
#include <time.h>

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
#endif

#include "networking-headers.h"
#include "libserveez.h"
#include "gnutella.h"
#include "nut-core.h"
#include "nut-hostlist.h"
#include "unused.h"

/*
 * This routine is the write_socket callback when delivering the
 * host catcher list.  It just waits until the whole HTML has been
 * successfully sent and closes the connection afterwards.
 */
int
nut_hosts_write (svz_socket_t *sock)
{
  int num_written;

  /* write as much data as possible */
  num_written = send (sock->sock_desc, sock->send_buffer,
                      sock->send_buffer_fill, 0);

  /* some data has been written */
  if (num_written > 0)
    {
      sock->last_send = time (NULL);

      /* reduce send buffer */
      if (sock->send_buffer_fill > num_written)
        {
          memmove (sock->send_buffer,
                   sock->send_buffer + num_written,
                   sock->send_buffer_fill - num_written);
        }
      sock->send_buffer_fill -= num_written;
    }
  /* seems like an error */
  else if (num_written < 0)
    {
      svz_log_net_error ("nut: send");
      if (svz_wait_if_unavailable (sock, 1))
        num_written = 0;
    }

  /* has all data been sent successfully?  */
  if (sock->send_buffer_fill <= 0 && !(sock->userflags & NUT_FLAG_HOSTS))
    {
      num_written = -1;
    }

  return (num_written < 0) ? -1 : 0;
}

struct hosts_check_closure
{
  int problemp;
  time_t now;
  svz_socket_t *sock;
};

static void
hosts_check_internal (UNUSED void *k, void *v, void *closure)
{
  nut_host_t *host = v;
  struct hosts_check_closure *x = closure;
  svz_socket_t *sock;

  if (x->problemp)
    return;

  sock = x->sock;
  if (sock->send_buffer_fill > (NUT_SEND_BUFSIZE - 256))
    {
      /* send buffer queue overrun ...  */
      if (svz_sock_printf (sock, ".\n.\n.\n") == -1)
        x->problemp = 1;
    }
  else
    {
      /* usual gnutella host output */
      int day, hour, min, sec;
      time_t t;

      t = x->now - host->last_reply;
      day = t / (3600 * 24);
      t %= (3600 * 24);
      hour = t / 3600;
      t %= 3600;
      min = t / 60;
      t %= 60;
      sec = t;
      if (svz_sock_printf (sock, "%-22s %d days %d:%02d:%02d\n",
                           nut_client_key (host->ip, host->port),
                           day, hour, min, sec) == -1)
        x->problemp = 1;
    }
}

/*
 * This is the check_request callback for the HTML host list output.
 */
int
nut_hosts_check (svz_socket_t *sock)
{
  nut_config_t *cfg = sock->cfg;
  struct hosts_check_closure x;
  char buf[64];

  /* do not enter this routine if you do not want to send something */
  if (!(sock->userflags & NUT_FLAG_HOSTS))
    return 0;

  /* send normal HTTP header */
  if (svz_sock_printf (sock, NUT_HTTP_HEADER) == -1)
    return -1;

  /* send HTML header */
  if (svz_sock_printf (sock, NUT_HTML_HEADER, svz_hash_size (cfg->net)) == -1)
    return -1;

  /* go through all caught gnutella hosts and print their info */
  x.problemp = 0;
  x.sock = sock;
  x.now = time (NULL);
  svz_hash_foreach (hosts_check_internal, cfg->net, &x);
  if (x.problemp)
    return -1;

  /* send HTML footer */
  if (svz_sock_printf (sock, NUT_HTML_FOOTER,
                       PACKAGE_NAME, PACKAGE_VERSION,
                       SVZ_PP_ADDR (buf, sock->local_addr),
                       ntohs (sock->local_port))
      == -1)
    return -1;

  /* state that we have sent all available data */
  sock->userflags &= ~NUT_FLAG_HOSTS;

  /* shutdown the socket if all data has been written */
  if (sock->send_buffer_fill <= 0)
    return -1;

  return 0;
}

/*
 * Within this routine we collect all available gnutella hosts.  Thus
 * we might never ever lack of gnutella net connections.  IP and PORT
 * must be both in network byte order.
 */
int
nut_host_catcher (svz_socket_t *sock, in_addr_t ip, in_port_t port)
{
  nut_host_t *client;
  nut_config_t *cfg = sock->cfg;

  client = (nut_host_t *) svz_hash_get (cfg->net, nut_client_key (ip, port));

  /* not yet in host catcher hash */
  if (client == NULL)
    {
      svz_address_t *addr = svz_address_make (AF_INET, &ip);
      int samep = svz_address_same (addr, sock->local_addr);

      svz_free (addr);
      /* check if it is a valid ip/host combination */
      if ((ip & 0xff000000) == 0 || (ip & 0x00ff0000) == 0 ||
          (ip & 0x0000ff00) == 0 || (ip & 0x000000ff) == 0 ||
          (ip & 0xff000000) == 0xff000000 || (ip & 0x00ff0000) == 0x00ff0000 ||
          (ip & 0x0000ff00) == 0x0000ff00 || (ip & 0x000000ff) == 0x000000ff ||
          samep ||
          port == 0)
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "nut: invalid host: %s:%u\n",
                   svz_inet_ntoa (ip), ntohs (port));
#endif
          return -1;
        }

      client = svz_malloc (sizeof (nut_host_t));
      client->last_reply = time (NULL);
      client->ip = ip;
      client->port = port;
      memset (client->id, 0, NUT_GUID_SIZE);
      svz_hash_put (cfg->net, nut_client_key (ip, port), client);
    }

  /* just update last seen time stamp */
  else
    {
      client->last_reply = time (NULL);
    }
  return 0;
}
