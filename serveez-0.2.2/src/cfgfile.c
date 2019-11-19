/*
 * cfgfile.c - configuration file and left overs
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include <stdio.h>
#include <string.h>
#include "networking-headers.h"
#include "libserveez.h"
#include "cfgfile.h"

/*
 * Include headers of servers.
 */
#include "foo-server/foo-proto.h"
#if ENABLE_HTTP_PROTO
# include "http-server/http-proto.h"
#endif
#if ENABLE_IRC_PROTO
# include "irc-server/irc-proto.h"
#endif
#if ENABLE_CONTROL_PROTO
# include "ctrl-server/control-proto.h"
#endif
#if ENABLE_SNTP_PROTO
# include "sntp-server/sntp-proto.h"
#endif
#if ENABLE_GNUTELLA
# include "nut-server/gnutella.h"
#endif
#if ENABLE_TUNNEL
# include "tunnel-server/tunnel.h"
#endif
#if ENABLE_FAKEIDENT
# include "fakeident-server/ident-proto.h"
#endif
#if ENABLE_PROG_SERVER
# include "prog-server/prog-server.h"
#endif

/*
 * Aggregate them for easy iteration.
 */
static svz_servertype_t *builtin[] = {
#if ENABLE_HTTP_PROTO
  &http_server_definition,
#endif
#if ENABLE_IRC_PROTO
  &irc_server_definition,
#endif
#if ENABLE_CONTROL_PROTO
  &ctrl_server_definition,
#endif
#if ENABLE_SNTP_PROTO
  &sntp_server_definition,
#endif
#if ENABLE_GNUTELLA
  &nut_server_definition,
#endif
#if ENABLE_TUNNEL
  &tnl_server_definition,
#endif
#if ENABLE_FAKEIDENT
  &fakeident_server_definition,
#endif
#if ENABLE_PROG_SERVER
  &prog_server_definition,
#endif
  &foo_server_definition
};
#define builtin_count  (sizeof (builtin) / sizeof (builtin[0]))

/*
 * Initialize all static server definitions.
 */
void
init_server_definitions (void)
{
  size_t i;

  for (i = 0; i < builtin_count; i++)
    svz_servertype_add (builtin[i]);
}

/*
 * Display available servers to stdout.
 */
void
print_available_servers (void)
{
  const char format[] = "%-20s%s\n";
  size_t i;

  for (i = 0; i < builtin_count; i++)
    printf (format, builtin[i]->prefix, builtin[i]->description);
#if ENABLE_GUILE_SERVER
  printf (format, "(dynamic)", "(servers defined in scheme)");
#endif
}
