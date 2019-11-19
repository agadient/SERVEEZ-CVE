/*
 * ident-proto.h - fake ident server
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __IDENT_PROTO_H__
#define __IDENT_PROTO_H__ 1

/*
 * Configuration of this server
 */
struct fakeident_config
{
  char *systemtype;      /* the system type in the response, 'UNIX' */
  char *username;        /* the username responded for all requests */
};

/*
 * This server's definition
 */
extern svz_servertype_t fakeident_server_definition;

#endif /* not __IDENT_PROTO_H__ */
