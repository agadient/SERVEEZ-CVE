/*
 * interfaces.h - network interface definitions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __INTERFACE_H__
#define __INTERFACE_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/address.h"
/* end svzint */

/*
 * Structure for collecting IP interfaces.
 */
typedef struct svz_interface
{
  size_t index;         /* interface index */
  char *description;    /* interface description */
  svz_address_t *addr;  /* address */
  int detected;         /* interface flag */
}
svz_interface_t;

typedef int (svz_interface_do_t) (const svz_interface_t *, void *);

__BEGIN_DECLS

SERVEEZ_API int svz_foreach_interface (svz_interface_do_t *, void *);
SERVEEZ_API int svz_interface_add (size_t, char *, int, const void *, int);
SBO svz_interface_t *svz_interface_search (char *);
SBO void svz_interface_check (void);

__END_DECLS

#endif /* not __INTERFACE_H__ */
