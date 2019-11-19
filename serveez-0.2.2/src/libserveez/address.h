/*
 * address.h - abstract address for IPv4, IPv6
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
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

#ifndef __ADDRESS_H__
#define __ADDRESS_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
/* end svzint */

typedef struct svz_address svz_address_t;

__BEGIN_DECLS

SERVEEZ_API svz_address_t *svz_address_make (int family, const void *bits);
SERVEEZ_API int svz_address_family (const svz_address_t *addr);
SERVEEZ_API int svz_address_to (void *dest, const svz_address_t *addr);
SERVEEZ_API int svz_address_same (const svz_address_t *a,
                                  const svz_address_t *b);
SERVEEZ_API const char *svz_pp_address (char *buf, size_t size,
                                        const svz_address_t *addr);
SERVEEZ_API const char *svz_pp_addr_port (char *buf, size_t size,
                                          const svz_address_t *addr,
                                          in_port_t port);
SERVEEZ_API svz_address_t *svz_address_copy (const svz_address_t *addr);

__END_DECLS

#endif /* not __ADDRESS_H__ */

/* Idioms.  */

/**
 * Expand to a series of commands.  First, if @var{place} is
 * non-@code{NULL}, then @code{svz_free} it.  Next, assign to
 * @var{place} a new address object made by calling
 * @code{svz_address_make} with @var{family} and @var{bits}.
 */
#define SVZ_SET_ADDR(place,family,bits)  do             \
    {                                                   \
      if (place)                                        \
        svz_free (place);                               \
      place = svz_address_make (family, bits);          \
    }                                                   \
  while (0)

/**
 * Expand to a call to @code{svz_pp_address}, passing it
 * @var{buf} and @code{sizeof @var{buf}}, in addition to @var{addr}.
 */
#define SVZ_PP_ADDR(buf,addr)                   \
  svz_pp_address (buf, sizeof buf, addr)

/**
 * Expand to a call to @code{svz_pp_addr_port}, passing it
 * @var{buf} and @code{sizeof @var{buf}}, in addition to
 * @var{addr} and @var{port}.
 */
#define SVZ_PP_ADDR_PORT(buf,addr,port)                 \
  svz_pp_addr_port (buf, sizeof buf, addr, port)

/* address.h ends here */
