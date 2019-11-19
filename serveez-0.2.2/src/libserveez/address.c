/*
 * address.c - abstract address for IPv4, IPv6
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

#include "config.h"
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include "networking-headers.h"
#include "misc-macros.h"
#include "libserveez/alloc.h"
#include "libserveez/address.h"

/* Why is ‘struct svz_address’ internal?
   <http://lists.gnu.org/archive/html/dev-serveez/2011-05/msg00003.html>

   It is internal to maximize implementation slack as well as API/ABI
   stability.  This is important because the type is meant to be flexible
   (optional families) and extensible (new families).  Making it public
   burdens the client code with churn.  The benefits of transparency, namely,
   no need for _family, _copy and _to, do not outweigh this burden.  */

struct svz_address
{
  sa_family_t family;
  union
  {
    in_addr_t in4;
#if IPV6_OK
    struct in6_addr in6;
#endif
  } u;
};

/**
 * Return an address object to hold an address in @var{family},
 * represented by @var{bits}.  @var{family} must be one of:
 *
 * @table @code
 * @item AF_INET
 * An IPv4 address; @var{bits} is @code{in_addr_t *}.
 *
 * @item AF_INET6
 * (if supported by your system) An IPv6 address;
 * @var{bits} is @code{struct in6_addr *}.
 * @end table
 *
 * The @var{bits} are expected in network byte order.
 * If there are problems, return @code{NULL}.
 */
svz_address_t *
svz_address_make (int family, const void *bits)
{
  svz_address_t *addr;

  if (! bits)
    return NULL;

  addr = svz_malloc (sizeof (svz_address_t));
  switch (family)
    {
    case AF_INET:
      memcpy (&addr->u.in4, bits, sizeof (in_addr_t));
      break;
#if IPV6_OK
    case AF_INET6:
      memcpy (&addr->u.in6, bits, sizeof (struct in6_addr *));
      break;
#endif
    default:
      family = AF_UNSPEC;
    }
  addr->family = family;
  return addr;
}

/**
 * Return the address family of @var{addr}.
 */
int
svz_address_family (const svz_address_t *addr)
{
  return addr->family;
}

/**
 * Copy the address bits out of @var{addr} to @var{dest}.
 * Return 0 on success, -1 if either @var{addr} or @var{dest}
 * is @code{NULL}, or the @var{addr} family is @code{AF_UNSPEC}.
 */
int
svz_address_to (void *dest, const svz_address_t *addr)
{
  if (!addr || !dest || AF_UNSPEC == addr->family)
    return -1;

  switch (addr->family)
    {
    case AF_INET:
      memcpy (dest, &addr->u.in4, sizeof (in_addr_t));
      break;
#if IPV6_OK
    case AF_INET6:
      memcpy (dest, &addr->u.in6, sizeof (struct in6_addr));
      break;
#endif
    default:
      /* Should not get here!  */
      return -1;
    }
  return 0;
}

/**
 * Return 1 if @var{a} and @var{b} represent the same address
 * (identical family and bits), otherwise 0.
 */
int
svz_address_same (const svz_address_t *a, const svz_address_t *b)
{
  return a->family == b->family
    && !memcmp (&a->u, &b->u,
                (AF_INET == a->family
                 ? sizeof (in_addr_t)
                 :
#if IPV6_OK
                 sizeof (struct in6_addr)
#else
                 0));
#endif
}

/**
 * Format an external representation of @var{addr} into @var{buf},
 * of @var{size} bytes.  The format depends on the family of
 * @var{addr}.  For IPv4, this is numbers-and-dots.  For IPv6, it
 * is ``the most appropriate IPv6 network address format for
 * @var{addr}'', according to the manpage of @code{inet_ntop}, the
 * function that actually does the work.
 *
 * If @var{buf} or @var{addr} is @code{NULL}, or @var{size} is not
 * big enough, return @code{NULL}.  Otherwise, return @var{buf}.
 */
const char *
svz_pp_address (char *buf, size_t size, const svz_address_t *addr)
{
  if (!buf || !addr || !size)
    return NULL;

  return inet_ntop (addr->family, &addr->u, buf, size);
}

/**
 * Format an external representation of @var{addr} and @var{port}
 * (in network byte order) into @var{buf}, of @var{size} bytes.
 * The address @dfn{xrep} (external representation) is done by
 * @code{svz_pp_address}, q.v.  The rest of the formatting depends
 * on the @var{addr} family.
 *
 * @multitable @columnfractions 0.4 0.6
 * @headitem Family
 * @tab Formatting
 * @item @code{AF_INET} (IPv4)
 * @tab @code{@var{xrep}:@var{port}}
 * @item @code{AF_INET6} (IPv6)
 * @tab @code{[@var{xrep}]:@var{port}}
 * @end multitable
 *
 * If @var{buf} or @var{addr} is @code{NULL}, or @var{size} is not
 * big enough, return @code{NULL}.  Otherwise, return @var{buf}.
 */
const char *
svz_pp_addr_port (char *buf, size_t size,
                  const svz_address_t *addr,
                  in_port_t port)
{
  char abuf[64];                        /* FIXME */
  char pbuf[10];

  if (!buf || !addr || !size)
    return NULL;

  snprintf (pbuf, sizeof pbuf, ":%d", ntohs (port));
  switch (addr->family)
    {
    case AF_INET:
      snprintf (buf, size, "%s%s", SVZ_PP_ADDR (abuf, addr), pbuf);
      break;
#if IPV6_OK
    case AF_INET6:
      snprintf (buf, size, "[%s]%s", SVZ_PP_ADDR (abuf, addr), pbuf);
      break;
#endif
    }
  return buf;
}

/**
 * Return a copy of @var{addr}.
 */
svz_address_t *
svz_address_copy (const svz_address_t *addr)
{
  return svz_address_make (addr->family, &addr->u);
}

/* address.c ends here */
