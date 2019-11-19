/*
 * nut-core.c - gnutella core implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2002 Stefan Jahn <stefan@lkcc.org>
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
#include <ctype.h>

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
#endif

#include "networking-headers.h"
#include "libserveez.h"
#include "gnutella.h"
#include "nut-core.h"

/*
 * Gnutella client structure creator.
 */
nut_client_t *
nut_create_client (void)
{
  return svz_calloc (sizeof (nut_client_t));
}

/*
 * Parses a `host:port' combination from the given character string
 * @var{addr} and stores the @var{port} in network byte order.  If the `:port'
 * part is not given, a default port is delivered.  Returns NULL if the
 * string is invalid and otherwise the hostname.  The caller is responsible
 * for freeing the returned string.
 */
char *
nut_parse_host (char *addr, in_port_t *port)
{
  char *p, *host;

  /* create a local copy of the given address string */
  p = host = svz_strdup (addr);
  if (!host)
    {
      /* address string was NULL or empty */
      return NULL;
    }

  /* find separating colon */
  while (*p != ':' && *p)
    p++;
  if (*p)
    {
      *p = '\0';
      p++;
    }
  else
    p = NULL;

  /* convert and store both of the parsed values */
  *port = htons (p
                 ? svz_atoi (p)
                 : NUT_PORT);
  return host;
}

/*
 * This routine parses a `a.b.c.d:port' combination from the given
 * character string @var{addr} and stores both of the values in @var{ip}
 * and @var{port} in network byte order.  If `:port' is not given, a default
 * port is delivered.  Returns -1 on errors and otherwise zero.
 */
int
nut_parse_addr (char *addr, in_addr_t *ip, in_port_t *port)
{
  char *p, *colon, *host;

  /* create a local copy of the given address string */
  p = host = svz_strdup (addr);
  if (!host)
    {
      /* address string was NULL or empty */
      return -1;
    }

  /* validate IP characters */
  while (((*p >= '0' && *p <= '9') || *p == '.') && *p)
    p++;
  if ((*p < '0' || *p > '9') && *p != '.' && *p && *p != ':')
    {
      svz_free (host);
      return -1;
    }

  /* find separating colon */
  colon = p;
  while (*colon != ':' && *colon)
    colon++;
  if (*colon)
    {
      *colon = '\0';
      colon++;
    }
  else
    colon = NULL;

  /* convert and store both of the parsed values */
  *ip = inet_addr (host);
  *port = htons (colon
                 ? svz_atoi (colon)
                 : NUT_PORT);
  svz_free (host);
  return 0;
}

/*
 * This function creates a hash key for a given IP and PORT information
 * for the host catcher hash.  Both values must be given in network byte
 * order.
 */
char *
nut_client_key (in_addr_t ip, in_port_t port)
{
  static char key[32];

  sprintf (key, "%s:%u", svz_inet_ntoa (ip), ntohs (port));
  return key;
}

/* These definitions are for the GUID creating functions in Win32.  */
#ifdef __MINGW32__
CreateGuidProc CreateGuid = NULL;
HMODULE oleHandle = NULL;
#endif /* __MINGW32__ */

/*
 * This routine randomly calculates a Globally Unique Identifier (GUID)
 * and stores it in the given argument.
 */
void
nut_calc_guid (uint8_t *guid)
{
  int n;

#ifdef __MINGW32__
  if (CreateGuid != NULL)
    {
      CreateGuid (guid);
      return;
    }
  else
#endif /* __MINGW32__ */

  for (n = 0; n < NUT_GUID_SIZE; n++)
    {
      /* guid[n] = 256 * rand () / RAND_MAX; */
      guid[n] = (uint8_t) ((rand () >> 1) & 0xff);
    }
}

/*
 * The following routine delivers a text representation of the given
 * GUID.  The format is taken from he M$ headers.
 */
char *
nut_print_guid (uint8_t *guid)
{
  static char id[NUT_GUID_SIZE * 2 + 4];

  sprintf (id,
           "%02X%02X%02X%02X-"
           "%02X%02X-"
           "%02X%02X-"
           "%02X%02X%02X%02X%02X%02X%02X%02X",
           guid[0], guid[1], guid[2], guid[3],
           guid[4], guid[5],
           guid[6], guid[7],
           guid[8], guid[9], guid[10], guid[11],
           guid[12], guid[13], guid[14], guid[15]);

  return id;
}

char *
nut_text_guid (uint8_t *guid)
{
  static char id[NUT_GUID_SIZE * 2 + 1];
  char *w;
  int n;

  for (n = 0, w = id; n < NUT_GUID_SIZE; n++, w += 2)
    sprintf (w, "%02X", guid[n]);
  return id;
}

/*
 * Convert gnutella header to binary data and back.
 */
nut_header_t *
nut_get_header (uint8_t *data)
{
  static nut_header_t hdr;
  unsigned int uint32;

  memcpy (hdr.id, data, NUT_GUID_SIZE);
  data += NUT_GUID_SIZE;
  hdr.function = *data++;
  hdr.ttl = *data++;
  hdr.hop = *data++;
  memcpy (&uint32, data, SIZEOF_UINT32);
  hdr.length = ltohl (uint32);
  return (&hdr);
}

uint8_t *
nut_put_header (nut_header_t *hdr)
{
  static uint8_t buffer[SIZEOF_NUT_HEADER];
  uint8_t *data = buffer;
  unsigned int uint32;

  memcpy (data, hdr->id, NUT_GUID_SIZE);
  data += NUT_GUID_SIZE;
  *data++ = hdr->function;
  *data++ = hdr->ttl;
  *data++ = hdr->hop;
  uint32 = htoll (hdr->length);
  memcpy (data, &uint32, SIZEOF_UINT32);
  return buffer;
}

/*
 * Convert gnutella ping response to binary data and back.
 */
nut_pong_t *
nut_get_pong (uint8_t *data)
{
  static nut_pong_t reply;
  uint16_t uint16;
  unsigned int uint32;

  memcpy (&uint16, data, SIZEOF_UINT16);
  reply.port = ltons (uint16);
  data += SIZEOF_UINT16;
  memcpy (&reply.ip, data, SIZEOF_UINT32);
  data += SIZEOF_UINT32;
  memcpy (&uint32, data, SIZEOF_UINT32);
  reply.files = ltohl (uint32);
  data += SIZEOF_UINT32;
  memcpy (&uint32, data, SIZEOF_UINT32);
  reply.size = ltohl (uint32);
  return (&reply);
}

uint8_t *
nut_put_pong (nut_pong_t *reply)
{
  static uint8_t buffer[SIZEOF_NUT_PONG];
  uint8_t *data = buffer;
  uint16_t uint16;
  unsigned int uint32;

  uint16 = ntols (reply->port);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  memcpy (data, &reply->ip, SIZEOF_UINT32);
  data += SIZEOF_UINT32;
  uint32 = htoll (reply->files);
  memcpy (data, &uint32, SIZEOF_UINT32);
  data += SIZEOF_UINT32;
  uint32 = htoll (reply->size);
  memcpy (data, &uint32, SIZEOF_UINT32);
  return buffer;
}

/*
 * Convert gnutella search query to binary data and back.
 */
nut_query_t *
nut_get_query (uint8_t *data)
{
  static nut_query_t query;
  uint16_t uint16;

  memcpy (&uint16, data, SIZEOF_UINT16);
  query.speed = ltohs (uint16);
  return (&query);
}

uint8_t *
nut_put_query (nut_query_t *query)
{
  static uint8_t buffer[SIZEOF_NUT_QUERY];
  uint8_t *data = buffer;
  uint16_t uint16;

  uint16 = htols (query->speed);
  memcpy (data, &uint16, SIZEOF_UINT16);
  return buffer;
}

/*
 * Convert gnutella file records to binary data and back.
 */
nut_record_t *
nut_get_record (uint8_t *data)
{
  static nut_record_t record;
  unsigned int uint32;

  memcpy (&uint32, data, SIZEOF_UINT32);
  record.index = ltohl (uint32);
  data += SIZEOF_UINT32;
  memcpy (&uint32, data, SIZEOF_UINT32);
  record.size = ltohl (uint32);
  return (&record);
}

uint8_t *
nut_put_record (nut_record_t *record)
{
  static uint8_t buffer[SIZEOF_NUT_RECORD];
  uint8_t *data = buffer;
  unsigned int uint32;

  uint32 = htoll (record->index);
  memcpy (data, &uint32, SIZEOF_UINT32);
  data += SIZEOF_UINT32;
  uint32 = htoll (record->size);
  memcpy (data, &uint32, SIZEOF_UINT32);
  return buffer;
}

/*
 * Convert gnutella query hits to binary data and back.
 */
nut_reply_t *
nut_get_reply (uint8_t *data)
{
  static nut_reply_t reply;
  uint16_t uint16;

  reply.records = *data++;
  memcpy (&uint16, data, SIZEOF_UINT16);
  reply.port = ltons (uint16);
  data += SIZEOF_UINT16;
  memcpy (&reply.ip, data, SIZEOF_UINT32);
  data += SIZEOF_UINT32;
  memcpy (&uint16, data, SIZEOF_UINT16);
  reply.speed = ltohs (uint16);
  return (&reply);
}

uint8_t *
nut_put_reply (nut_reply_t *reply)
{
  static uint8_t buffer[SIZEOF_NUT_REPLY];
  uint8_t *data = buffer;
  uint16_t uint16;

  *data++ = reply->records;
  uint16 = ntols (reply->port);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  memcpy (data, &reply->ip, SIZEOF_UINT32);
  data += SIZEOF_UINT32;
  uint16 = htols (reply->speed);
  memcpy (data, &uint16, SIZEOF_UINT16);
  return buffer;
}

/*
 * Convert gnutella push request to binary data and back.
 */
nut_push_t *
nut_get_push (uint8_t *data)
{
  static nut_push_t push;
  unsigned int uint32;
  uint16_t uint16;

  memcpy (push.id, data, NUT_GUID_SIZE);
  data += NUT_GUID_SIZE;
  memcpy (&uint32, data, SIZEOF_UINT32);
  push.index = ltohl (uint32);
  data += SIZEOF_UINT32;
  memcpy (&push.ip, data, SIZEOF_UINT32);
  data += SIZEOF_UINT32;
  memcpy (&uint16, data, SIZEOF_UINT16);
  push.port = ltons (uint16);
  return (&push);
}

uint8_t *
nut_put_push (nut_push_t *push)
{
  static uint8_t buffer[SIZEOF_NUT_PUSH];
  uint8_t *data = buffer;
  unsigned int uint32;
  uint16_t uint16;

  memcpy (data, push->id, NUT_GUID_SIZE);
  data += NUT_GUID_SIZE;
  uint32 = htoll (push->index);
  memcpy (data, &uint32, SIZEOF_UINT32);
  data += SIZEOF_UINT32;
  memcpy (data, &push->ip, SIZEOF_UINT32);
  data += SIZEOF_UINT32;
  uint16 = ntols (push->port);
  memcpy (data, &uint16, SIZEOF_UINT16);
  return buffer;
}

/*
 * Canonizes a given filename and converts it to something printable.
 */
void
nut_canonize_file (char *file)
{
  while (*file)
    {
      if (!isalnum ((uint8_t) *file) && !isprint ((uint8_t) *file))
        *file = '_';
      file++;
    }
}

/*
 * This routine parses a given gnutella (HTTP) header for certain
 * properties and delivers either a property value which must be
 * ‘svz_free’d afterwards or NULL.
 */
char *
nut_parse_property (char *header, int len, char *property)
{
  char *h = header, *p = property, *value, *end = header + len;

  while (h < end)
    {
      /* find beginning of property */
      while (h < end && tolower (*h) != tolower (*p))
        h++;
      if (h >= end)
        return NULL;

      /* compare whole property name */
      header = h;
      while (h < end && *p && tolower (*h++) == tolower (*p++));
      if (h >= end)
        return NULL;
      if (*p)
        {
          /* fallback to property's first character */
          h = header + 1;
          p = property;
          continue;
        }

      /* parse property value */
      while (h < end && *h++ == ' ');
      if (h >= end || *(h - 1) != ':')
        return NULL;
      while (h < end && *h == ' ')
        h++;
      header = h;
      while (h < end && *h != '\r' && *h != '\n')
        h++;
      if (h >= end || h == header)
        return NULL;

      /* copy property value */
      len = h - header;
      value = svz_malloc (len + 1);
      memcpy (value, header, len);
      value[len] = '\0';
      return value;
    }
  return NULL;
}
