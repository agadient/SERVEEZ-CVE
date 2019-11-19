/*
 * raw-socket.c - raw ip socket implementations
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

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
#endif

#include "networking-headers.h"
#include "libserveez/util.h"
#include "libserveez/core.h"
#include "libserveez/raw-socket.h"

/* local definitions */
#define IP_VERSION_4     4
#define IP_CHECKSUM_OFS  10

/* ip protocol definitions */
#define ICMP_PROTOCOL  1
#define TCP_PROTOCOL   6
#define UDP_PROTOCOL  17

/* version and length are 4 bit values in the ip header */
#define IP_HDR_VERSION(hdr) ((hdr->version_length >> 4) & 0x0f)
#define IP_HDR_LENGTH(hdr)  ((hdr->version_length & 0x0f) << 2)

/* ip header flags (part of frag_offset) */
#define IP_HDR_FLAGS(hdr) ((hdr->frag_offset) & 0xE000)
#define IP_FLAG_DF 0x4000 /* Don't Fragment This Datagram (DF).  */
#define IP_FLAG_MF 0x2000 /* More Fragments Flag (MF).  */
#define IP_HDR_FRAG(hdr) ((hdr->frag_offset) & 0x1FFF)

/* IP header structure.  */
typedef struct
{
  uint8_t version_length;     /* header length (in DWORDs) and ip version */
  uint8_t tos;                /* type of service = 0 */
  uint16_t length;            /* total ip packet length */
  uint16_t ident;             /* ip identifier */
  uint16_t frag_offset;       /* fragment offset (in 8 bytes) and flags */
  uint8_t ttl;                /* time to live */
  uint8_t protocol;           /* ip protocol */
  uint16_t checksum;          /* ip header checksum */
  in_addr_t src;              /* source address */
  in_addr_t dst;              /* destination address */
}
svz_ip_header_t;

/*
 * The raw socket support on Windoze machines is quite doubtful and
 * almostly unusable because:
 * 1. sent packets are not received on the same socket
 * 2. not all raw sockets get all ip packets in all processes
 * 3. not sure about Winsock 1/2 versions to use (ws2_32.dll or wsock32.dll)
 */

/*
 * Get IP header from plain data.
 */
static svz_ip_header_t *
unpack_header (uint8_t *data)
{
  static svz_ip_header_t hdr;
  uint16_t uint16;
  unsigned int uint32;

  hdr.version_length = *data++;
  hdr.tos = *data++;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.length = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.ident = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.frag_offset = ntohs (uint16);
  data += SIZEOF_UINT16;
  hdr.ttl = *data++;
  hdr.protocol = *data++;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.checksum = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint32, data, SIZEOF_UINT32);
  hdr.src = uint32;
  data += SIZEOF_UINT32;
  memcpy (&uint32, data, SIZEOF_UINT32);
  hdr.dst = uint32;

  return &hdr;
}

/*
 * Recalculate any IP checksum.
 */
uint16_t
svz_raw_ip_checksum (uint8_t *data, int len)
{
  register unsigned checksum = 0;

  /*
   * Calculate the 16 bit one's complement of the one's complement sum
   * of all 16 bit words in the header.  For computing the checksum,
   * the checksum field should be zero.  This checksum may be replaced in
   * the future.
   */
  while (len > 1)
    {
      /* This is the inner loop */
      checksum += *data | (*(data + 1) << 8);
      len -= 2;
      data += 2;
    }

  /* Add left-over byte, if any */
  if (len > 0)
    checksum += *data;

  /* Fold 32-bit checksum to 16 bits */
  while (checksum >> 16)
    checksum = (checksum & 0xffff) + (checksum >> 16);
  checksum = ~checksum;

  return htons ((uint16_t) checksum);
}

/*
 * Checking the IP header only.  Return the length of the header if it
 * is valid, otherwise -1.
 */
int
svz_raw_check_ip_header (uint8_t *data, int len)
{
  svz_ip_header_t *ip_header;

  /* get the IP header and reject the checksum within plain data */
  ip_header = unpack_header (data);
  data[IP_CHECKSUM_OFS] = 0;
  data[IP_CHECKSUM_OFS + 1] = 0;

#if 0
  printf ("ip version      : %d\n"
          "header length   : %d byte\n"
          "type of service : %d\n"
          "total length    : %d byte\n"
          "ident           : 0x%04X\n"
          "flags           : 0x%04X %s %s\n"
          "frag. offset    : %d\n"
          "ttl             : %d\n"
          "protocol        : 0x%02X\n"
          "checksum        : 0x%04X\n"
          "source          : %s\n",
          IP_HDR_VERSION (ip_header), IP_HDR_LENGTH (ip_header),
          ip_header->tos, ip_header->length, ip_header->ident,
          IP_HDR_FLAGS (ip_header),
          IP_HDR_FLAGS (ip_header) & IP_FLAG_DF ? "[Don't Fragment]" : "",
          IP_HDR_FLAGS (ip_header) & IP_FLAG_MF ? "[More Fragments]" : "",
          IP_HDR_FRAG (ip_header), ip_header->ttl, ip_header->protocol,
          ip_header->checksum, svz_inet_ntoa (ip_header->src));
  printf ("destination     : %s\n", svz_inet_ntoa (ip_header->dst));
#endif

  /* Is this IPv4 version?  */
  if (IP_HDR_VERSION (ip_header) != IP_VERSION_4)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "raw: cannot handle IPv%d\n",
               IP_HDR_VERSION (ip_header));
#endif
      return -1;
    }

  /* Check Internet Header Length.  */
  if (IP_HDR_LENGTH (ip_header) > len)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "raw: invalid IHL (%d > %d)\n",
               IP_HDR_LENGTH (ip_header), len);
#endif
      return -1;
    }

  /* Check total length.  */
  if (ip_header->length < len)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "raw: invalid total length (%d < %d)\n",
               ip_header->length, len);
#endif
      return -1;
    }

  /* Check protocol type.  */
  if (ip_header->protocol != ICMP_PROTOCOL)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "raw: invalid protocol 0x%02X\n",
               ip_header->protocol);
#endif
      return -1;
    }

  /* Recalculate and check the header checksum.  */
  if (svz_raw_ip_checksum (data, IP_HDR_LENGTH (ip_header)) !=
      ip_header->checksum)
    {
      /* FIXME: Why are header checksums invalid on big packets?  */
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG,
               "raw: invalid ip header checksum (%04X != %04X)\n",
               svz_raw_ip_checksum (data, IP_HDR_LENGTH (ip_header)),
               ip_header->checksum);
#endif
    }

  return IP_HDR_LENGTH (ip_header);
}
