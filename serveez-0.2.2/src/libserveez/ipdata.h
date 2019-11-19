/*
 * ipdata.h - TCP/IP QueryEx definitons.
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __IPDATA_H__
#define __IPDATA_H__ 1

/*
 * IP address entry.
 */
typedef struct IPAddrEntry
{
  in_addr_t      iae_addr;
  uint32_t       iae_index;
  uint32_t       iae_mask;
  in_addr_t      iae_bcastaddr;
  uint32_t       iae_reasmsize;
  uint16_t       iae_context;
  uint16_t       iae_pad;
}
IPAddrEntry;

#define IP_MIB_STATS_ID           1
#define IP_MIB_ADDRTABLE_ENTRY_ID 0x102
#define IP_INTFC_FLAG_P2P         1
#define IP_INTFC_INFO_ID          0x103
#define IF_MIB_STATS_ID           1
#define MAX_PHYSADDR_SIZE         8
#define MAX_IFDESCR_LEN           256

/*
 * Structure of an interface entry.
 */
typedef struct IFEntry
{
  uint32_t if_index;
  uint32_t if_type;
  uint32_t if_mtu;
  uint32_t if_speed;
  uint32_t if_physaddrlen;
  uint8_t  if_physaddr[MAX_PHYSADDR_SIZE];
  uint32_t if_adminstatus;
  uint32_t if_operstatus;
  uint32_t if_lastchange;
  uint32_t if_inoctets;
  uint32_t if_inucastpkts;
  uint32_t if_innucastpkts;
  uint32_t if_indiscards;
  uint32_t if_inerrors;
  uint32_t if_inunknownprotos;
  uint32_t if_outoctets;
  uint32_t if_outucastpkts;
  uint32_t if_outnucastpkts;
  uint32_t if_outdiscards;
  uint32_t if_outerrors;
  uint32_t if_outqlen;
  uint32_t if_descrlen;
  uint8_t  if_descr[1];
}
IFEntry;

/*
 * Structure of an entity ID.
 */
typedef struct TDIEntityID
{
  uint32_t tei_entity;
  uint32_t tei_instance;
}
TDIEntityID;

/*
 * Structure of an object ID.
 */
typedef struct TDIObjectID
{
  TDIEntityID   toi_entity;
  uint32_t      toi_class;
  uint32_t      toi_type;
  uint32_t      toi_id;
}
TDIObjectID;

#define MAX_TDI_ENTITIES          512
#define INFO_CLASS_GENERIC        0x100
#define INFO_CLASS_PROTOCOL       0x200
#define INFO_CLASS_IMPLEMENTATION 0x300
#define INFO_TYPE_PROVIDER        0x100
#define INFO_TYPE_ADDRESS_OBJECT  0x200
#define INFO_TYPE_CONNECTION      0x300
#define ENTITY_LIST_ID            0
#define GENERIC_ENTITY            0
#define CL_NL_ENTITY              0x301
#define IF_ENTITY                 0x200
#define CONTEXT_SIZE              16

/*
 * The following are IDs supported by all entities.  They are of class
 * GENERIC and type PROVIDER.
 * The ID to get the entity type.  The return from this type is an
 * unsigned integer (see below).
 */
#define ENTITY_TYPE_ID 1

/*
 * Valid values to get back from entity type ID query.
 */
#define CL_NL_IP    0x303 /* Entity implements IP.  */
#define IF_GENERIC  0x200 /* Generic interface.  */
#define IF_MIB      0x202 /* Supports MIB-2 interface.  */

/*
 * QueryInformationEx IOCTL.  The return buffer is passed as the OutputBuffer
 * in the DeviceIoControl request.  This structure is passed as the
 * InputBuffer.
 */
struct tcp_request_query_information_ex
{
  TDIObjectID ID;                      /* object ID to query.  */
  uint8_t Context[CONTEXT_SIZE];       /* multi-request context.  Zeroed */
                                       /* for the first request.  */
};

typedef struct tcp_request_query_information_ex
   TCP_REQUEST_QUERY_INFORMATION_EX,
  *PTCP_REQUEST_QUERY_INFORMATION_EX;

#endif /* not __IPDATA_H__ */
