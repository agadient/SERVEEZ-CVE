/*
 * iphlpapi.h - IP Helper API definitons.
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

#ifndef __IPHLPAPI_H__
#define __IPHLPAPI_H__ 1

#include <windows.h>

#define MAXLEN_IFDESCR          256
#define MAXLEN_PHYSADDR         8
#define MAX_INTERFACE_NAME_LEN  256

#ifndef ANY_SIZE
# define ANY_SIZE 1
#endif

/*
 * Interface structure definitions.
 */
typedef struct _MIB_IFROW
{
  WCHAR wszName[MAX_INTERFACE_NAME_LEN];
  DWORD dwIndex;
  DWORD dwType;
  DWORD dwMtu;
  DWORD dwSpeed;
  DWORD dwPhysAddrLen;
  BYTE  bPhysAddr[MAXLEN_PHYSADDR];
  DWORD dwAdminStatus;
  DWORD dwOperStatus;
  DWORD dwLastChange;
  DWORD dwInOctets;
  DWORD dwInUcastPkts;
  DWORD dwInNUcastPkts;
  DWORD dwInDiscards;
  DWORD dwInErrors;
  DWORD dwInUnknownProtos;
  DWORD dwOutOctets;
  DWORD dwOutUcastPkts;
  DWORD dwOutNUcastPkts;
  DWORD dwOutDiscards;
  DWORD dwOutErrors;
  DWORD dwOutQLen;
  DWORD dwDescrLen;
  BYTE  bDescr[MAXLEN_IFDESCR];
}
MIB_IFROW, *PMIB_IFROW;

/*
 * Interface table structure.
 */
typedef struct _MIB_IFTABLE
{
  DWORD     dwNumEntries;
  MIB_IFROW table[ANY_SIZE];
}
MIB_IFTABLE, *PMIB_IFTABLE;

/*
 * IP address structure.
 */
typedef struct _MIB_IPADDRROW
{
  DWORD          dwAddr;
  DWORD          dwIndex;
  DWORD          dwMask;
  DWORD          dwBCastAddr;
  DWORD          dwReasmSize;
  uint16_t       unused1;
  uint16_t       unused2;
}
MIB_IPADDRROW, *PMIB_IPADDRROW;

/*
 * IP address table structure.
 */
typedef struct _MIB_IPADDRTABLE
{
  DWORD         dwNumEntries;
  MIB_IPADDRROW table[ANY_SIZE];
}
MIB_IPADDRTABLE, *PMIB_IPADDRTABLE;

/*
 * Function call prototypes.
 */
typedef DWORD (__stdcall *GetIfTableProc) (PMIB_IFTABLE, PULONG, BOOL);
typedef DWORD (__stdcall *GetIpAddrTableProc) (PMIB_IPADDRTABLE, PULONG, BOOL);

#endif /* not __IPHLPAPI_H__ */
