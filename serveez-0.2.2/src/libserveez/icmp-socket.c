/*
 * icmp-socket.c - ICMP socket implementations
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

#include "timidity.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>
#include <time.h>

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
#endif

#ifdef __MINGW32__
# include <process.h>
#endif
#include "networking-headers.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/core.h"
#include "libserveez/server-core.h"
#include "libserveez/icmp-socket.h"
#include "libserveez/raw-socket.h"
#include "libserveez/server.h"
#include "libserveez/binding.h"

#define IP_HEADER_SIZE   20

/* general definitions */
#define ICMP_ECHOREPLY          0       /* Echo Reply                   */
#define ICMP_DEST_UNREACH       3       /* Destination Unreachable      */
#define ICMP_SOURCE_QUENCH      4       /* Source Quench                */
#define ICMP_REDIRECT           5       /* Redirect (change route)      */
#define ICMP_ECHO               8       /* Echo Request                 */
#define ICMP_TIME_EXCEEDED      11      /* Time Exceeded                */
#define ICMP_PARAMETERPROB      12      /* Parameter Problem            */
#define ICMP_TIMESTAMP          13      /* Timestamp Request            */
#define ICMP_TIMESTAMPREPLY     14      /* Timestamp Reply              */
#define ICMP_INFO_REQUEST       15      /* Information Request          */
#define ICMP_INFO_REPLY         16      /* Information Reply            */
#define ICMP_ADDRESS            17      /* Address Mask Request         */
#define ICMP_ADDRESSREPLY       18      /* Address Mask Reply           */
#define ICMP_MAX_TYPE           18

/* Text representation of ICMP type codes.  */
static char *icmp_request[] = {
  "echo reply",
  NULL,
  NULL,
  "destination unreachable",
  "source quench",
  "redirect (change route)",
  NULL,
  NULL,
  "echo request",
  NULL,
  NULL,
  "time exceeded",
  "parameter problem",
  "timestamp request",
  "timestamp reply",
  "information request",
  "information reply",
  "address mask request",
  "address mask reply"
};

/* ICMP header structure.  */
typedef struct
{
  uint8_t type;            /* message type */
  uint8_t code;            /* type sub-code */
  uint16_t checksum;       /* check sum */
  uint16_t ident;          /* identifier */
  uint16_t sequence;       /* sequence number */
  in_port_t port;          /* remote port address */
}
svz_icmp_header_t;

#ifdef __MINGW32__

/*
 * Microsoft discourages the use of their ICMP.DLL API, but it seems
 * to be the only way to make use of raw sockets anyway.  The API is
 * almostly unusable because:
 * 1. you cannot receive if not previously sent a packet
 * 2. the IcmpSendEcho call is blocking
 * 3. receive and send is one call
 * 4. you cannot set the ICMP header (type, code)
 */

/*
 * Note 2: For the most part, you can refer to RFC 791 for details
 * on how to fill in values for the IP option information structure.
 */
typedef struct ip_option_information
{
  uint8_t Ttl;          /* Time To Live (used for traceroute) */
  uint8_t Tos;          /* Type Of Service (usually 0) */
  uint8_t Flags;        /* IP header flags (usually 0) */
  uint8_t OptionsSize;  /* Size of options data (usually 0, max 40) */
  uint8_t *OptionsData; /* Options data buffer */
}
IPINFO;

/*
 * Note 1: The Reply Buffer will have an array of ICMP_ECHO_REPLY
 * structures, followed by options and the data in ICMP echo reply
 * datagram received.  You must have room for at least one ICMP
 * echo reply structure, plus 8 bytes for an ICMP header.
 */
typedef struct icmp_echo_reply
{
  in_addr_t Address;       /* source address */
  unsigned long Status;    /* IP status value (see below) */
  unsigned long RTTime;    /* Round Trip Time in milliseconds */
  uint16_t DataSize;       /* reply data size */
  uint16_t Reserved;
  void *Data;              /* reply data buffer */
  IPINFO Options;          /* reply options */
}
ICMPECHO;

/*
 * DLL function definitions of IcmpCloseHandle, IcmpCreateFile,
 * IcmpParseReplies, IcmpSendEcho and IcmpSendEcho2.
 */
typedef HANDLE (__stdcall * IcmpCreateFileProc) (void);
typedef BOOL (__stdcall * IcmpCloseHandleProc) (HANDLE IcmpHandle);
typedef DWORD (__stdcall * IcmpSendEchoProc) (
  HANDLE IcmpHandle,          /* handle returned from ‘IcmpCreateFile’ */
  in_addr_t DestAddress,      /* destination IP address (in network order) */
  void *RequestData,          /* pointer to buffer to send */
  uint16_t RequestSize,       /* length of data in buffer */
  IPINFO *RequestOptns,       /* see Note 2 */
  void *ReplyBuffer,          /* see Note 1 */
  unsigned long ReplySize,    /* length of reply (at least 1 reply) */
  unsigned long Timeout       /* time in milliseconds to wait for reply */
);

/*
 * Error definitions.
 */
#define IP_STATUS_BASE           11000
#define IP_SUCCESS               0
#define IP_BUF_TOO_SMALL         (IP_STATUS_BASE + 1)
#define IP_DEST_NET_UNREACHABLE  (IP_STATUS_BASE + 2)
#define IP_DEST_HOST_UNREACHABLE (IP_STATUS_BASE + 3)
#define IP_DEST_PROT_UNREACHABLE (IP_STATUS_BASE + 4)
#define IP_DEST_PORT_UNREACHABLE (IP_STATUS_BASE + 5)
#define IP_NO_RESOURCES          (IP_STATUS_BASE + 6)
#define IP_BAD_OPTION            (IP_STATUS_BASE + 7)
#define IP_HW_ERROR              (IP_STATUS_BASE + 8)
#define IP_PACKET_TOO_BIG        (IP_STATUS_BASE + 9)
#define IP_REQ_TIMED_OUT         (IP_STATUS_BASE + 10)
#define IP_BAD_REQ               (IP_STATUS_BASE + 11)
#define IP_BAD_ROUTE             (IP_STATUS_BASE + 12)
#define IP_TTL_EXPIRED_TRANSIT   (IP_STATUS_BASE + 13)
#define IP_TTL_EXPIRED_REASSEM   (IP_STATUS_BASE + 14)
#define IP_PARAM_PROBLEM         (IP_STATUS_BASE + 15)
#define IP_SOURCE_QUENCH         (IP_STATUS_BASE + 16)
#define IP_OPTION_TOO_BIG        (IP_STATUS_BASE + 17)
#define IP_BAD_DESTINATION       (IP_STATUS_BASE + 18)
#define IP_ADDR_DELETED          (IP_STATUS_BASE + 19)
#define IP_SPEC_MTU_CHANGE       (IP_STATUS_BASE + 20)
#define IP_MTU_CHANGE            (IP_STATUS_BASE + 21)
#define IP_UNLOAD                (IP_STATUS_BASE + 22)
#define IP_GENERAL_FAILURE       (IP_STATUS_BASE + 50)
#define MAX_IP_STATUS            IP_GENERAL_FAILURE
#define IP_PENDING               (IP_STATUS_BASE + 255)

/* Functions and handles for the ICMP.DLL API.  */
static IcmpCreateFileProc IcmpCreateFile = NULL;
static IcmpCloseHandleProc IcmpCloseHandle = NULL;
static IcmpSendEchoProc IcmpSendEcho = NULL;
static HANDLE IcmpHandle = NULL;
static HANDLE hIcmp = INVALID_HANDLE_VALUE;

/*
 * Load the @file{ICMP.DLL} library into process address space and get all
 * necessary function pointers.
 */
void
svz_icmp_startup (void)
{
  /* load library */
  if ((IcmpHandle = LoadLibrary ("ICMP.DLL")) == NULL)
    {
      svz_log_sys_error ("icmp: LoadLibrary");
      return;
    }

  /* obtain functions */
  IcmpCreateFile = (IcmpCreateFileProc)
    GetProcAddress (IcmpHandle, "IcmpCreateFile");
  IcmpCloseHandle = (IcmpCloseHandleProc)
    GetProcAddress (IcmpHandle, "IcmpCloseHandle");
  IcmpSendEcho = (IcmpSendEchoProc)
    GetProcAddress (IcmpHandle, "IcmpSendEcho");
  if (IcmpSendEcho == NULL ||
      IcmpCloseHandle == NULL || IcmpCreateFile == NULL)
    {
      svz_log_sys_error ("icmp: GetProcAddress");
      FreeLibrary (IcmpHandle);
      IcmpHandle = NULL;
      return;
    }

  /* open ping service */
  if (svz_invalid_handle_p (hIcmp = IcmpCreateFile ()))
    {
      svz_log_sys_error ("IcmpCreateFile");
      FreeLibrary (IcmpHandle);
      IcmpHandle = NULL;
      return;
    }

#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "icmp services successfully initialized\n");
#endif
}

/*
 * Shutdown the ping service.
 */
void
svz_icmp_cleanup (void)
{
  /* close ip service */
  if (! svz_invalid_handle_p (hIcmp))
    {
      if (!IcmpCloseHandle (hIcmp))
       svz_log_sys_error ("IcmpCloseHandle");
    }

  /* release ICMP.DLL */
  if (IcmpHandle)
    {
      FreeLibrary (IcmpHandle);
      IcmpHandle = NULL;
    }
}
#endif /* __MINGW32__ */

/* Static buffer for ip packets.  */
static char icmp_buffer[IP_HEADER_SIZE + ICMP_HEADER_SIZE + ICMP_MSG_SIZE];

/*
 * Get ICMP header from plain data.
 */
static svz_icmp_header_t *
unpack_header (uint8_t *data)
{
  static svz_icmp_header_t hdr;
  uint16_t uint16;

  hdr.type = *data++;
  hdr.code = *data++;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.checksum = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.ident = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.sequence = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.port = uint16;

  return &hdr;
}

/*
 * Create ICMP header (data block) from given structure.
 */
static uint8_t *
pack_header (svz_icmp_header_t *hdr)
{
  static uint8_t buffer[ICMP_HEADER_SIZE];
  uint8_t *data = buffer;
  uint16_t uint16;

  *data++ = hdr->type;
  *data++ = hdr->code;
  uint16 = htons (hdr->checksum);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  uint16 = htons (hdr->ident);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  uint16 = htons (hdr->sequence);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  uint16 = hdr->port;
  memcpy (data, &uint16, SIZEOF_UINT16);

  return buffer;
}

#define ICMP_ERROR      -1
#define ICMP_DISCONNECT -2

/*
 * Parse and check IP and ICMP header.  Return the amount of leading bytes
 * to be truncated.  Return ICMP_ERROR on packet errors and return
 * ICMP_DISCONNECT when we received an disconnection signal.
 */
static int
check_packet (svz_socket_t *sock, uint8_t *data, int len)
{
  int length;
  uint8_t *p = data;
  svz_icmp_header_t *header;

  /* First check the IP header.  */
  if ((length = svz_raw_check_ip_header (p, len)) == -1)
    return ICMP_ERROR;

  /* Get the actual ICMP header.  */
  header = unpack_header (p + length);
  p += length + ICMP_HEADER_SIZE;
  len -= length + ICMP_HEADER_SIZE;

  /* Do these checks only if it is the right kind of packet.  */
  if (header->type == sock->itype)
    {
      /* validate the ICMP data checksum */
      if (header->checksum != svz_raw_ip_checksum (p, len))
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "icmp: invalid data checksum\n");
#endif
          return ICMP_ERROR;
        }

      /* check the ICMP header identification */
      if (header->ident == getpid () + sock->id)
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "icmp: rejecting native packet\n");
#endif
          return ICMP_ERROR;
        }

      /* check ICMP remote port */
      if ((header->port != sock->remote_port) &&
          !(sock->flags & SVZ_SOFLG_LISTENING))
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "icmp: rejecting filtered packet\n");
#endif
          return ICMP_ERROR;
        }
      sock->remote_port = header->port;
    }

  /* What kind of packet is this ? */
#if ENABLE_DEBUG
  else if (header->type <= ICMP_MAX_TYPE)
    {
      if (icmp_request[header->type])
        svz_log (SVZ_LOG_DEBUG, "icmp: %s received\n",
                 icmp_request[header->type]);
      else
        svz_log (SVZ_LOG_DEBUG, "unsupported protocol 0x%02X received\n",
                 header->type);
      return ICMP_ERROR;
    }
#endif /* ENABLE_DEBUG */

  if (header->type == sock->itype)
    {
      if (header->code == SVZ_ICMP_SERVEEZ_CONNECT &&
          sock->flags & SVZ_SOFLG_LISTENING)
        {
          svz_log (SVZ_LOG_NOTICE, "icmp: accepting connection\n");
        }
      else if (header->code == SVZ_ICMP_SERVEEZ_CLOSE)
        {
          svz_log (SVZ_LOG_NOTICE, "icmp: closing connection\n");
          return ICMP_DISCONNECT;
        }
      return (length + ICMP_HEADER_SIZE);
    }
#if ENABLE_DEBUG
  else
    {
      svz_log (SVZ_LOG_DEBUG, "unsupported protocol 0x%02X received\n",
               header->type);
    }
#endif /* ENABLE_DEBUG */

  return ICMP_ERROR;
}

/*
 * Default reader for ICMP sockets.  The sender is stored within
 * @code{sock->remote_addr} and @code{sock->remote_port} afterwards.
 */
static int
read_socket (svz_socket_t *sock)
{
  int num_read;
  socklen_t len;
  struct sockaddr_in sender;
  int trunc;

  len = sizeof (struct sockaddr_in);

  /* Receive data.  */
  if (!(sock->flags & SVZ_SOFLG_CONNECTED))
    {
      num_read = recvfrom (sock->sock_desc, icmp_buffer,
                           sizeof (icmp_buffer), 0,
                           (struct sockaddr *) &sender, &len);
    }
  else
    {
      num_read = recv (sock->sock_desc, icmp_buffer,
                       sizeof (icmp_buffer), 0);
    }

  /* Valid packet data arrived.  */
  if (num_read > 0)
    {
#if 0
      svz_hexdump (stdout, "icmp packet received", sock->sock_desc,
                   icmp_buffer, num_read, 0);
#endif
      sock->last_recv = time (NULL);
      if (!(sock->flags & SVZ_SOFLG_FIXED))
        SVZ_SET_ADDR (sock->remote_addr, AF_INET, &sender.sin_addr.s_addr);
#if ENABLE_DEBUG
      {
        char buf[64];

        svz_log (SVZ_LOG_DEBUG, "icmp: recv%s: %s (%u bytes)\n",
                 sock->flags & SVZ_SOFLG_CONNECTED ? "" : "from",
                 SVZ_PP_ADDR (buf, sock->remote_addr), num_read);
      }
#endif /* ENABLE_DEBUG */

      /*
       * Check the ICMP packet and put the packet load only into the
       * receive buffer of the socket structure.
       */
      trunc = check_packet (sock, (uint8_t *) icmp_buffer, num_read);
      if (trunc >= 0)
        {
          num_read -= trunc;
          if (num_read > sock->recv_buffer_size - sock->recv_buffer_fill)
            {
              svz_log (SVZ_LOG_ERROR,
                       "receive buffer overflow on icmp socket %d\n",
                       sock->sock_desc);
              return -1;
            }

          memcpy (sock->recv_buffer + sock->recv_buffer_fill,
                  icmp_buffer + trunc, num_read);
          sock->recv_buffer_fill += num_read;

          /* Check access lists.  */
          if (svz_sock_check_access (sock, sock) < 0)
            return 0;

          if (sock->check_request)
            sock->check_request (sock);
        }
      else if (trunc == ICMP_DISCONNECT)
        {
          return -1;
        }
    }
  /* Some error occurred.  */
  else
    {
      svz_log_net_error ("icmp: recv%s", (sock->flags & SVZ_SOFLG_CONNECTED
                                          ? ""
                                          : "from"));
      if (! svz_socket_unavailable_error_p ())
        return -1;
    }
  return 0;
}

/*
 * Default reader for ICMP server sockets.  Allocates necessary buffers and
 * reverts to @code{read_socket}.
 */
int
svz_icmp_lazy_read_socket (svz_socket_t *sock)
{
  svz_portcfg_t *port = sock->port;

  svz_sock_resize_buffers (sock, port->send_buffer_size,
                           port->recv_buffer_size);
  sock->read_socket = read_socket;

  return sock->read_socket (sock);
}

/*
 * The default ICMP write callback is called whenever the socket
 * descriptor has been @code{select}'ed or @code{poll}'ed to be ready for
 * sending.
 */
int
svz_icmp_write_socket (svz_socket_t *sock)
{
  int num_written;
  unsigned do_write;
  char *p;
  socklen_t len;
  struct sockaddr_in receiver;

  /* Return here if there is nothing to do.  */
  if (sock->send_buffer_fill <= 0)
    return 0;

  len = sizeof (struct sockaddr_in);
  receiver.sin_family = AF_INET;

  /* Get destination address and data length from send buffer.  */
  p = sock->send_buffer;
  memcpy (&do_write, p, sizeof (do_write));
  p += sizeof (do_write);
  memcpy (&receiver.sin_addr.s_addr, p, sizeof (in_addr_t));
  p += sizeof (in_addr_t);
  memcpy (&receiver.sin_port, p, sizeof (sock->remote_port));
  p += sizeof (sock->remote_port);
  assert ((int) do_write <= sock->send_buffer_fill);

  /* If socket is `connect ()'ed use `send ()' instead of `sendto ()'.  */
  if (!(sock->flags & SVZ_SOFLG_CONNECTED))
    {
      num_written = sendto (sock->sock_desc, p,
                            do_write - (p - sock->send_buffer),
                            0, (struct sockaddr *) &receiver, len);
    }
  else
    {
      num_written = send (sock->sock_desc, p,
                          do_write - (p - sock->send_buffer), 0);
    }

  /* Some error occurred while sending.  */
  if (num_written < 0)
    {
      svz_log_net_error ("icmp: send%s", (sock->flags & SVZ_SOFLG_CONNECTED
                                          ? ""
                                          : "to"));
      if (svz_socket_unavailable_error_p ())
        num_written = 0;
    }
  /* Packet data could be transmitted.  */
  else
    {
      sock->last_send = time (NULL);
      if ((unsigned) sock->send_buffer_fill > do_write)
        {
          memmove (sock->send_buffer,
                   sock->send_buffer + do_write,
                   sock->send_buffer_fill - do_write);
        }
      sock->send_buffer_fill -= do_write;
    }

#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "icmp: send%s: %s (%u bytes)\n",
           sock->flags & SVZ_SOFLG_CONNECTED ? "" : "to",
           svz_inet_ntoa (receiver.sin_addr.s_addr),
           do_write - (p - sock->send_buffer));
#endif /* ENABLE_DEBUG */

  return num_written < 0 ? -1 : 0;
}

/**
 * ``If you are calling this function we will send an empty ICMP packet
 * signaling that this connection is going down soon.''
 * [ttn sez: huh?]
 */
int
svz_icmp_send_control (svz_socket_t *sock, uint8_t type)
{
  static char *buffer = icmp_buffer;
  svz_icmp_header_t hdr;
  unsigned len;
  int ret = 0;

  len = sizeof (len);
  svz_address_to (&buffer[len], sock->remote_addr);
  len += sizeof (in_addr_t);
  memcpy (&buffer[len], &sock->remote_port, sizeof (sock->remote_port));
  len += sizeof (sock->remote_port);

  hdr.type = sock->itype;
  hdr.code = type;
  hdr.checksum = svz_raw_ip_checksum (NULL, 0);
  hdr.ident = (uint16_t) (getpid () + sock->id);
  hdr.sequence = sock->send_seq;
  hdr.port = sock->remote_port;
  memcpy (&buffer[len], pack_header (&hdr), ICMP_HEADER_SIZE);
  len += ICMP_HEADER_SIZE;
  memcpy (buffer, &len, sizeof (len));

  if ((ret = svz_sock_write (sock, buffer, len)) == -1)
    {
      sock->flags |= SVZ_SOFLG_KILLED;
    }
  return ret;
}

/**
 * Send @var{buf} with length @var{length} via this ICMP socket @var{sock}.
 * If @var{length} supersedes the maximum ICMP message size the buffer is
 * split into smaller packets.
 */
int
svz_icmp_write (svz_socket_t *sock, char *buf, int length)
{
  static char *buffer = icmp_buffer;
  svz_icmp_header_t hdr;
  unsigned len, size;
  int ret = 0;

  /* Return if the socket has already been killed.  */
  if (sock->flags & SVZ_SOFLG_KILLED)
    return 0;

  while (length)
    {
      /*
       * Put the data length and destination address in front
       * of each packet.
       */
      len = sizeof (len);
      svz_address_to (&buffer[len], sock->remote_addr);
      len += sizeof (in_addr_t);
      memcpy (&buffer[len], &sock->remote_port, sizeof (sock->remote_port));
      len += sizeof (sock->remote_port);
      if ((size = length) > ICMP_MSG_SIZE)
        size = ICMP_MSG_SIZE;

      /* Create ICMP header and put it in front of packet load.  */
      hdr.type = sock->itype;
      hdr.code = SVZ_ICMP_SERVEEZ_DATA;
      hdr.checksum = svz_raw_ip_checksum ((uint8_t *) buf, size);
      hdr.ident = (uint16_t) (getpid () + sock->id);
      hdr.sequence = sock->send_seq++;
      hdr.port = sock->remote_port;
      memcpy (&buffer[len], pack_header (&hdr), ICMP_HEADER_SIZE);
      len += ICMP_HEADER_SIZE;

      /* Copy the given buffer.  */
      memcpy (&buffer[len], buf, size);
      len += size;

      /* Put chunk length to buffer.  */
      memcpy (buffer, &len, sizeof (len));
      buf += size;
      length -= size;

      /* Actually send the data or put it into the send buffer queue.  */
      if ((ret = svz_sock_write (sock, buffer, len)) == -1)
        {
          sock->flags |= SVZ_SOFLG_KILLED;
          break;
        }
    }

  return ret;
}

/*
 * Default @code{check_request} callback for ICMP sockets.
 */
int
svz_icmp_check_request (svz_socket_t *sock)
{
  size_t n;
  svz_server_t *server;
  svz_array_t *bindings;
  svz_binding_t *binding;

  if (svz_sock_bindings (sock) == NULL && sock->handle_request == NULL)
    return -1;

  /*
   * If there is a valid `handle_request' callback (dedicated icmp
   * connection) call it.  This kind of behaviour is due to a socket
   * creation via 'icmp_connect' (s.b.) and setting up a static
   * `handle_request' callback.
   */
  if (sock->handle_request)
    {
      if (sock->handle_request (sock, sock->recv_buffer,
                                sock->recv_buffer_fill))
        return -1;
      sock->recv_buffer_fill = 0;
      return 0;
    }

  /* Go through all icmp servers on this server socket.  */
  bindings = svz_binding_filter (sock);
  svz_array_foreach (bindings, binding, n)
    {
      server = binding->server;
      sock->cfg = server->cfg;

      if (server->handle_request)
        {
          if (!server->handle_request (sock, sock->recv_buffer,
                                       sock->recv_buffer_fill))
            {
              sock->recv_buffer_fill = 0;
              break;
            }
        }
    }
  svz_array_destroy (bindings);

  /* Check if any server processed this packet.  */
  if (sock->recv_buffer_fill)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "rejecting icmp packet on socket %d\n",
               sock->sock_desc);
#endif
      sock->recv_buffer_fill = 0;
    }

  sock->cfg = NULL;
  return 0;
}

/**
 * Create an ICMP socket for receiving and sending.
 * Return @code{NULL} on errors, otherwise an enqueued socket structure.
 */
svz_socket_t *
svz_icmp_connect (svz_address_t *host, in_port_t port, uint8_t type)
{
  svz_t_socket sockfd;
  svz_socket_t *sock;

  STILL_NO_V6_DAMMIT (host);

  /* Create a client socket.  */
  if ((sockfd = svz_socket_create (SVZ_PROTO_ICMP)) == (svz_t_socket) -1)
      return NULL;

  /* Try to connect to the server.  Does it make sense for ICMP ? */
  if (svz_socket_connect (sockfd, host, port) == -1)
     return NULL;

  /* Create socket structure and enqueue it.  */
  if ((sock = svz_sock_alloc ()) == NULL)
    {
      svz_closesocket (sockfd);
      return NULL;
    }

  svz_sock_resize_buffers (sock, ICMP_BUF_SIZE, ICMP_BUF_SIZE);
  svz_sock_unique_id (sock);
  sock->sock_desc = sockfd;
  sock->proto = SVZ_PROTO_ICMP;
  sock->flags |= (SVZ_SOFLG_SOCK | SVZ_SOFLG_CONNECTED | SVZ_SOFLG_FIXED);
  sock->itype = type;
  svz_sock_enqueue (sock);
  svz_sock_intern_connection_info (sock);

  /* Put foreign address here.  */
  sock->remote_addr = svz_address_copy (host);
  sock->remote_port = sock->id;

  sock->read_socket = read_socket;
  sock->write_socket = svz_icmp_write_socket;
  sock->check_request = svz_icmp_check_request;

  /* Finally send a connection message.  */
  svz_icmp_send_control (sock, SVZ_ICMP_SERVEEZ_CONNECT);
  svz_sock_connections++;
  return sock;
}
