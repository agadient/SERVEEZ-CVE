/*
 * guile-api.c - export additional Serveez functionality to Guile
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

#if ENABLE_GUILE_SERVER

#include <string.h>
#include <errno.h>

#ifndef __MINGW32__
# include <sys/socket.h>
# include <netdb.h>
#endif

#if HAVE_RPC_RPCENT_H
# include <rpc/rpcent.h>
#endif
#if HAVE_RPC_RPC_H
# include <rpc/rpc.h>
#endif
#if HAVE_RPC_CLNT_SOC_H
# include <rpc/clnt_soc.h>
#endif
#if HAVE_RPC_PMAP_CLNT_H
# include <rpc/pmap_clnt.h>
#endif
#if HAVE_RPC_PMAP_PROT_H
# include <rpc/pmap_prot.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_IO_H
# include <io.h>
#endif

#include <libguile.h>
#include "networking-headers.h"
#include "libserveez.h"
#include "guile-missing.h"

/* Validate network port range.  */
#define VALIDATE_NETPORT(port, cell, arg) do {                       \
    (port) = gi_scm2long (cell);                                     \
    if ((port) < 0 || (port) >= 65536) SCM_OUT_OF_RANGE (arg, cell); \
  } while (0)


/* Converts the given hostname @var{host} into a Internet address in host
   byte order and stores it into @var{addr}.  Returns zero on success.  This
   is a blocking operation.  */
static int
guile_resolve (char *host, in_addr_t *addr)
{
  struct hostent *ent;

  if ((ent = gethostbyname (host)) == NULL)
    return -1;
  if (ent->h_addrtype == AF_INET)
    {
      memcpy (addr, ent->h_addr_list[0], ent->h_length);
      return 0;
    }
  return -1;
}

SCM_DEFINE
(guile_sock_connect,
 "svz:sock:connect", 2, 1, 0,
 (SCM host, SCM proto, SCM port),
 doc: /***********
Establish a network connection to the given @var{host} [ :@var{port} ].
If @var{proto} equals @code{PROTO_ICMP} the @var{port} argument is
ignored.  Valid identifiers for @var{proto} are @code{PROTO_TCP},
@code{PROTO_UDP} and @code{PROTO_ICMP}.  The @var{host} argument must be
either a string in dotted decimal form, a valid hostname or an exact number
in host byte order.  When giving a hostname this operation might block.
The @var{port} argument must be an exact number in the range from
0 to 65535, also in host byte order.  Return a valid @code{#<svz-socket>}
or @code{#f} on failure.  */)
{
#define FUNC_NAME s_guile_sock_connect
  svz_socket_t *sock;
  in_addr_t v4addr;
  svz_address_t *xhost;
  in_port_t xport = 0;
  long p;
  int xproto;
  struct sockaddr_in addr;
  SCM ret = SCM_BOOL_F;

  SCM_ASSERT_TYPE (gi_exactp (host) || gi_stringp (host),
                   host, SCM_ARG1, FUNC_NAME, "string or exact");
  ASSERT_EXACT (2, proto);

  /* Extract host to connect to.  */
  if (gi_exactp (host))
    v4addr = htonl (gi_scm2int (host));
  else
    {
      char str[128];

      GI_GET_XREP (str, host);
      if (svz_inet_aton (str, &addr) == -1)
        {
          if (guile_resolve (str, &v4addr) == -1)
            {
              guile_error ("%s: IP in dotted decimals or hostname expected",
                           FUNC_NAME);
              return ret;
            }
        }
      else
        v4addr = addr.sin_addr.s_addr;
    }
  xhost = svz_address_make (AF_INET, &v4addr);

  /* Extract protocol to use.  */
  xproto = gi_scm2int (proto);

  /* Find out about given port.  */
  if (BOUNDP (port))
    {
      ASSERT_EXACT (3, port);
      VALIDATE_NETPORT (p, port, SCM_ARG3);
      xport = htons (p);
    }

  /* Depending on the requested protocol; create different kinds of
     socket structures.  */
  switch (xproto)
    {
    case SVZ_PROTO_TCP:
      sock = svz_tcp_connect (xhost, xport);
      break;
    case SVZ_PROTO_UDP:
      sock = svz_udp_connect (xhost, xport);
      break;
    case SVZ_PROTO_ICMP:
      sock = svz_icmp_connect (xhost, xport, SVZ_ICMP_SERVEEZ);
      break;
    default:
      SCM_OUT_OF_RANGE (SCM_ARG2, proto);
    }
  svz_free (xhost);

  if (sock == NULL)
    return ret;

  sock->disconnected_socket = guile_func_disconnected_socket;
  return socket_smob (sock);
#undef FUNC_NAME
}

#if GUILE_MISSING_inet_ntoa
SCM_DEFINE
(guile_svz_inet_ntoa,
 "inet-ntoa", 1, 0, 0,
 (SCM address),
 doc: /***********
Convert the Internet host address
@var{address} given in network byte order to a string in standard
numbers-and-dots notation.  */)
{
#define FUNC_NAME s_guile_svz_inet_ntoa
  char *str;
  ASSERT_EXACT (1, address);
  str = svz_inet_ntoa (gi_scm2ulong (address));
  return gi_string2scm (str);
#undef FUNC_NAME
}
#endif  /* GUILE_MISSING_inet_ntoa */

#if GUILE_MISSING_inet_aton
SCM_DEFINE
(guile_svz_inet_aton,
 "inet-aton", 1, 0, 0,
 (SCM address),
 doc: /***********
Convert the Internet host address @var{address} from the standard
numbers-and-dots notation into binary data in network byte order.
Return @code{#f} if the address is invalid.  */)
{
#define FUNC_NAME s_guile_svz_inet_aton
  struct sockaddr_in addr;
  char str[48];

  ASSERT_STRING (1, address);
  GI_GET_XREP (str, address);
  if (svz_inet_aton (str, &addr) == -1)
    {
      guile_error ("%s: IP address in dotted decimals expected", FUNC_NAME);
      return SCM_BOOL_F;
    }
  return gi_nnint2scm (addr.sin_addr.s_addr);
#undef FUNC_NAME
}
#endif  /* GUILE_MISSING_inet_aton */

#if GUILE_MISSING_ntohl
SCM_DEFINE
(guile_svz_ntohl,
 "ntohl", 1, 0, 0,
 (SCM netlong),
 doc: /***********
Convert the 32 bit long integer
@var{netlong} from network byte order to host byte order.  */)
{
#define FUNC_NAME s_guile_svz_ntohl
  ASSERT_EXACT (1, netlong);
  return gi_nnint2scm (ntohl (gi_scm2ulong (netlong)));
#undef FUNC_NAME
}
#endif  /* GUILE_MISSING_ntohl */

#if GUILE_MISSING_htonl
SCM_DEFINE
(guile_svz_htonl,
 "htonl", 1, 0, 0,
 (SCM hostlong),
 doc: /***********
Convert the 32 bit long integer
@var{hostlong} from host byte order to network byte order.  */)
{
#define FUNC_NAME s_guile_svz_htonl
  ASSERT_EXACT (1, hostlong);
  return gi_nnint2scm (htonl (gi_scm2ulong (hostlong)));
#undef FUNC_NAME
}
#endif  /* GUILE_MISSING_htonl */

#if GUILE_MISSING_ntohs
SCM_DEFINE
(guile_svz_ntohs,
 "ntohs", 1, 0, 0,
 (SCM netshort),
 doc: /***********
Convert the 16 bit short integer
@var{netshort} from network byte order to host byte order.  */)
{
#define FUNC_NAME s_guile_svz_ntohs
  long i;
  ASSERT_EXACT (1, netshort);
  VALIDATE_NETPORT (i, netshort, SCM_ARG1);
  return gi_integer2scm (ntohs (i));
#undef FUNC_NAME
}
#endif  /* GUILE_MISSING_ntohs */

#if GUILE_MISSING_htons
SCM_DEFINE
(guile_svz_htons,
 "htons", 1, 0, 0,
 (SCM hostshort),
 doc: /***********
Convert the 16 bit short integer
@var{hostshort} from host byte order to network byte order.  */)
{
#define FUNC_NAME s_guile_svz_htons
  long i;
  ASSERT_EXACT (1, hostshort);
  VALIDATE_NETPORT (i, hostshort, SCM_ARG1);
  return gi_integer2scm (htons (i));
#undef FUNC_NAME
}
#endif  /* GUILE_MISSING_htons */

SCM_DEFINE
(guile_sock_receive_buffer,
 "svz:sock:receive-buffer", 1, 0, 0,
 (SCM sock),
 doc: /***********
Return the receive buffer of the
socket @var{sock} as a binary smob.  */)
{
#define FUNC_NAME s_guile_sock_receive_buffer
  svz_socket_t *xsock;
  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  return guile_data_to_bin (xsock->recv_buffer, xsock->recv_buffer_fill);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_receive_buffer_size,
 "svz:sock:receive-buffer-size", 1, 1, 0,
 (SCM sock, SCM size),
 doc: /***********
Return the current receive buffers size and fill status in bytes of
the socket @var{sock} as a pair of exact numbers.  If the optional
argument @var{size} is given, set the receive buffer to the
specified size in bytes.  */)
{
#define FUNC_NAME s_guile_sock_receive_buffer_size
  svz_socket_t *xsock;
  int len;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  if (BOUNDP (size))
    {
      ASSERT_EXACT (2, size);
      len = gi_scm2int (size);
      svz_sock_resize_buffers (xsock, xsock->send_buffer_size, len);
    }
  return scm_cons (gi_integer2scm (xsock->recv_buffer_size),
                   gi_integer2scm (xsock->recv_buffer_fill));
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_send_buffer,
 "svz:sock:send-buffer", 1, 0, 0,
 (SCM sock),
 doc: /***********
Return the send buffer of the
socket @var{sock} as a binary smob.  */)
{
#define FUNC_NAME s_guile_sock_send_buffer
  svz_socket_t *xsock;
  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  return guile_data_to_bin (xsock->send_buffer, xsock->send_buffer_fill);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_send_buffer_size,
 "svz:sock:send-buffer-size", 1, 1, 0,
 (SCM sock, SCM size),
 doc: /***********
Return the current send buffer size and fill status in
bytes of the socket @var{sock} as a pair of exact numbers.  If the
optional argument @var{size} is given, set the send buffer to
the specified size in bytes.  */)
{
#define FUNC_NAME s_guile_sock_send_buffer_size
  svz_socket_t *xsock;
  int len;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  if (BOUNDP (size))
    {
      ASSERT_EXACT (2, size);
      len = gi_scm2int (size);
      svz_sock_resize_buffers (xsock, len, xsock->recv_buffer_size);
    }
  return scm_cons (gi_integer2scm (xsock->send_buffer_size),
                   gi_integer2scm (xsock->send_buffer_fill));
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_receive_buffer_reduce,
 "svz:sock:receive-buffer-reduce", 1, 1, 0,
 (SCM sock, SCM length),
 doc: /***********
Dequeue @var{length} bytes from the receive buffer of the
socket @var{sock}, or all bytes if @var{length} is omitted.
Return the number of bytes actually shuffled away.  */)
{
#define FUNC_NAME s_guile_sock_receive_buffer_reduce
  svz_socket_t *xsock;
  int len;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);

  /* Check if second length argument is given.  */
  if (BOUNDP (length))
    {
      ASSERT_EXACT (2, length);
      len = gi_scm2int (length);
      if (len < 0 || len > xsock->recv_buffer_fill)
        SCM_OUT_OF_RANGE (SCM_ARG2, length);
    }
  else
    {
      len = xsock->recv_buffer_fill;
    }
  svz_sock_reduce_recv (xsock, len);
  return gi_integer2scm (len);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_remote_address,
 "svz:sock:remote-address", 1, 1, 0,
 (SCM sock, SCM address),
 doc: /***********
Return the current remote address as a pair like
@code{(host . port)} with both entries in network byte order.  If you pass
the optional argument @var{address}, you can set the remote address of
the socket @var{sock}.  */)
{
#define FUNC_NAME s_guile_sock_remote_address
  svz_socket_t *xsock;
  in_addr_t v4addr;
  long port;
  SCM pair;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  svz_address_to (&v4addr, xsock->remote_addr);
  pair = scm_cons (gi_nnint2scm (v4addr),
                   gi_integer2scm ((int) xsock->remote_port));
  if (BOUNDP (address))
    {
      SCM_ASSERT_TYPE (SCM_PAIRP (address) && gi_exactp (SCM_CAR (address))
                       && gi_exactp (SCM_CDR (address)), address, SCM_ARG2,
                       FUNC_NAME, "pair of exact");
      VALIDATE_NETPORT (port, SCM_CDR (address), SCM_ARG2);
      v4addr = gi_scm2ulong (SCM_CAR (address));
      SVZ_SET_ADDR (xsock->remote_addr, AF_INET, &v4addr);
      xsock->remote_port = port;
    }
  return pair;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_local_address,
 "svz:sock:local-address", 1, 1, 0,
 (SCM sock, SCM address),
 doc: /***********
Return the current local address as a pair like
@code{(host . port)} with both entries in network byte order.  If you pass
the optional argument @var{address}, you can set the local address of
the socket @var{sock}.  */)
{
#define FUNC_NAME s_guile_sock_local_address
  svz_socket_t *xsock;
  in_addr_t v4addr;
  long port;
  SCM pair;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  svz_address_to (&v4addr, xsock->local_addr);
  pair = scm_cons (gi_nnint2scm (v4addr),
                   gi_integer2scm ((int) xsock->local_port));
  if (BOUNDP (address))
    {
      SCM_ASSERT_TYPE (SCM_PAIRP (address) && gi_exactp (SCM_CAR (address))
                       && gi_exactp (SCM_CDR (address)), address, SCM_ARG2,
                       FUNC_NAME, "pair of exact");
      VALIDATE_NETPORT (port, SCM_CDR (address), SCM_ARG2);
      v4addr = gi_scm2ulong (SCM_CAR (address));
      SVZ_SET_ADDR (xsock->local_addr, AF_INET, &v4addr);
      xsock->local_port = port;
    }
  return pair;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_parent,
 "svz:sock:parent", 1, 1, 0,
 (SCM sock, SCM parent),
 doc: /***********
Return the given socket's @var{sock} parent and optionally set it to the
socket @var{parent}.  Return either a valid
@code{#<svz-socket>} object or an empty list.  */)
{
#define FUNC_NAME s_guile_sock_parent
  SCM oparent = SCM_EOL;
  svz_socket_t *xsock, *xparent;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  if ((xparent = svz_sock_getparent (xsock)) != NULL)
    oparent = socket_smob (xparent);
  if (BOUNDP (parent))
    {
      CHECK_SMOB_ARG (socket, parent, SCM_ARG2, "svz-socket", xparent);
      svz_sock_setparent (xsock, xparent);
    }
  return oparent;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_referrer,
 "svz:sock:referrer", 1, 1, 0,
 (SCM sock, SCM referrer),
 doc: /***********
Return the given socket's @var{sock} referrer and optionally set it to the
socket @var{referrer}.  Return either a valid
@code{#<svz-socket>} or an empty list.  */)
{
#define FUNC_NAME s_guile_sock_referrer
  SCM oreferrer = SCM_EOL;
  svz_socket_t *xsock, *xreferrer;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  if ((xreferrer = svz_sock_getreferrer (xsock)) != NULL)
    oreferrer = socket_smob (xreferrer);
  if (BOUNDP (referrer))
    {
      CHECK_SMOB_ARG (socket, referrer, SCM_ARG2, "svz-socket", xreferrer);
      svz_sock_setreferrer (xsock, xreferrer);
    }
  return oreferrer;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_server,
 "svz:sock:server", 1, 1, 0,
 (SCM sock, SCM server),
 doc: /***********
Return the @code{#<svz-server>} object associated with the
given argument @var{sock}.  The optional argument @var{server} can be used
to redefine this association and must be a valid @code{#<svz-server>}
object.  For a usual socket callback like @code{connect-socket} or
@code{handle-request}, the association is already in place.  But for sockets
created by @code{svz:sock:connect}, you can use it in order to make the
returned socket object part of a server.  */)
{
#define FUNC_NAME s_guile_sock_server
  SCM oserver = SCM_EOL;
  svz_socket_t *xsock;
  svz_server_t *xserver;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  if ((xserver = svz_server_find (xsock->cfg)) != NULL)
    oserver = server_smob (xserver);
  if (BOUNDP (server))
    {
      CHECK_SMOB_ARG (server, server, SCM_ARG2, "svz-server", xserver);
      xsock->cfg = xserver->cfg;
    }
  return oserver;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_protocol,
 "svz:sock:protocol", 1, 0, 0,
 (SCM sock),
 doc: /***********
Return one of the @code{PROTO_TCP}, @code{PROTO_UDP}, @code{PROTO_ICMP},
@code{PROTO_RAW} or @code{PROTO_PIPE} constants indicating the type of
the socket structure @var{sock}.  If there is no protocol information
available, return @code{#f}.  */)
{
#define FUNC_NAME s_guile_sock_protocol
  svz_socket_t *xsock;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  return gi_integer2scm (xsock->proto);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_final_print,
 "svz:sock:final-print", 1, 0, 0,
 (SCM sock),
 doc: /***********
Schedule the socket @var{sock} for shutdown after all data
within the send buffer queue has been sent.  You should call this
right @strong{before} the last call to @code{svz:sock:print}.  */)
{
#define FUNC_NAME s_guile_sock_final_print
  svz_socket_t *xsock;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  xsock->flags |= SVZ_SOFLG_FINAL_WRITE;
  return SCM_UNSPECIFIED;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_no_delay,
 "svz:sock:no-delay", 1, 1, 0,
 (SCM sock, SCM enable),
 doc: /***********
Turn the Nagle algorithm for the TCP socket @var{sock} on or off depending
on the optional @var{enable} argument.  Return the previous state of this
flag (@code{#f} if Nagle is active, @code{#t} otherwise).  By default this
flag is switched off.  This socket option is useful when dealing with small
packet transfer in order to disable unnecessary delays.  */)
{
#define FUNC_NAME s_guile_sock_no_delay
  svz_socket_t *xsock;
  int old = 0, set = 0;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  if (xsock->proto & SVZ_PROTO_TCP)
    {
      if (BOUNDP (enable))
        {
          SCM_ASSERT_TYPE (SCM_BOOLP (enable) || gi_exactp (enable),
                           enable, SCM_ARG2, FUNC_NAME, "boolean or exact");
          if ((SCM_BOOLP (enable) && gi_nfalsep (enable) != 0) ||
              (gi_exactp (enable) && gi_scm2int (enable) != 0))
            set = 1;
        }
      if (svz_tcp_nodelay (xsock->sock_desc, set, &old) < 0)
        old = 0;
      else if (!BOUNDP (enable))
        svz_tcp_nodelay (xsock->sock_desc, old, NULL);
    }
  return SCM_BOOL (old);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_p,
 "svz:sock?", 1, 0, 0,
 (SCM sock),
 doc: /***********
Return @code{#t} if the given cell @var{sock} is an instance of a valid
@code{#<svz-socket>}, otherwise @code{#f}.  */)
{
#define FUNC_NAME s_guile_sock_p
  return CHECK_SMOB (socket, sock) ? SCM_BOOL_T : SCM_BOOL_F;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_server_p,
 "svz:server?", 1, 0, 0,
 (SCM server),
 doc: /***********
Return @code{#t} if the given cell @var{server} is an instance of a valid
@code{#<svz-server>}, otherwise @code{#f}.  */)
{
#define FUNC_NAME s_guile_server_p
  return CHECK_SMOB (server, server) ? SCM_BOOL_T : SCM_BOOL_F;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_disconnected_socket,
 "svz:sock:disconnected", 1, 1, 0,
 (SCM sock, SCM proc),
 doc: /***********
Set the @code{disconnected-socket} member of the socket structure
@var{sock} to @var{proc}.  The given callback
runs whenever the socket is lost for some external reason.
Return the previously set handler if there is one.  */)
{
#define FUNC_NAME s_guile_sock_disconnected_socket
  SOCK_CALLBACK_BODY (disconnected_socket, sfn_disconnected);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_kicked_socket,
 "svz:sock:kicked", 1, 1, 0,
 (SCM sock, SCM proc),
 doc: /***********
Set the @code{kicked-socket} callback of the given socket structure
@var{sock} to @var{proc} and return any previously
set procedure.  This callback gets called whenever the socket gets
closed by Serveez intentionally.  */)
{
#define FUNC_NAME s_guile_sock_kicked_socket
  SOCK_CALLBACK_BODY (kicked_socket, sfn_kicked);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_trigger_cond,
 "svz:sock:trigger-condition", 1, 1, 0,
 (SCM sock, SCM proc),
 doc: /***********
Set the @code{trigger-condition} callback for the socket
structure @var{sock} to @var{proc}.  Return the
previously set procedure if available.  The callback is run once every
server loop indicating whether the @code{trigger} callback should be
run or not.  */)
{
#define FUNC_NAME s_guile_sock_trigger_cond
  SOCK_CALLBACK_BODY (trigger_cond, sfn_trigger_condition);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_trigger_func,
 "svz:sock:trigger", 1, 1, 0,
 (SCM sock, SCM proc),
 doc: /***********
Set the @code{trigger} callback of the socket structure @var{sock} to
@var{proc} and return any previously set procedure.
The callback is run when the @code{trigger-condition} callback returns
@code{#t}.  */)
{
#define FUNC_NAME s_guile_sock_trigger_func
  SOCK_CALLBACK_BODY (trigger_func, sfn_trigger);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_idle_func,
 "svz:sock:idle", 1, 1, 0,
 (SCM sock, SCM proc),
 doc: /***********
Set the @code{idle} callback of the socket structure
@var{sock} to @var{proc}.  Return any previously
set procedure.  The callback is run by the periodic task scheduler when the
@code{idle-counter} of the socket structure drops to zero.  If this counter
is not zero it gets decremented once a second.  The @code{idle}
callback can reset @code{idle-counter} to some value and thus can
re-schedule itself for a later task.  */)
{
#define FUNC_NAME s_guile_sock_idle_func
  SOCK_CALLBACK_BODY (idle_func, sfn_idle);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_check_request_oob,
 "svz:sock:check-oob-request", 1, 1, 0,
 (SCM sock, SCM proc),
 doc: /***********
Set the @code{check-oob-request} callback of the given socket
structure @var{sock} to @var{proc}, returning
the previous callback (if there was any set before).
The callback is run whenever urgent data (out-of-band)
has been detected on the socket.  */)
{
#define FUNC_NAME s_guile_sock_check_request_oob
  SOCK_CALLBACK_BODY (check_request_oob, sfn_check_oob_request);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_idle_counter,
 "svz:sock:idle-counter", 1, 1, 0,
 (SCM sock, SCM counter),
 doc: /***********
Return the socket structure @var{sock}'s current
@code{idle-counter} value.  If the optional argument @var{counter} is
given, the set the @code{idle-counter}.  Please have a look at the
@code{svz:sock:idle} procedure for the exact meaning of this value.  */)
{
#define FUNC_NAME s_guile_sock_idle_counter
  svz_socket_t *xsock;
  int ocounter;

  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  ocounter = xsock->idle_counter;
  if (BOUNDP (counter))
    {
      ASSERT_EXACT (2, counter);
      xsock->idle_counter = gi_scm2int (counter);
    }
  return gi_integer2scm (ocounter);
#undef FUNC_NAME
}

static svz_server_t *
named_instance_or_smob (SCM server, const char *FUNC_NAME)
{
  char name[64];
  svz_server_t *rv = NULL;

  if (GI_GET_XREP_MAYBE (name, server))
    rv = svz_server_get (name);
  if (! rv)
    CHECK_SMOB_ARG (server, server, SCM_ARG1, "svz-server or string", rv);
  return rv;
}

SCM_DEFINE
(guile_server_listeners,
 "svz:server:listeners", 1, 0, 0,
 (SCM server),
 doc: /***********
Return a list of listening @code{#<svz-socket>} smobs to which the
given server instance @var{server} is currently bound, or an empty list
if there is no such binding yet.  */)
{
#define FUNC_NAME s_guile_server_listeners
  svz_server_t *xserver = named_instance_or_smob (server, FUNC_NAME);
  svz_array_t *listeners;
  size_t i;
  SCM list = SCM_EOL;

  /* Create a list of socket smobs for the server.  */
  if ((listeners = svz_server_listeners (xserver)) != NULL)
    {
      for (i = 0; i < svz_array_size (listeners); i++)
        list = scm_cons (socket_smob ((svz_socket_t *)
                                      svz_array_get (listeners, i)),
                         list);
      svz_array_destroy (listeners);
    }
  return scm_reverse (list);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_server_clients,
 "svz:server:clients", 1, 0, 0,
 (SCM server),
 doc: /***********
Return a list of @code{#<svz-socket>} client smobs associated with
the given server instance @var{server} in arbitrary order, or an
empty list if there is no such client.  */)
{
#define FUNC_NAME s_guile_server_clients
  svz_server_t *xserver = named_instance_or_smob (server, FUNC_NAME);
  svz_array_t *clients;
  svz_socket_t *sock;
  size_t i;
  SCM list = SCM_EOL;

  /* Create a list of socket smobs for the server.  */
  if ((clients = svz_server_clients (xserver)) != NULL)
    {
      svz_array_foreach (clients, sock, i)
        list = scm_cons (socket_smob (sock), list);
      svz_array_destroy (clients);
    }
  return list;
#undef FUNC_NAME
}

#if HAVE_GETRPCENT || HAVE_GETRPCBYNAME || HAVE_GETRPCBYNUMBER
static SCM
scm_return_rpcentry (struct rpcent *entry)
{
  return scm_vector
    (gi_list_3 (gi_string2scm (entry->r_name),
                scm_makfromstrs (-1, entry->r_aliases),
                gi_integer2scm (entry->r_number)));
}

SCM_DEFINE
(scm_getrpc,
 "getrpc", 0, 1, 0,
 (SCM arg),
 doc: /***********
Lookup a network rpc service @var{arg} (name or service number),
and return a network rpc service object.
If given no arguments, it behave like @code{getrpcent}.  */)
{
#define FUNC_NAME s_scm_getrpc
  struct rpcent *entry = NULL;

#if HAVE_GETRPCENT
  if (!BOUNDP (arg))
    {
      if ((entry = getrpcent ()) == NULL)
        return SCM_BOOL_F;
      return scm_return_rpcentry (entry);
    }
#endif /* HAVE_GETRPCENT */
#if HAVE_GETRPCBYNAME
  if (gi_stringp (arg))
    {
      char name[64];

      GI_GET_XREP (name, arg);
      entry = getrpcbyname (name);
    }
  else
#endif /* HAVE_GETRPCBYNAME */
#if HAVE_GETRPCBYNUMBER
    {
      ASSERT_EXACT (1, arg);
      entry = getrpcbynumber (gi_scm2int (arg));
    }
#endif /* #if HAVE_GETRPCBYNUMBER */

  if (!entry)
    scm_syserror_msg (FUNC_NAME, "no such rpc service ~A",
                      scm_cons (arg, SCM_EOL), errno);
  return scm_return_rpcentry (entry);
#undef FUNC_NAME
}
#endif /* HAVE_GETRPCENT || HAVE_GETRPCBYNAME || HAVE_GETRPCBYNUMBER */

#if !HAVE_DECL_SETRPCENT
extern void setrpcent (int);
#endif
#if !HAVE_DECL_ENDRPCENT
extern void endrpcent (void);
#endif

#if HAVE_SETRPCENT && HAVE_ENDRPCENT
SCM_DEFINE
(scm_setrpc,
 "setrpc", 0, 1, 0,
 (SCM stayopen),
 doc: /***********
Open and rewind the file @file{/etc/rpc}.
If the @var{stayopen} flag is non-zero, the net data base will not be
closed after each call to @code{getrpc}.  If @var{stayopen} is omitted,
this is equivalent to calling @code{endrpcent}.  Otherwise it is
equivalent to calling @code{setrpcent} with arg 1.  */)
{
#define FUNC_NAME s_scm_setrpc
  if (!BOUNDP (stayopen))
    endrpcent ();
  else
    setrpcent (!SCM_FALSEP (stayopen));
  return SCM_UNSPECIFIED;
#undef FUNC_NAME
}
#endif /* HAVE_SETRPCENT && HAVE_ENDRPCENT */

#if HAVE_PMAP_GETMAPS
SCM_DEFINE
(scm_portmap_list,
 "portmap-list", 0, 1, 0,
 (SCM address),
 doc: /***********
Return a list of the current RPC program-to-port mappings
on the host located at IP address @var{address}, which
defaults to the local machine's IP address.
Return an empty list if either there is no such list
available or an error occurred while fetching the list.  */)
{
#define FUNC_NAME s_scm_portmap_list
  struct sockaddr_in addr, raddr;
  struct pmaplist *map;
  char str[48];
  SCM list = SCM_EOL, mapping;

  memset (&addr, 0, sizeof (struct sockaddr_in));
#if HAVE_GET_MYADDRESS
  get_myaddress (&addr);
#else
  addr.sin_family = AF_INET;
  addr.sin_port = htons (PMAPPORT);
  addr.sin_addr.s_addr = htonl (INADDR_LOOPBACK);
#endif
  if (BOUNDP (address))
    {
      ASSERT_STRING (1, address);
      GI_GET_XREP (str, address);
      if (svz_inet_aton (str, &raddr) == -1)
        {
          guile_error ("%s: IP in dotted decimals expected", FUNC_NAME);
          return SCM_EOL;
        }
      addr.sin_addr.s_addr = raddr.sin_addr.s_addr;
    }

  if ((map = pmap_getmaps (&addr)) == NULL)
    return SCM_EOL;
  do
    {
      mapping = gi_list_3 (gi_integer2scm (map->pml_map.pm_vers),
                           gi_integer2scm (map->pml_map.pm_prot),
                           gi_integer2scm (map->pml_map.pm_port));
      mapping = scm_cons (gi_integer2scm (map->pml_map.pm_prog),
                          mapping);
      mapping = scm_vector (mapping);
      list = scm_cons (mapping ,list);
    }
  while ((map = map->pml_next) != NULL);
  return scm_reverse (list);
#undef FUNC_NAME
}
#endif /* HAVE_PMAP_GETMAPS */

static SCM
errnostring (void)
{
  return scm_strerror (gi_integer2scm (errno));
}

#if HAVE_PMAP_SET && HAVE_PMAP_UNSET
SCM_DEFINE
(scm_portmap,
 "portmap", 2, 2, 0,
 (SCM prognum, SCM versnum, SCM protocol, SCM port),
 doc: /***********
Establish a (portmap service) mapping
between the triple [@var{prognum},@var{versnum},@var{protocol}] and
@var{port} on the machine's portmap service.  The value of @var{protocol}
is most likely @code{IPPROTO_UDP} or @code{IPPROTO_TCP}.
If instead @var{protocol} and @var{port} are omitted, destroy
all mapping between the triple [@var{prognum},@var{versnum},*] and ports
on the machine's portmap service.  */)
{
#define FUNC_NAME s_scm_portmap
  ASSERT_EXACT (1, prognum);
  ASSERT_EXACT (2, prognum);

  if (!BOUNDP (protocol) && !BOUNDP (port))
    {
      if (!pmap_unset (gi_scm2int (prognum), gi_scm2int (versnum)))
        scm_syserror_msg (FUNC_NAME, "~A: pmap_unset ~A ~A",
                          gi_list_3 (errnostring (), prognum, versnum),
                          errno);
    }
  else
    {
      ASSERT_EXACT (3, protocol);
      ASSERT_EXACT (4, port);

      if (!pmap_set (gi_scm2int (prognum), gi_scm2int (versnum),
                     gi_scm2int (protocol), gi_scm2int (port)))
        scm_syserror_msg (FUNC_NAME, "~A: pmap_set ~A ~A ~A ~A",
                          gi_list_5 (errnostring (), prognum,
                                     versnum, protocol, port),
                          errno);
    }
  return SCM_UNSPECIFIED;
#undef FUNC_NAME
}
#endif /* HAVE_PMAP_SET && HAVE_PMAP_UNSET */

/* Validate @var{callback}; protect it from gc.  */
static void
validate_callback (SCM callback, const char *who)
{
  SCM_ASSERT_TYPE (SCM_PROCEDUREP (callback), callback,
                   SCM_ARG2, who, "procedure");
  /* TODO: Check arity.  */

  /* Protect callback from garbage collection meanwhile.  */
  gi_gc_protect (callback);
}

#define VALIDATE_CALLBACK(x)  validate_callback (x, FUNC_NAME)

/* Callback wrapper for coserver responses.  */
static int
guile_coserver_callback (char *res, void *closure)
{
  SCM callback = (SCM) closure;
  int ret = -1;

  /* If successfully done, run the given Guile procedure.  */
  if (res != NULL)
    {
      guile_call (callback, 1, gi_string2scm (res));
      ret = 0;
    }

  /* Pass the value to garbage collection again.  */
  gi_gc_unprotect (callback);
  return ret;
}

#define ENQ_COSERVER_REQUEST(req,coserver)      \
  svz_coserver_ ## coserver ## _invoke          \
  (req, guile_coserver_callback,                \
   (void *) callback)

SCM_DEFINE
(guile_coserver_dns,
 "svz:coserver:dns", 2, 0, 0,
 (SCM host, SCM callback),
 doc: /***********
Enqueue the @var{host} string argument into the internal
DNS coserver queue.  When the coserver responds, the procedure
@var{callback} is run as @code{(callback addr)}.  The @var{addr}
argument passed to the callback is a string representing the appropriate
IP address for the given hostname @var{host}.  */)
{
#define FUNC_NAME s_guile_coserver_dns
  char request[128];

  /* Check argument list first.  */
  ASSERT_STRING (1, host);
  VALIDATE_CALLBACK (callback);

  /* Convert hostname into C string.  */
  GI_GET_XREP (request, host);

  ENQ_COSERVER_REQUEST (request, dns);
  return SCM_UNSPECIFIED;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_coserver_rdns,
 "svz:coserver:reverse-dns", 2, 0, 0,
 (SCM addr, SCM callback),
 doc: /***********
Enqueue the given @var{addr} argument, which must be
an IP address in network byte order, into the internal reverse DNS coserver
queue.  When the coserver responds, the procedure @var{callback} is
run as @code{(callback host)} where @var{host} is the hostname of the
requested IP address @var{addr}.  */)
{
#define FUNC_NAME s_guile_coserver_rdns
  in_addr_t ip;
  svz_address_t *a;

  /* Check argument list first.  */
  ASSERT_EXACT (1, addr);
  VALIDATE_CALLBACK (callback);

  /* Convert IP address into C long value.  */
  ip = gi_scm2ulong (addr);
  a = svz_address_make (AF_INET, &ip);

  ENQ_COSERVER_REQUEST (a, rdns);
  svz_free (a);
  return SCM_UNSPECIFIED;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_coserver_ident,
 "svz:coserver:ident", 2, 0, 0,
 (SCM sock, SCM callback),
 doc: /***********
Enqueue the given @code{#<svz-socket>} @var{sock} into the
internal ident coserver queue.  When the coserver responds, it
runs the procedure @var{callback} as @code{(callback user)}, where
@var{user} is the corresponding username for the client connection
@var{sock}.  */)
{
#define FUNC_NAME s_guile_coserver_ident
  svz_socket_t *xsock;

  /* Check argument list first.  */
  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  VALIDATE_CALLBACK (callback);

  ENQ_COSERVER_REQUEST (xsock, ident);
  return SCM_UNSPECIFIED;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_find,
 "svz:sock:find", 1, 0, 0,
 (SCM ident),
 doc: /***********
Return the @code{#<svz-socket>} specified by @var{ident},
a pair of integers in the form @code{(identification . version)}.
If that socket no longer exists, return @code{#f}.  */)
{
#define FUNC_NAME s_guile_sock_find
  int version, id;
  svz_socket_t *sock;

  SCM_ASSERT_TYPE (SCM_PAIRP (ident) && gi_exactp (SCM_CAR (ident)) &&
                   gi_exactp (SCM_CDR (ident)), ident, SCM_ARG1,
                   FUNC_NAME, "pair of exact");
  id = gi_scm2int (SCM_CAR (ident));
  version = gi_scm2int (SCM_CDR (ident));
  if ((sock = svz_sock_find (id, version)) != NULL)
    return socket_smob (sock);
  return SCM_BOOL_F;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_ident,
 "svz:sock:ident", 1, 0, 0,
 (SCM sock),
 doc: /***********
Return a pair of numbers identifying the given
@code{#<svz-socket>} @var{sock}, which can be passed to
@code{svz:sock:find}.  This may be necessary when you are passing
a @code{#<svz-socket>} through coserver callback arguments in order to
verify that the passed @code{#<svz-socket>} is still valid when the
coserver callback runs.  */)
{
#define FUNC_NAME s_guile_sock_ident
  svz_socket_t *xsock;
  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  return scm_cons (gi_integer2scm (xsock->id), gi_integer2scm (xsock->version));
#undef FUNC_NAME
}

SCM_DEFINE
(guile_read_file,
 "svz:read-file", 2, 0, 0,
 (SCM port, SCM size),
 doc: /***********
Return either a binary smob containing a data block read
from the open input port @var{port} with a maximum number of @var{size}
bytes, or the end-of-file object if the underlying ports end has been
reached.  The size of the returned binary smob may be less than the
requested size @var{size} if it exceed the current size of the given port
@var{port}.  Throw an exception if an error occurred while
reading from the port.  */)
{
#define FUNC_NAME s_guile_read_file
  int fdes, len, ret;
  uint8_t *data;

  /* Check argument list.  */
  SCM_ASSERT_TYPE (SCM_NIMP (port) && SCM_FPORTP (port) &&
                   SCM_OPINFPORTP (port),
                   port, SCM_ARG1, FUNC_NAME, "open input port");
  ASSERT_EXACT (2, size);

  /* Get underlying file descriptor.  */
  fdes = gi_scm2int (scm_fileno (port));

  if ((len = gi_scm2int (size)) <= 0)
    SCM_OUT_OF_RANGE (SCM_ARG2, size);

  /* Allocate necessary data.  */
  data = gi_malloc (len, BDATA_WHAT);

  /* Read from file descriptor and evaluate return value.  */
  if ((ret = read (fdes, data, len)) < 0)
    {
      BFREE (DATA, data, len);
      scm_syserror_msg (FUNC_NAME, "~A: read ~A ~A",
                        gi_list_3 (errnostring (),
                                   gi_integer2scm (fdes),
                                   size),
                        errno);
    }
  else if (ret == 0)
    {
      BFREE (DATA, data, len);
      return SCM_EOF_VAL;
    }
  else if (ret != len)
    {
      data = gi_realloc (data, len, ret, BDATA_WHAT);
    }

  /* Finally return binary smob.  */
  return guile_garbage_to_bin (data, ret);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_send_oob,
 "svz:sock:send-oob", 2, 0, 0,
 (SCM sock, SCM oob),
 doc: /***********
Send byte @var{oob} as urgent (out-of-band) data through the
underlying TCP stream of TCP @var{sock}.
Return @code{#t} on successful completion and otherwise
(either it failed to send the byte or the passed socket is not a TCP
socket) @code{#f}.  */)
{
#define FUNC_NAME s_guile_sock_send_oob
  svz_socket_t *xsock;
  int ret = -1;

  /* Check the arguments.  */
  CHECK_SMOB_ARG (socket, sock, SCM_ARG1, "svz-socket", xsock);
  SCM_ASSERT_TYPE (gi_exactp (oob) || SCM_CHARP (oob),
                   oob, SCM_ARG2, FUNC_NAME, "char or exact");

  /* Send the oob byte through TCP sockets only.  */
  if (xsock->proto & SVZ_PROTO_TCP)
    {
      xsock->oob = (uint8_t)
        (SCM_CHARP (oob) ? SCM_CHAR (oob) :
         (uint8_t) gi_scm2int (oob));
      ret = svz_tcp_send_oob (xsock);
    }
  return ((ret < 0) ? SCM_BOOL_F : SCM_BOOL_T);
#undef FUNC_NAME
}

/* Initialize the API procedures supported by Guile.  */
void
guile_api_init (void)
{
#if HAVE_PMAP_SET && HAVE_PMAP_UNSET
  gi_define ("IPPROTO_UDP", gi_integer2scm (IPPROTO_UDP));
  gi_define ("IPPROTO_TCP", gi_integer2scm (IPPROTO_TCP));
#endif

  gi_define ("PROTO_TCP", gi_integer2scm (SVZ_PROTO_TCP));
  gi_define ("PROTO_UDP", gi_integer2scm (SVZ_PROTO_UDP));
  gi_define ("PROTO_ICMP", gi_integer2scm (SVZ_PROTO_ICMP));
  gi_define ("PROTO_RAW", gi_integer2scm (SVZ_PROTO_RAW));
  gi_define ("PROTO_PIPE", gi_integer2scm (SVZ_PROTO_PIPE));
  gi_define ("KICK_FLOOD", gi_integer2scm (0));
  gi_define ("KICK_QUEUE", gi_integer2scm (1));
}

/* Finalize the API procedures.  */
void
guile_api_finalize (void)
{
}

#endif /* ENABLE_GUILE_SERVER */
