/*
 * (GNU Serveez 0.2.2) libserveez.h
 *
 * Copyright (C) 2011-2012, 2013 Thien-Thi Nguyen
 * Copyright (C) 2000-2003  Stefan Jahn <stefan@lkcc.org>
 * Copyright (C)      2002  Andreas Rottmann <a.rottmann@gmx.at>
 * Copyright (C) 2000-2001  Raimund Jacob <raimi@lkcc.org>
 * Copyright (C)      1999  Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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

#ifndef __LIBSERVEEZ_H__
#define __LIBSERVEEZ_H__ 1

/* (svzconfig) */
/* Define to 1 if you have the <winsock2.h> header file.  */
/* #undef SVZ_HAVE_WINSOCK2_H */

/* Define to 1 if you have the <arpa/inet.h> header file.  */
#define SVZ_HAVE_ARPA_INET_H 1

/* Define to 1 if you have the <netinet/in.h> header file.  */
#define SVZ_HAVE_NETINET_IN_H 1

/* Define to 'int' if <winsock2.h> does not define 'HANDLE'.  */
#define svz_t_handle int

/* Define to 'int' if <winsock2.h> does not define 'SOCKET'.  */
#define svz_t_socket int

/* Make CygWin / MinGW32 use large FD sets.  */
/* #undef FD_SETSIZE */

/* Define for faster code generation.  */
/* #undef WIN32_LEAN_AND_MEAN */

/* Define if you are using Windows Socket-API (not CYGWIN).  */
/* #undef Win32_Winsock */


/* (defines) */

/* System headers: standard ones unconditional;
   the rest only ‘#ifdef SVZ_HAVE_HEADER_H’ .. ‘#endif’.  */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#ifdef SVZ_HAVE_WINSOCK2_H
#include <winsock2.h>
#endif

#ifdef SVZ_HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#ifdef SVZ_HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif


/* ‘__BEGIN_DECLS’ should be used at the beginning of your declarations,
   so that C++ compilers don't mangle their names.  Use ‘__END_DECLS’ at
   the end of C declarations.  */

#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS
# define __END_DECLS
#endif

/* ‘SERVEEZ_API’ is a macro prepended to all function and data definitions
   which should be exported or imported in the resulting dynamic link
   library in the Win32 port.  */

#if defined (__SERVEEZ_IMPORT__)
# define SERVEEZ_API __declspec (dllimport) extern
#elif defined (__SERVEEZ_EXPORT__) || defined (DLL_EXPORT)
# define SERVEEZ_API __declspec (dllexport) extern
#else
# define SERVEEZ_API extern
#endif


/* (address) */

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


/* (boot) */

/* Runtime parameters.  */
#define SVZ_RUNPARM_VERBOSITY    0
#define SVZ_RUNPARM_MAX_SOCKETS  1

__BEGIN_DECLS

SERVEEZ_API const char * const * svz_library_features (size_t *);
SERVEEZ_API void svz_boot (char const *);
SERVEEZ_API long svz_uptime (void);
SERVEEZ_API int svz_runparm (int, int);
SERVEEZ_API void svz_halt (void);

__END_DECLS

/**
 * Return the value of runtime parameter @var{nick}.
 */
#define SVZ_RUNPARM(nick)                       \
  svz_runparm (-1, SVZ_RUNPARM_ ## nick)

/**
 * Set the runtime paramater @var{nick}
 * to have value @var{val}, an integer.
 */
#define SVZ_RUNPARM_X(nick,val)                 \
  svz_runparm (SVZ_RUNPARM_ ## nick, (val))


/* (alloc) */

__BEGIN_DECLS

/* Function type definitions.  */
typedef void * (* svz_malloc_func_t) (size_t);
typedef void * (* svz_realloc_func_t) (void *, size_t);
typedef void (* svz_free_func_t) (void *);

/* Global allocator functions.  */
SERVEEZ_API void svz_set_mm_funcs (svz_malloc_func_t,
                                   svz_realloc_func_t,
                                   svz_free_func_t);

/* Internal allocator functions.  */
SERVEEZ_API void *svz_malloc (size_t);
SERVEEZ_API void *svz_calloc (size_t);
SERVEEZ_API void *svz_realloc (void *, size_t);
SERVEEZ_API void svz_free (void *);
SERVEEZ_API char *svz_strdup (const char *);


SERVEEZ_API void svz_get_curalloc (size_t *);

__END_DECLS


/* (array) */

typedef struct svz_array svz_array_t;

__BEGIN_DECLS

SERVEEZ_API svz_array_t *svz_array_create (size_t, svz_free_func_t);
SERVEEZ_API void svz_array_destroy (svz_array_t *);
SERVEEZ_API void *svz_array_get (svz_array_t *, size_t);
SERVEEZ_API void *svz_array_set (svz_array_t *, size_t, void *);
SERVEEZ_API void svz_array_add (svz_array_t *, void *);
SERVEEZ_API void *svz_array_del (svz_array_t *, size_t);
SERVEEZ_API size_t svz_array_size (svz_array_t *);

__END_DECLS

/**
 * Expand into a @code{for}-statement header, for iterating over
 * @var{array}.  On each cycle, @var{value} is assigned to successive
 * elements of @var{array}, and @var{i} the element's position.
 */
#define svz_array_foreach(array, value, i)                      \
  for ((i) = 0, (value) = svz_array_get ((array), 0);           \
       (array) && (i) < svz_array_size (array);                 \
       ++(i), (value) = svz_array_get ((array), (i)))


/* (hash) */

typedef struct svz_hash_entry svz_hash_entry_t;
typedef struct svz_hash_bucket svz_hash_bucket_t;
typedef struct svz_hash svz_hash_t;

typedef void (svz_hash_do_t) (void *, void *, void *);

__BEGIN_DECLS

/*
 * Basic hash table functions.
 */
SERVEEZ_API svz_hash_t *svz_hash_create (size_t, svz_free_func_t);
SERVEEZ_API svz_hash_t *
svz_hash_configure (svz_hash_t *hash,
                    size_t (* keylen) (const char *),
                    unsigned long (* code) (const char *),
                    int (* equals) (const char *, const char *));
SERVEEZ_API void svz_hash_destroy (svz_hash_t *);
SERVEEZ_API void *svz_hash_delete (svz_hash_t *, const char *);
SERVEEZ_API void *svz_hash_put (svz_hash_t *, const char *, void *);
SERVEEZ_API void *svz_hash_get (const svz_hash_t *, const char *);
SERVEEZ_API void svz_hash_foreach (svz_hash_do_t *, svz_hash_t *, void *);
SERVEEZ_API size_t svz_hash_size (const svz_hash_t *);
SERVEEZ_API char *svz_hash_contains (const svz_hash_t *, void *);
SERVEEZ_API int svz_hash_exists (const svz_hash_t *, char *);

__END_DECLS


/* (util) */

/*
 * level of server's verbosity:
 * 0 - only fatal error messages
 * 1 - error messages
 * 2 - warnings
 * 3 - informational messages
 * 4 - debugging output
 * levels always imply numerically lesser levels
 */
#define SVZ_LOG_FATAL     0
#define SVZ_LOG_ERROR     1
#define SVZ_LOG_WARNING   2
#define SVZ_LOG_NOTICE    3
#define SVZ_LOG_DEBUG     4

__BEGIN_DECLS


SERVEEZ_API void svz_log (int, const char *, ...);
SERVEEZ_API void svz_log_setfile (FILE *);
SERVEEZ_API int svz_hexdump (FILE *, char *, int, char *, int, int);
SERVEEZ_API char *svz_itoa (unsigned int);
SERVEEZ_API unsigned int svz_atoi (char *);
SERVEEZ_API char *svz_getcwd (void);
SERVEEZ_API int svz_openfiles (int);
SERVEEZ_API char *svz_time (long);
SERVEEZ_API char *svz_tolower (char *);
SERVEEZ_API char *svz_sys_version (void);

SERVEEZ_API int svz_socket_unavailable_error_p (void);

SERVEEZ_API const char *svz_sys_strerror (void);
SERVEEZ_API void svz_log_sys_error (char const *, ...);
SERVEEZ_API void svz_log_net_error (char const *, ...);


SERVEEZ_API int svz_mingw_at_least_nt4_p (void);

__END_DECLS


/* (socket) */

/* Do not write more than this many bytes to a socket at once.  */
#define SVZ_SOCK_MAX_WRITE    1024


#define SVZ_SOFLG_INIT        0x00000000 /* Value for initializing.  */
#define SVZ_SOFLG_INBUF       0x00000001 /* Outbuf is allocated.  */
#define SVZ_SOFLG_OUTBUF      0x00000002 /* Inbuf is allocated.  */
#define SVZ_SOFLG_CONNECTED   0x00000004 /* Socket is connected.  */
#define SVZ_SOFLG_LISTENING   0x00000008 /* Socket is listening.  */
#define SVZ_SOFLG_KILLED      0x00000010 /* Socket will be shut down soon.  */
#define SVZ_SOFLG_NOFLOOD     0x00000020 /* Flood protection off.  */
#define SVZ_SOFLG_INITED      0x00000040 /* Socket was initialized.  */
#define SVZ_SOFLG_ENQUEUED    0x00000080 /* Socket is on socket queue.  */
#define SVZ_SOFLG_RECV_PIPE   0x00000100 /* Receiving pipe is active.  */
#define SVZ_SOFLG_SEND_PIPE   0x00000200 /* Sending pipe is active.  */
#define SVZ_SOFLG_FILE        0x00000400 /* Socket is no socket, but file.  */
#define SVZ_SOFLG_COSERVER    0x00000800 /* Socket is a coserver */
#define SVZ_SOFLG_SOCK        0x00001000 /* Socket is a plain socket.  */
/* Socket is no socket, but pipe.  */
#define SVZ_SOFLG_PIPE \
  ( SVZ_SOFLG_RECV_PIPE | \
    SVZ_SOFLG_SEND_PIPE )
#define SVZ_SOFLG_CONNECTING  0x00002000 /* Socket is still connecting */
#define SVZ_SOFLG_PRIORITY    0x00004000 /* Enqueue socket preferred.  */
#define SVZ_SOFLG_FIXED       0x00008000 /* Dedicated UDP connection.  */
#define SVZ_SOFLG_FINAL_WRITE 0x00010000 /* Disconnect as soon as send
                                            queue is empty.  */
#define SVZ_SOFLG_READING     0x00020000 /* Pending read operation.  */
#define SVZ_SOFLG_WRITING     0x00040000 /* Pending write operation.  */
#define SVZ_SOFLG_FLUSH       0x00080000 /* Flush receive and send queue.  */
#define SVZ_SOFLG_NOSHUTDOWN  0x00100000 /* Disable shutdown.  */
#define SVZ_SOFLG_NOOVERFLOW  0x00200000 /* Disable receive buffer overflow.  */

typedef struct svz_socket svz_socket_t;

struct svz_socket
{
  svz_socket_t *next;           /* Next socket in chain.  */
  svz_socket_t *prev;           /* Previous socket in chain.  */

  int id;                       /* Unique ID for this socket.  */
  int version;                  /* Socket version */
  int parent_id;                /* A sockets parent ID.  */
  int parent_version;           /* A sockets parent version.  */
  int referrer_id;              /* Referring socket ID.  */
  int referrer_version;         /* Referring socket version.  */

  int proto;                    /* Server/Protocol flag.  */
  int flags;                    /* One of the SVZ_SOFLG_* flags above.  */
  int userflags;                /* Can be used for protocol specific flags.  */
  svz_t_socket sock_desc;       /* Socket descriptor.  */
  int file_desc;                /* Used for files descriptors.  */
  svz_t_handle pipe_desc[2];    /* Used for the pipes and coservers.  */
  svz_t_handle pid;             /* Process id.  */

#ifdef __MINGW32__
  LPOVERLAPPED overlap[2];      /* Overlap info for WinNT.  */
  int recv_pending;             /* Number of pending read bytes.  */
  int send_pending;             /* Number of pending write bytes.  */
#endif /* not __MINGW32__ */

  char *recv_pipe;              /* File of the receive pipe.  */
  char *send_pipe;              /* File of the send pipe.  */

  char *boundary;               /* Packet boundary.  */
  int boundary_size;            /* Packet boundary length */

  /* The following items always MUST be in network byte order.  */
  in_port_t remote_port;        /* Port number of remote end.  */
  svz_address_t *remote_addr;   /* IP address of remote end.  */
  in_port_t local_port;         /* Port number of local end.  */
  svz_address_t *local_addr;    /* IP address of local end.  */

  char *send_buffer;            /* Buffer for outbound data.  */
  char *recv_buffer;            /* Buffer for inbound data.  */
  int send_buffer_size;         /* Size of SEND_BUFFER.  */
  int recv_buffer_size;         /* Size of RECV_BUFFER.  */
  int send_buffer_fill;         /* Valid bytes in SEND_BUFFER.  */
  int recv_buffer_fill;         /* Valid bytes in RECV_BUFFER.  */

  uint16_t sequence;            /* Currently received sequence.  */
  uint16_t send_seq;            /* Send stream sequence number.  */
  uint16_t recv_seq;            /* Receive stream sequence number.  */
  uint8_t itype;                /* ICMP message type.  */

  /*
   * READ_SOCKET gets called whenever data is available on the socket.
   * Normally, this is set to a default function which reads all available
   * data from the socket and feeds it to CHECK_REQUEST, but specific
   * sockets may need another policy.
   */
  int (* read_socket) (svz_socket_t *sock);

  /*
   * READ_SOCKET_OOB is run when urgent data has been detected on the
   * socket.  By default it reads a single byte, stores it in OOB and passes
   * it to the CHECK_REQUEST_OOB callback.
   */
  int (* read_socket_oob) (svz_socket_t *sock);

  /*
   * WRITE_SOCKET is called when data is is valid in the output buffer
   * and the socket gets available for writing.  Normally, this simply
   * writes as much data as possible to the socket and removes it from
   * the buffer.
   */
  int (* write_socket) (svz_socket_t *sock);

  /*
   * DISCONNECTED_SOCKET gets called whenever the socket is lost for
   * some external reason.
   */
  int (* disconnected_socket) (svz_socket_t *sock);

  /*
   * CONNECTED_SOCKET gets called whenever the socket is finally
   * connected.
   */
  int (* connected_socket) (svz_socket_t *sock);

  /*
   * KICKED_SOCKET gets called whenever the socket gets closed by us.
   */
  int (* kicked_socket) (svz_socket_t *sock, int reason);

  /*
   * CHECK_REQUEST gets called whenever data was read from the socket.
   * Its purpose is to check whether a complete request was read, and
   * if it was, it should be handled and removed from the input buffer.
   */
  int (* check_request) (svz_socket_t *sock);

  /*
   * CHECK_REQUEST_OOB is called when urgent data has been read from the
   * socket.  The received byte is stored in OOB.
   */
  int (* check_request_oob) (svz_socket_t *sock);

  /*
   * HANDLE_REQUEST gets called when the CHECK_REQUEST got
   * a valid packet.
   */
  int (* handle_request) (svz_socket_t *sock, char *request, int len);

  /*
   * CHILD_DIED is called when the PID stored in the socket structure
   * equals the one signaled by the internal signal handler.
   */
  int (* child_died) (svz_socket_t *sock);

  /*
   * Called if TRIGGER_COND returned non-zero.
   */
  int (* trigger_func) (svz_socket_t *sock);

  /*
   * TRIGGER_COND is called once every server loop.  If it returns
   * non-zero TRIGGER_FUNC is run.
   */
  int (* trigger_cond) (svz_socket_t *sock);

  /*
   * IDLE_FUNC gets called from the periodic task scheduler.  Whenever
   * IDLE_COUNTER (see below) is non-zero, it is decremented and
   * IDLE_FUNC gets called when it drops to zero.  IDLE_FUNC can reset
   * IDLE_COUNTER to some value and thus can re-schedule itself for a
   * later task.
   */
  int (* idle_func) (svz_socket_t * sock);

  int idle_counter;             /* Counter for calls to IDLE_FUNC.  */

  long last_send;               /* Timestamp of last send to socket.  */
  long last_recv;               /* Timestamp of last receive from socket */

  /* Note: These two are used only if flood protection is enabled.  */
  int flood_points;             /* Accumulated flood points.  */
  int flood_limit;              /* Limit of the above before kicking.  */

  /* Out-of-band data for TCP protocol.  This byte is used for both,
     receiving and sending.  */
  uint8_t oob;

  /* Set to non-zero @code{time} value if the the socket is temporarily
     unavailable (EAGAIN).  This is why we use O_NONBLOCK socket descriptors.  */
  int unavailable;

  /* Miscellaneous field.  Listener keeps array of server instances here.
     This array is NULL terminated.  */
  void *data;

  /* When the final protocol detection has been done this should get the
     actual configuration hash.  */
  void *cfg;

  /* Port configuration of a parent (listener).  */
  void *port;

  /* Codec data pointers.  Yet another extension.  */
  void *recv_codec;
  void *send_codec;
};

__BEGIN_DECLS

typedef void (svz_sock_prefree_fn) (const svz_socket_t *);
SERVEEZ_API void svz_sock_prefree (int addsub, svz_sock_prefree_fn fn);
SERVEEZ_API int svz_sock_nconnections (void);
SERVEEZ_API int svz_sock_write (svz_socket_t *, char *, int);
SERVEEZ_API int svz_sock_printf (svz_socket_t *, const char *, ...);
SERVEEZ_API int svz_sock_resize_buffers (svz_socket_t *, int, int);
SERVEEZ_API int svz_sock_check_request (svz_socket_t *);
SERVEEZ_API int svz_wait_if_unavailable (svz_socket_t *, unsigned int);
SERVEEZ_API void svz_sock_reduce_recv (svz_socket_t *, int);
SERVEEZ_API void svz_sock_reduce_send (svz_socket_t *, int);

__END_DECLS


/* (core) */

/* protocol definitions */
#define SVZ_PROTO_TCP   0x00000001 /* tcp  - bidirectional, reliable */
#define SVZ_PROTO_UDP   0x00000002 /* udp  - multidirectional, unreliable */
#define SVZ_PROTO_PIPE  0x00000004 /* pipe - unidirectional, reliable */
#define SVZ_PROTO_ICMP  0x00000008 /* icmp - multidirectional, unreliable */
#define SVZ_PROTO_RAW   0x00000010 /* raw  - multidirectional, unreliable */

/* Silence the "declared inside parameter list" warning.  */
struct stat;

__BEGIN_DECLS

SERVEEZ_API int svz_fd_cloexec (int);
SERVEEZ_API int svz_tcp_cork (svz_t_socket, int);
SERVEEZ_API int svz_tcp_nodelay (svz_t_socket, int, int *);
SERVEEZ_API int svz_closesocket (svz_t_socket);
SERVEEZ_API char *svz_inet_ntoa (in_addr_t);
SERVEEZ_API int svz_inet_aton (char *, struct sockaddr_in *);
SERVEEZ_API int svz_sendfile (int, int, off_t *, size_t);
SERVEEZ_API int svz_open (const char *, int, mode_t);
SERVEEZ_API int svz_close (int);
SERVEEZ_API int svz_fstat (int, struct stat *);
SERVEEZ_API FILE *svz_fopen (const char *, const char *);
SERVEEZ_API int svz_fclose (FILE *);


__END_DECLS


/* (pipe-socket) */

#define SVZ_READ   0                    /* read pipe index */
#define SVZ_WRITE  1                    /* write pipe index */

/*
 * Definition of a named pipe.
 */
typedef struct svz_pipe
{
  char *name;        /* name of named pipe */
  mode_t perm;       /* user and group permissions */
  char *user;        /* user name */
  uid_t uid;         /* user id (calculated from user name) */
  gid_t pgid;        /* primary group id */
  char *group;       /* group name */
  gid_t gid;         /* group id (calculated from group name) */
}
svz_pipe_t;

__BEGIN_DECLS

SERVEEZ_API svz_socket_t *svz_pipe_create (svz_t_handle,
                                           svz_t_handle);
SERVEEZ_API int svz_pipe_create_pair (svz_t_handle pipe_desc[2]);
SERVEEZ_API svz_socket_t *svz_pipe_connect (svz_pipe_t *,
                                            svz_pipe_t *);

SERVEEZ_API void svz_invalidate_handle (svz_t_handle *);
SERVEEZ_API int svz_invalid_handle_p (svz_t_handle);
SERVEEZ_API int svz_closehandle (svz_t_handle);

__END_DECLS


/* (portcfg) */

/* Port configuration items.  */
#define SVZ_PORTCFG_ANY   "any"
#define SVZ_PORTCFG_NOIP  "*"


/* Return values for port configuration comparisons.  */
#define SVZ_PORTCFG_NOMATCH  0x0001
#define SVZ_PORTCFG_EQUAL    0x0002
#define SVZ_PORTCFG_MATCH    0x0004
#define SVZ_PORTCFG_CONFLICT 0x0008

/*
 * Definition of a single port configuration reflecting either a network
 * (TCP, UDP, ICMP or RAW) or filesystem configuration (PIPE).
 */
typedef struct svz_portcfg
{
  /* the symbolic name of this port configuration */
  char *name;

  /* one of the PROTO_ flags defined in <core.h> */
  int proto;

  /* one of PORTCFG_FLAG_ flags */
  int flags;

  /* unified structure for each type of port */
  union protocol_t
  {
    /* tcp port */
    struct tcp_t
    {
      in_port_t port;          /* TCP/IP port */
      char *ipaddr;            /* dotted decimal or "*" for any address */
      struct sockaddr_in addr; /* converted from the above 2 values */
      char *device;            /* network device */
      int backlog;             /* backlog argument for ‘listen’ */
    } tcp;

    /* udp port */
    struct udp_t
    {
      in_port_t port;          /* UDP port */
      char *ipaddr;            /* dotted decimal or "*" */
      struct sockaddr_in addr; /* converted from the above 2 values */
      char *device;            /* network device */
    } udp;

    /* icmp port */
    struct icmp_t
    {
      char *ipaddr;            /* dotted decimal or "*" */
      struct sockaddr_in addr; /* converted from the above value */
      char *device;            /* network device */
      uint8_t type;            /* message type */
    } icmp;

    /* raw ip port */
    struct raw_t
    {
      char *ipaddr;            /* dotted decimal or "*" */
      struct sockaddr_in addr; /* converted from the above value */
      char *device;            /* network device */
    } raw;

    /* pipe port */
    struct pipe_t
    {
      svz_pipe_t recv;         /* pipe for sending data into serveez */
      svz_pipe_t send;         /* pipe serveez sends responses out on */
    } pipe;
  }
  protocol;

  /* maximum number of bytes for protocol identification */
  int detection_fill;

  /* maximum seconds for protocol identification */
  int detection_wait;

  /* initial buffer sizes */
  int send_buffer_size;
  int recv_buffer_size;

  /* allowed number of connects per second (hammer protection) */
  int connect_freq;

  /* remembers connect frequency for each ip */
  svz_hash_t *accepted;

  /* denied and allowed access list (ip based) */
  svz_array_t *deny;
  svz_array_t *allow;
}
svz_portcfg_t;

/*
 * Accessor definitions for each type of protocol.
 */
#define SVZ_CFG_TCP(portcfg,x)     (portcfg->protocol.tcp.x)
#define SVZ_CFG_UDP(portcfg,x)     (portcfg->protocol.udp.x)
#define SVZ_CFG_ICMP(portcfg,x)    (portcfg->protocol.icmp.x)
#define SVZ_CFG_RAW(portcfg,x)     (portcfg->protocol.raw.x)
#define SVZ_CFG_PIPE(portcfg,x)    (portcfg->protocol.pipe.x)

__BEGIN_DECLS

SERVEEZ_API struct sockaddr_in *svz_portcfg_addr (svz_portcfg_t *);
SERVEEZ_API char *svz_portcfg_ipaddr (svz_portcfg_t *);
SERVEEZ_API char *svz_portcfg_device (svz_portcfg_t *);
SERVEEZ_API in_port_t svz_portcfg_port (svz_portcfg_t *);

SERVEEZ_API svz_portcfg_t *svz_portcfg_create (void);
SERVEEZ_API int svz_portcfg_equal (svz_portcfg_t *, svz_portcfg_t *);
SERVEEZ_API svz_portcfg_t *svz_portcfg_add (char *, svz_portcfg_t *);
SERVEEZ_API svz_portcfg_t *svz_portcfg_get (char *);
SERVEEZ_API void svz_portcfg_destroy (svz_portcfg_t *);
SERVEEZ_API int svz_portcfg_mkaddr (svz_portcfg_t *);
SERVEEZ_API svz_portcfg_t *svz_portcfg_dup (svz_portcfg_t *);

__END_DECLS


/* (cfg) */

/*
 * Each server can have a an array of key-value-pairs specific for it.
 * Use macros at end of this file for setting up these.
 */
typedef struct svz_key_value_pair
{
  int type;        /* data type (string, integer, etc.) */
  char *name;      /* variable name (symbol) */
  int defaultable; /* set if this item is defaultable */
  void *address;   /* memory address of the variable */
}
svz_key_value_pair_t;

/*
 * This structure defines callbacks for the (internal) server configuration
 * function.  Each of these have the following arguments:
 * instance: might be the name of the instance to configure
 * arg:     an optional argument (e.g. scheme cell), supplied by user
 * name:    the symbolic name of the configuration item
 * target:  target address of the configuration item
 * hasdef:  is there a default value
 * def:     the default value for this configuration item
 *
 * The 'before' and 'after' callbacks are called just before and after
 * other options are set.  The user is supposed to emit error messages since
 * the library cannot guess what went wrong.
 * Both callbacks have to return @code{SVZ_ITEM_OK} to allow the configure
 * function to complete successfully.  No other callback is invoked when
 * the 'before' callback fails.
 *
 * Default values and the @var{hasdef} flag are passed to callbacks for
 * no sane reason.  You do not need to care about them if you set
 * appropriate return values.  If you use them, however, everything that
 * is a pointer needs to be copied.
 */

#define SVZ_ITEM_OK             0 /* okay, value set */
#define SVZ_ITEM_DEFAULT        1 /* use default, be silent if missing */
#define SVZ_ITEM_DEFAULT_ERRMSG 2 /* use default, croak if missing */
#define SVZ_ITEM_FAILED         3 /* error, error messages already emitted */
#define SVZ_ITEM_FAILED_ERRMSG  4 /* error, please report error */

typedef struct
{
  int (* before)   (char *instance, void *arg);
  int (* integer)  (char *instance, void *arg, char *name,
                    int *target, int hasdef, int def);
  int (* boolean)  (char *instance, void *arg, char *name,
                    int *target, int hasdef, int def);
  int (* intarray) (char *instance, void *arg, char *name,
                    svz_array_t **target, int hasdef, svz_array_t *def);
  int (* string)   (char *instance, void *arg, char *name,
                    char **target, int hasdef, char *def);
  int (* strarray) (char *instance, void *arg, char *name,
                    svz_array_t **target, int hasdef, svz_array_t *def);
  int (* hash)     (char *instance, void *arg, char *name,
                    svz_hash_t **target, int hasdef, svz_hash_t *def);
  int (* portcfg)  (char *instance, void *arg, char *name,
                    svz_portcfg_t **target, int hasdef, svz_portcfg_t *def);
  int (* after)    (char *instance, void *arg);
}
svz_config_accessor_t;

typedef struct
{
  /* description of the configuration prototype */
  char *description;
  /* start of example structure */
  void *start;
  /* size of the above structure */
  int size;
  /* array of key-value-pairs of configuration items */
  svz_key_value_pair_t *items;
}
svz_config_prototype_t;

/* Constants for the @var{defaultable} argument.  */
#define SVZ_ITEM_DEFAULTABLE     1
#define SVZ_ITEM_NOTDEFAULTABLE  0

/* Configuration item identifiers.  */
#define SVZ_ITEM_END      0
#define SVZ_ITEM_INT      1
#define SVZ_ITEM_INTARRAY 2
#define SVZ_ITEM_STR      3
#define SVZ_ITEM_STRARRAY 4
#define SVZ_ITEM_HASH     5
#define SVZ_ITEM_PORTCFG  6
#define SVZ_ITEM_BOOL     7

/**
 * Expand to a data structure that properly associates the example
 * configuration @var{config} with the name @var{description} and its
 * configuration items @var{prototypes}, for use within a server type
 * definition.
 */
#define SVZ_CONFIG_DEFINE(description, config, prototypes) \
  { description, &(config), sizeof (config), (prototypes) }

/*
 * Returns a text representation of the given configuration item
 * identifier @var{item}.
 */
#define SVZ_ITEM_TEXT(item)                                  \
  ((item) == SVZ_ITEM_INT) ? "integer" :                     \
  ((item) == SVZ_ITEM_INTARRAY) ? "integer array" :          \
  ((item) == SVZ_ITEM_STR) ? "string" :                      \
  ((item) == SVZ_ITEM_STRARRAY) ? "string array" :           \
  ((item) == SVZ_ITEM_HASH) ? "hash table" :                 \
  ((item) == SVZ_ITEM_BOOL) ? "boolean" :                    \
  ((item) == SVZ_ITEM_PORTCFG) ? "port configuration" : NULL

/**
 * Register a simple integer.  C-type: @code{int}.  The given @var{name}
 * specifies the symbolic name of the integer and @var{item} the integer
 * itself (not its address).  The @var{defaultable} argument can be either
 * @code{SVZ_ITEM_DEFAULTABLE} or @code{SVZ_ITEM_NOTDEFAULTABLE}.
 */
#define SVZ_REGISTER_INT(name, item, defaultable) \
  { SVZ_ITEM_INT, (name), (defaultable), &(item) }

/**
 * Register an array of integers.  C-type: @code{svz_array_t *}.
 */
#define SVZ_REGISTER_INTARRAY(name, item, defaultable) \
  { SVZ_ITEM_INTARRAY, (name), (defaultable), &(item) }

/**
 * Register a boolean value.  C-type: @code{int}.
 */
#define SVZ_REGISTER_BOOL(name, item, defaultable) \
  { SVZ_ITEM_BOOL, (name), (defaultable), &(item) }

/**
 * Register a simple character string.  C-type: @code{char *}.
 */
#define SVZ_REGISTER_STR(name, item, defaultable) \
  { SVZ_ITEM_STR, (name), (defaultable), &(item) }

/**
 * Register a string array.  C-type: @code{svz_array_t *}.
 */
#define SVZ_REGISTER_STRARRAY(name, item, defaultable) \
  { SVZ_ITEM_STRARRAY, (name), (defaultable), &(item) }

/**
 * Register a hash table associating strings with strings only.  C-type:
 * @code{svz_hash_t *}.
 */
#define SVZ_REGISTER_HASH(name, item, defaultable) \
  { SVZ_ITEM_HASH, (name), (defaultable), &(item) }

/**
 * Register a port configuration.  C-type: @code{svz_portcfg_t *}.
 */
#define SVZ_REGISTER_PORTCFG(name, item, defaultable) \
  { SVZ_ITEM_PORTCFG, (name), (defaultable), &(item) }

/**
 * Indicate the end of the list of configuration items.  It is
 * the only mandatory item you need to specify in an example server type
 * configuration.
 */
#define SVZ_REGISTER_END() \
  { SVZ_ITEM_END, NULL, SVZ_ITEM_DEFAULTABLE, NULL }

#define SVZ_INTARRAY  0
#define SVZ_STRARRAY  1
#define SVZ_STRHASH   2

__BEGIN_DECLS

SERVEEZ_API void svz_config_free (svz_config_prototype_t *, void *);
SERVEEZ_API int svz_config_type_instantiate (char *, char *,
                                             char *, void *,
                                             svz_config_accessor_t *,
                                             size_t, char *);
SERVEEZ_API void *svz_collect (int, size_t, void *);

__END_DECLS

#define __SVZ_COLLECT(nick,ctype,cvar)                                  \
  svz_collect (SVZ_ ## nick, sizeof (cvar) / sizeof (ctype), cvar)

/**
 * Return an integer array @code{svz_array_t *}
 * created from @code{int @var{cvar}[]}.
 */
#define SVZ_COLLECT_INTARRAY(cvar)  __SVZ_COLLECT (INTARRAY, int, cvar)

/**
 * Return a string array @code{svz_array_t *}
 * created from @code{char *@var{cvar}[]}.
 */
#define SVZ_COLLECT_STRARRAY(cvar)  __SVZ_COLLECT (STRARRAY, char *, cvar)

/**
 * Return a string hash @code{svz_hash_t *}
 * created from @code{char *@var{cvar}[]}.
 */
#define SVZ_COLLECT_STRHASH(cvar)  __SVZ_COLLECT (STRHASH, char *, cvar)


/* (server) */

typedef struct svz_servertype svz_servertype_t;
typedef struct svz_server svz_server_t;

/*
 * Each server instance gets such a structure.
 */
struct svz_server
{
  /* one of the PROTO_ flags defined in <core.h> */
  int proto;
  /* variable name in configuration language, used to identify it */
  char *name;
  /* server description */
  char *description;
  /* configuration structure for this instance */
  void *cfg;
  /* pointer to this server instances server type */
  svz_servertype_t *type;
  /* arbitrary data field */
  void *data;

  /* init of instance */
  int (* init) (svz_server_t *);
  /* protocol detection */
  int (* detect_proto) (svz_server_t *, svz_socket_t *);
  /* what to do if detected */
  int (* connect_socket) (svz_server_t *, svz_socket_t *);
  /* finalize this instance */
  int (* finalize) (svz_server_t *);
  /* return client info */
  char * (* info_client) (svz_server_t *, svz_socket_t *);
  /* return server info */
  char * (* info_server) (svz_server_t *);
  /* server timer */
  int (* notify) (svz_server_t *);
  /* server reset callback */
  int (* reset) (svz_server_t *);
  /* packet processing */
  int (* handle_request) (svz_socket_t *, char *, int);
};

/*
 * Every type (class) of server is completely defined by the following
 * structure.
 */
struct svz_servertype
{
  /* full descriptive name */
  char *description;
  /* variable prefix (short name) as used in configuration */
  char *prefix;

  /* run once per server definition */
  int (* global_init) (svz_servertype_t *);
  /* per server instance callback */
  int (* init) (svz_server_t *);
  /* protocol detection routine */
  int (* detect_proto) (svz_server_t *, svz_socket_t *);
  /* for accepting a client (tcp or pipe only) */
  int (* connect_socket) (svz_server_t *, svz_socket_t *);
  /* per instance */
  int (* finalize) (svz_server_t *);
  /* per server definition */
  int (* global_finalize) (svz_servertype_t *);
  /* return client info */
  char * (* info_client) (svz_server_t *, svz_socket_t *);
  /* return server info */
  char * (* info_server) (svz_server_t *);
  /* server timer */
  int (* notify) (svz_server_t *);
  /* server reset */
  int (* reset) (svz_server_t *);
  /* packet processing */
  int (* handle_request) (svz_socket_t *, char *, int);

  /* configuration prototype */
  svz_config_prototype_t config_prototype;
};


typedef int (svz_servertype_do_t) (const svz_servertype_t *, void *);
typedef void (svz_server_do_t) (svz_server_t *, void *);

__BEGIN_DECLS

SERVEEZ_API int svz_foreach_servertype (svz_servertype_do_t *, void *);
SERVEEZ_API void svz_foreach_server (svz_server_do_t *, void *);
SERVEEZ_API svz_server_t *svz_server_get (char *);
SERVEEZ_API svz_server_t *svz_server_find (void *);
SERVEEZ_API svz_array_t *svz_server_clients (svz_server_t *);
SERVEEZ_API int svz_updn_all_servers (int);

SERVEEZ_API void svz_servertype_add (svz_servertype_t *);
SERVEEZ_API svz_servertype_t *svz_servertype_get (char *, int);
SERVEEZ_API svz_servertype_t *svz_servertype_find (svz_server_t *);

__END_DECLS


/* (binding) */


__BEGIN_DECLS

SERVEEZ_API int svz_server_bind (svz_server_t *, svz_portcfg_t *);
SERVEEZ_API svz_array_t *svz_server_portcfgs (svz_server_t *);
SERVEEZ_API svz_array_t *svz_server_listeners (svz_server_t *);
SERVEEZ_API svz_array_t *svz_sock_servers (svz_socket_t *);
SERVEEZ_API int svz_binding_contains_server (svz_socket_t *,
                                             svz_server_t *);
SERVEEZ_API size_t svz_pp_server_bindings (char *, size_t, svz_server_t *);

__END_DECLS


/* (tcp-socket) */

__BEGIN_DECLS

SERVEEZ_API svz_socket_t *svz_tcp_connect (svz_address_t *, in_port_t);
SERVEEZ_API int svz_tcp_read_socket (svz_socket_t *);
SERVEEZ_API int svz_tcp_send_oob (svz_socket_t *);

__END_DECLS


/* (udp-socket) */

/* The maximum size of a UDP packet.  */
#define SVZ_UDP_MSG_SIZE  (64 * 1024)

/* Space for 4 messages.  */
#define SVZ_UDP_BUF_SIZE  (4 * (SVZ_UDP_MSG_SIZE + 24))

__BEGIN_DECLS

SERVEEZ_API svz_socket_t *svz_udp_connect (svz_address_t *, in_port_t);
SERVEEZ_API int svz_udp_write (svz_socket_t *, char *, int);

__END_DECLS


/* (icmp-socket) */

/* Serveez-ICMP types and sub-codes.  */
#define SVZ_ICMP_SERVEEZ        42
#define SVZ_ICMP_SERVEEZ_DATA    0
#define SVZ_ICMP_SERVEEZ_REQ     1
#define SVZ_ICMP_SERVEEZ_ACK     2
#define SVZ_ICMP_SERVEEZ_CLOSE   3
#define SVZ_ICMP_SERVEEZ_CONNECT 4

__BEGIN_DECLS

SERVEEZ_API svz_socket_t *svz_icmp_connect (svz_address_t *, in_port_t, uint8_t);
SERVEEZ_API int svz_icmp_send_control (svz_socket_t *, uint8_t);
SERVEEZ_API int svz_icmp_write (svz_socket_t *, char *, int);

__END_DECLS


/* (server-core) */



typedef int (svz_socket_do_t) (svz_socket_t *, void *);

__BEGIN_DECLS

SERVEEZ_API int svz_foreach_socket (svz_socket_do_t *, void *);
SERVEEZ_API svz_socket_t *svz_sock_find (int, int);
SERVEEZ_API int svz_sock_schedule_for_shutdown (svz_socket_t *);
SERVEEZ_API int svz_sock_enqueue (svz_socket_t *);
SERVEEZ_API void svz_sock_setparent (svz_socket_t *, svz_socket_t *);
SERVEEZ_API svz_socket_t *svz_sock_getparent (svz_socket_t *);
SERVEEZ_API void svz_sock_setreferrer (svz_socket_t *, svz_socket_t *);
SERVEEZ_API svz_socket_t *svz_sock_getreferrer (svz_socket_t *);
SERVEEZ_API svz_portcfg_t *svz_sock_portcfg (svz_socket_t *);

SERVEEZ_API int svz_shutting_down_p (void);
SERVEEZ_API void svz_loop_pre (void);
SERVEEZ_API void svz_loop_post (void);
SERVEEZ_API void svz_loop (void);
SERVEEZ_API void svz_loop_one (void);

__END_DECLS


/* (coserver/coserver) */

/*
 * Every invoked internal coserver has got such a structure.
 * It contains all the data it needs to run properly.
 */
typedef struct
{
#ifdef __MINGW32__

  /* Win32 specific part.  */
  CRITICAL_SECTION sync;        /* critical section handle */
  HANDLE thread;                /* the thread handle for access */
  DWORD tid;                    /* internal thread id */

#else /* not __MINGW32__ */

  /* Unix specific part.  */
  int pid;                      /* process id */

#endif /* not __MINGW32__ */

  char * (* callback) (char *); /* callback routine, blocking...  */
  svz_socket_t *sock;           /* socket structure for this coserver */
  int type;                     /* coserver type id */
  int busy;                     /* is this thread currently busy?  */
}
svz_coserver_t;


/*
 * This structure holds a socket's ‘.id’ and ‘.version’ information,
 * created by ‘svz_make_sock_iv’ to be later fed (by the callback) to
 * ‘svz_sock_find’.  The callback should ‘svz_free’ it afterwards.
 */
typedef struct
{
  int id;
  int version;
}
svz_sock_iv_t;

/*
 * The callback structure is used to finally execute some code
 * which should be called whenever one of the coservers produces
 * any data for the server.
 */
typedef int (* svz_coserver_handle_result_t) (char *, void *);

typedef struct
{
  svz_coserver_handle_result_t handle_result; /* any code callback */
  void *closure;                              /* opaque to libserveez */
}
svz_coserver_callback_t;

/*
 * Types of internal servers you can start as threads or processes.
 * coserver-TODO:
 * add your coserver identification here and increase SVZ_MAX_COSERVER_TYPES
 */
#define SVZ_COSERVER_REVERSE_DNS 0 /* reverse DNS lookup ID */
#define SVZ_COSERVER_IDENT       1 /* identification ID */
#define SVZ_COSERVER_DNS         2 /* DNS lookup ID */
#define SVZ_MAX_COSERVER_TYPES   3 /* number of different coservers */

typedef int (svz_coserver_do_t) (const svz_coserver_t *, void *);

__BEGIN_DECLS

SERVEEZ_API svz_sock_iv_t *svz_make_sock_iv (svz_socket_t *);
SERVEEZ_API int svz_foreach_coserver (svz_coserver_do_t *, void *);
SERVEEZ_API void svz_coserver_check (void);
SERVEEZ_API int svz_updn_all_coservers (int);
SERVEEZ_API void svz_coserver_destroy (int);
SERVEEZ_API svz_coserver_t *svz_coserver_create (int);
SERVEEZ_API const char *svz_coserver_type_name (const svz_coserver_t *);

/*
 * These are the three wrappers for our existing coservers.
 */
SERVEEZ_API void svz_coserver_rdns_invoke (svz_address_t *,
                                           svz_coserver_handle_result_t,
                                           void *);

SERVEEZ_API void svz_coserver_dns_invoke (char *,
                                          svz_coserver_handle_result_t,
                                          void *);

SERVEEZ_API void svz_coserver_ident_invoke (svz_socket_t *,
                                            svz_coserver_handle_result_t,
                                            void *);

__END_DECLS


/* (interface) */

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

__END_DECLS


/* (dynload) */

__BEGIN_DECLS

SERVEEZ_API void svz_dynload_path_set (svz_array_t *);
SERVEEZ_API svz_array_t *svz_dynload_path_get (void);

__END_DECLS


/* (passthrough) */

/* Structure containing a system independent environment.  */
typedef struct
{
  int size;     /* Number of environment entries.  */
  char **entry; /* Environment entries in the format "VAR=VALUE".  */
  char *block;  /* Temporary environment block.  */
}
svz_envblock_t;

/* Definitions for the @var{user} argument of @code{svz_sock_process}.  */
#define SVZ_PROCESS_NONE  ((char *) 0L)
#define SVZ_PROCESS_OWNER ((char *) ~0L)

__BEGIN_DECLS

SERVEEZ_API int svz_sock_process (svz_socket_t *, char *, char *,
                                  char **, svz_envblock_t *, int,
                                  char *);
#ifdef __MINGW32__
SERVEEZ_API int svz_mingw_child_dead_p (char *, svz_t_handle *);
#endif
SERVEEZ_API int svz_most_recent_dead_child_p (svz_t_handle);
SERVEEZ_API void svz_envblock_setup (void);
SERVEEZ_API svz_envblock_t *svz_envblock_create (void);
SERVEEZ_API int svz_envblock_default (svz_envblock_t *);
SERVEEZ_API int svz_envblock_add (svz_envblock_t *, char *, ...);
SERVEEZ_API void svz_envblock_destroy (svz_envblock_t *);
SERVEEZ_API void * svz_envblock_get (svz_envblock_t *);

__END_DECLS


/* (codec/codec) */

/* Modes of operation.  */
#define SVZ_CODEC_INIT   0x0001
#define SVZ_CODEC_FLUSH  0x0002
#define SVZ_CODEC_RESET  0x0004
#define SVZ_CODEC_FINISH 0x0008
#define SVZ_CODEC_CODE   0x0010

/* Return values of the codec callbacks.  */
#define SVZ_CODEC_OK       0x0001
#define SVZ_CODEC_FINISHED 0x0002
#define SVZ_CODEC_ERROR    0x0004
#define SVZ_CODEC_MORE_OUT 0x0008
#define SVZ_CODEC_MORE_IN  0x0010

/* Internal state of a codec.  */
#define SVZ_CODEC_NONE  0x0000
#define SVZ_CODEC_READY 0x0001

/* Codec types.  */
#define SVZ_CODEC_ENCODER 0x0001
#define SVZ_CODEC_DECODER 0x0002

typedef struct svz_codec svz_codec_t;
typedef struct svz_codec_data svz_codec_data_t;

/*
 * General codec data structure for both encoding and encoding calls.
 */
struct svz_codec_data
{
  /* Current codec class.  */
  svz_codec_t *codec;

  /* Operation flags.  */
  int flag;

  /* Current state flags.  */
  int state;

  /* Input buffer description.  */
  char *in_buffer;
  int in_fill;
  int in_size;

  /* Output buffer description.  */
  char *out_buffer;
  int out_fill;
  int out_size;

  /* Configuration field (passed to each codec callback).  Could be
     used as compression level, algorithm, etc. indicator.  */
  void *config;

  /* Arbitrary data field.  Can be used by codec for internal data.  */
  void *data;

  /* Saved @code{check_request} callback.  Used by receiving codecs.  */
  int (* check_request) (svz_socket_t *sock);

  /* Saved @code{write_socket} callback.  Used by sending codecs.  */
  int (* write_socket) (svz_socket_t *sock);

  /* Saved @code{disconnected_socket} callback.  Used by both.  */
  int (* disconnected_socket) (svz_socket_t *sock);
};

/*
 * Description of a codec class.
 */
struct svz_codec
{
  /* Name of the codec.  Should be short descriptive name.  */
  char *description;

  /* Codec type.  */
  int type;

  /* Initializer.  */
  int (* init) (svz_codec_data_t *);

  /* Finalizer.  */
  int (* finalize) (svz_codec_data_t *);

  /* Encoding / decoding routine.  */
  int (* code) (svz_codec_data_t *);

  /* Last error description.  */
  char * (* error) (svz_codec_data_t *);

  /* Overall ratio request.  */
  int (* ratio) (svz_codec_data_t *, size_t *, size_t *);

  /* Magic detection sequence.  */
  char *detection;

  /* Length of the above detection sequence.  */
  int detection_size;
};

typedef int (svz_codec_do_t) (const svz_codec_t *, void *);

__BEGIN_DECLS

SERVEEZ_API int svz_foreach_codec (svz_codec_do_t *, void *);
SERVEEZ_API svz_codec_t * svz_codec_get (char *, int);
SERVEEZ_API int svz_codec_register (svz_codec_t *);
SERVEEZ_API int svz_codec_unregister (svz_codec_t *);
SERVEEZ_API int svz_codec_sock_receive_setup (svz_socket_t *,
                                              svz_codec_t *);
SERVEEZ_API int svz_codec_sock_receive (svz_socket_t *);
SERVEEZ_API int svz_codec_sock_send_setup (svz_socket_t *,
                                           svz_codec_t *);
SERVEEZ_API int svz_codec_sock_send (svz_socket_t *);
SERVEEZ_API int svz_codec_sock_disconnect (svz_socket_t *);
SERVEEZ_API void svz_codec_ratio (svz_codec_t *,
                                  svz_codec_data_t *);
SERVEEZ_API svz_codec_t * svz_codec_sock_detect (svz_socket_t *);

__END_DECLS


#endif /* not __LIBSERVEEZ_H__ */

/* (GNU Serveez 0.2.2) libserveez.h ends here */
