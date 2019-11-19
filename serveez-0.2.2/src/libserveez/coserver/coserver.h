/*
 * coserver.h - internal coserver header definitions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __COSERVER_H__
#define __COSERVER_H__ 1

/* begin svzint */
#include "libserveez/defines.h"
#include "libserveez/array.h"
#include "libserveez/address.h"
#include "libserveez/socket.h"
/* end svzint */

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

/* begin svzint */
/* Buffer size for the coservers.  */
#define COSERVER_BUFSIZE 256
/* end svzint */

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

#endif /* not __COSERVER_H__ */
