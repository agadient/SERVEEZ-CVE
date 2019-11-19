/*
 * control-proto.c - control protocol implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "networking-headers.h"
#include "unused.h"

#ifndef __MINGW32__
# include <sys/types.h>
#endif

#if HAVE_LIBKSTAT
# include <kstat.h>
# include <sys/sysinfo.h>
#elif HAVE_SYS_PARAM_H && HAVE_SYS_PSTAT_H
# include <sys/param.h>
# include <sys/pstat.h>
# define HAVE_PSTAT 1
#elif HAVE_SYS_SYSGET_H && HAVE_SYS_SYSINFO_H
# include <sys/sysget.h>
# include <sys/sysinfo.h>
# define HAVE_SYSGET 1
#elif HAVE_HOST_STATISTICS
# include <mach/mach_init.h>
# include <mach/mach_host.h>
#endif

#if HAVE_TIMES
# include <sys/times.h>
#endif

#ifdef HAVE_CRYPT_H
# include <crypt.h>
#endif

#include "libserveez.h"
#include "control-proto.h"

#if ENABLE_HTTP_PROTO
# include "http-server/http-cache.h"
#endif

/*
 * The one and only...
 */
char *control_protocol_password;

/*
 * The control server instance configuration.
 */
ctrl_config_t ctrl_config =
{
  0 /* nothing */
};

/*
 * Definition of the configuration items processed by the configuration
 * language.
 */
svz_key_value_pair_t ctrl_config_prototype [] =
{
  SVZ_REGISTER_END ()
};

/*
 * Definition of the control protocol server.
 */
svz_servertype_t ctrl_server_definition =
{
  "control protocol server", /* long server description */
  "control",                 /* short server description */
  NULL,                      /* global initializer */
  ctrl_init,                 /* instance initializer */
  ctrl_detect_proto,         /* protocol detection routine */
  ctrl_connect_socket,       /* connection routine */
  ctrl_finalize,             /* instance finalization routine */
  NULL,                      /* global finalizer */
  ctrl_info_client,          /* client info */
  ctrl_info_server,          /* server info */
  NULL,                      /* server timer */
  NULL,                      /* server reset callback */
  NULL,                      /* handle request callback */
  SVZ_CONFIG_DEFINE ("control", ctrl_config, ctrl_config_prototype)
};

/*
 * Within the ‘ctrl_idle’ function this structure gets filled with
 * the appropriate data.
 */
cpu_state_t cpu_state;

/*
 * Server instance initializer.  This is currently used for binding the
 * server to a given port configuration.
 */
int
ctrl_init (UNUSED svz_server_t *server)
{
  return 0;
}

/*
 * Server instance finalizer.
 */
int
ctrl_finalize (UNUSED svz_server_t *server)
{
  return 0;
}

/*
 * Server info callback.
 */
char *
ctrl_info_server (UNUSED svz_server_t *server)
{
  static char info[128];

  sprintf (info, " nothing to be configured, yet");
  return info;
}

/*
 * Client info callback.
 */
char *
ctrl_info_client (UNUSED svz_server_t *server,
                  UNUSED svz_socket_t *sock)
{
  static char info[128];

  sprintf (info, "This is a control connection client.");
  return info;
}

/*
 * This function gets called for new sockets which are not yet
 * identified.  It returns a non-zero value when the content in
 * the receive buffer looks like the control protocol.
 */
int
ctrl_detect_proto (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  int ret = 0;

  /* accept both CRLF and CR */
  if (sock->recv_buffer_fill >= 2 &&
      sock->recv_buffer[0] == '\r' &&
      sock->recv_buffer[1] == '\n')
    {
      ret = 2;
    }
  else if (sock->recv_buffer_fill >= 1 && sock->recv_buffer[0] == '\n')
    {
      ret = 1;
    }

  /* control protocol detected */
  if (ret)
    {
      if (ret < sock->recv_buffer_fill)
        {
          memmove (sock->recv_buffer,
                   sock->recv_buffer + ret,
                   sock->recv_buffer_fill - ret);
        }
      sock->recv_buffer_fill -= ret;
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "control protocol client detected\n");
#endif
      return -1;
    }

  return 0;
}

/*
 * Format string for system business output on different systems.
 */
#ifdef HAVE_LIBKSTAT
# define CPU_FORMAT \
  "user %ld.%01ld%%, sys %ld.%01ld%%, wait %ld.%01ld%%, idle %ld.%01ld%%"
#elif HAVE_PROC_STAT
# define CPU_FORMAT \
  "user %ld.%01ld%%, nice %ld.%01ld%%, sys %ld.%01ld%%, idle %ld.%01ld%%"
#elif HAVE_PSTAT
# define CPU_FORMAT \
  "user %ld.%01ld%%, nice %ld.%01ld%%, sys %ld.%01ld%%, idle %ld.%01ld%%"
#elif HAVE_SYSGET
# define CPU_FORMAT \
  "user %ld.%01ld%%, sys %ld.%01ld%%, wait %ld.%01ld%%, idle %ld.%01ld%%"
#elif HAVE_HOST_STATISTICS
# define CPU_FORMAT \
  "user %ld.%01ld%%, sys %ld.%01ld%%, idle %ld.%01ld%%, nice %ld.%01ld%%"
#else
# define CPU_FORMAT "no cpu info available"
#endif

/*
 * When ctrl_detect_proto has identified a client connection being
 * a control protocol connection you have to call the following
 * routine.
 */
int
ctrl_connect_socket (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  svz_sock_resize_buffers (sock, CTRL_SEND_BUFSIZE, CTRL_RECV_BUFSIZE);
  sock->check_request = svz_sock_check_request;
  sock->handle_request = ctrl_handle_request;
  sock->boundary = CTRL_PACKET_DELIMITER;
  sock->boundary_size = CTRL_PACKET_DELIMITER_LEN;
  sock->idle_func = ctrl_idle;
  sock->idle_counter = CTRL_LOAD_UPDATE;

#if HAVE_PROC_STAT
  cpu_state.cpufile = CPU_FILE_NAME;
  cpu_state.cpuline = CPU_LINE_FORMAT;
#elif HAVE_LIBKSTAT /* not HAVE_PROC_STAT */

#else /* neither HAVE_PROC_STAT nor HAVE_LIBKSTAT */
  strcpy (cpu_state.info, CPU_FORMAT);
#endif

  cpu_state.cpuinfoline = CPU_FORMAT;

  /* send welcome message */
  svz_sock_printf (sock, "%s", CTRL_PASSWD);

  return 0;
}

/*
 * Quit command.  If the client sends this command the control protocol
 * connection will be closed immediately.
 */
int
ctrl_quit (UNUSED svz_socket_t *sock, int flag, UNUSED char *arg)
{
  return flag;
}

/*
 * Help screen.  Here you will get all the available commands of the
 * control protocol.  These depend on the features the current version
 * of Serveez implements.
 */
int
ctrl_help (svz_socket_t *sock, int flag, UNUSED char *arg)
{
  svz_sock_printf (sock,
    "\r\n available commands:\r\n"
    "   * help                - this help screen\r\n"
    "   * quit                - quit this control connection\r\n"
    "   * restart ident       - restart the ident coserver\r\n"
    "   * restart reverse dns - restart reverse DNS lookup coserver\r\n"
    "   * restart dns         - restart the DNS lookup coserver\r\n"
    "   * killall             - shutdown all client connections\r\n"
    "   * kill id NUM         - shutdown connection NUM\r\n"
    "   * stat                - general statistics\r\n"
    "   * stat SERVER         - SERVER's statistic\r\n"
    "   * stat coserver       - coserver statistics\r\n"
    "   * stat con            - connection statistics\r\n"
    "   * stat id NUM         - NUM's connection info\r\n"
    "   * stat all            - server and coserver state\r\n"
#if ENABLE_HTTP_PROTO
    "   * stat cache          - http cache statistics\r\n"
    "   * kill cache          - free all http cache entries\r\n"
#endif /* ENABLE_HTTP_PROTO */
    "\r\n");

  return flag;
}

/*
 * ID's connection info.  This function displays a given socket id's
 * socket structure.
 */
int
ctrl_stat_id (svz_socket_t *sock, int flag, char *arg)
{
  size_t n;
  int id;
  svz_socket_t *xsock;
  char proto[128];
  svz_server_t *server;
  svz_coserver_t *coserver;

  /* Find the appropriate client or server connection.  */
  id = atoi (arg);
  if ((xsock = svz_sock_find (id, -1)) == NULL)
    {
      svz_sock_printf (sock, "no such connection: %d\r\n", id);
      return flag;
    }

  svz_sock_printf (sock, "\r\nconnection id %d (version %d) "
                   "statistics\r\n\r\n",
                   id, xsock->version);

  /*
   * Process general socket structure's flags.  Uppercase words refer
   * to set bits and lowercase to unset bits.
   */
  svz_sock_printf (sock,
    " flags    : %s %s %s %s %s %s %s\r\n"
    "            %s %s %s %s %s %s %s\r\n",
    xsock->flags & SVZ_SOFLG_INBUF ?      "INBUF" : "inbuf",
    xsock->flags & SVZ_SOFLG_OUTBUF ?     "OUTBUF" : "outbuf",
    xsock->flags & SVZ_SOFLG_CONNECTED ?  "CONNECTED" : "connected",
    xsock->flags & SVZ_SOFLG_LISTENING ?  "LISTENING" : "listening",
    xsock->flags & SVZ_SOFLG_KILLED ?     "KILLED" : "killed",
    xsock->flags & SVZ_SOFLG_NOFLOOD ?    "flood" : "FLOOD",
    xsock->flags & SVZ_SOFLG_CONNECTING ? "CONNECTING" : "connecting",
    xsock->flags & SVZ_SOFLG_INITED ?     "INITED" : "inited",
    xsock->flags & SVZ_SOFLG_COSERVER ?   "COSERVER" : "coserver",
    xsock->flags & SVZ_SOFLG_PIPE ?       "PIPE" : "pipe",
    xsock->flags & SVZ_SOFLG_FILE ?       "FILE" : "file",
    xsock->flags & SVZ_SOFLG_SOCK ?       "SOCK" : "sock",
    xsock->flags & SVZ_SOFLG_ENQUEUED ?   "ENQUEUED" : "enqueued",
    xsock->flags & SVZ_SOFLG_PRIORITY ?   "PRIORITY" : "priority");

  svz_sock_printf (sock, " protocol : ");

  /* process connection type server flags */
  if (xsock->flags & SVZ_SOFLG_LISTENING)
    {
      svz_array_t *servers;

      /* a listening server */
      strcpy (proto, "server: ");
      if (xsock->proto & SVZ_PROTO_TCP)
        strcat (proto, "tcp ");
      if (xsock->proto & SVZ_PROTO_UDP)
        strcat (proto, "udp ");
      if (xsock->proto & SVZ_PROTO_ICMP)
        strcat (proto, "icmp ");
      if (xsock->proto & SVZ_PROTO_PIPE)
        strcat (proto, "pipe ");
      if (xsock->proto & SVZ_PROTO_RAW)
        strcat (proto, "raw ");

      svz_sock_printf (sock, "%s\r\n", proto);
      servers = svz_sock_servers (xsock);
      svz_array_foreach (servers, server, n)
        {
          svz_sock_printf (sock, "            %d. %s (%s)\r\n",
                           n + 1, server->name, server->description);
        }
      svz_array_destroy (servers);
    }
  /* process client info */
  else
    {
      /* usual client */
      if ((server = svz_server_find (xsock->cfg)) != NULL)
        {
          char *info;
          svz_sock_printf (sock, "%s client\r\n", server->name);
          if (server->info_client &&
              (info = server->info_client (server, xsock)) != NULL)
            {
              svz_sock_printf (sock, "            %s\r\n", info);
            }
        }
      /* coserver */
      else if (xsock->flags & SVZ_SOFLG_COSERVER)
        {
          coserver = xsock->data;
          svz_sock_printf (sock, "internal %s coserver\r\n",
                           svz_coserver_type_name (coserver));
        }
      /* unidentified */
      else
        {
          svz_sock_printf (sock, "not yet identified\r\n");
        }
    }

  /* print all previously collected statistics of this connection */
  if (xsock->flags & SVZ_SOFLG_SOCK)
    svz_sock_printf (sock, " sock fd  : %d\r\n", xsock->sock_desc);
  if (xsock->flags & SVZ_SOFLG_FILE)
    svz_sock_printf (sock, " file fd  : %d\r\n", xsock->file_desc);
  if (xsock->flags & SVZ_SOFLG_PIPE)
    svz_sock_printf (sock, " pipe fd  : %d (recv), %d (send)\r\n",
                     xsock->pipe_desc[SVZ_READ],
                     xsock->pipe_desc[SVZ_WRITE]);

  if (xsock->flags & SVZ_SOFLG_PIPE)
    {
      if (xsock->send_pipe)
        svz_sock_printf (sock, " foreign  : %s\r\n", xsock->send_pipe);
      if (xsock->recv_pipe)
        svz_sock_printf (sock, " local    : %s\r\n", xsock->recv_pipe);
    }
  if (xsock->flags & SVZ_SOFLG_SOCK)
    {
      char buf[64];

      svz_sock_printf (sock, " foreign  : %s\r\n",
                       SVZ_PP_ADDR_PORT (buf, xsock->remote_addr,
                                         xsock->remote_port));
      svz_sock_printf (sock, " local    : %s\r\n",
                       SVZ_PP_ADDR_PORT (buf, xsock->local_addr,
                                         xsock->local_port));
    }

  svz_sock_printf (sock,
                   " sendbuf  : %d (size), %d (fill), %s (last send)\r\n"
                   " recvbuf  : %d (size), %d (fill), %s (last recv)\r\n"
                   " idle     : %d\r\n"
#if ENABLE_FLOOD_PROTECTION
                   " flood    : %d (points), %d (limit)\r\n"
#endif /* ENABLE_FLOOD_PROTECTION */
                   " avail    : %s\r\n\r\n",
                   xsock->send_buffer_size,
                   xsock->send_buffer_fill,
                   svz_time (xsock->last_send),
                   xsock->recv_buffer_size,
                   xsock->recv_buffer_fill,
                   svz_time (xsock->last_recv),
                   xsock->idle_counter,
#if ENABLE_FLOOD_PROTECTION
                   xsock->flood_points,
                   xsock->flood_limit,
#endif /* ENABLE_FLOOD_PROTECTION */
                   xsock->unavailable ? "no" : "yes");

  return flag;
}

static char *
uptime (long diff)
{
#define TEXTSIZE 64
#define SET_TEXT(fmt,...)  snprintf (text, TEXTSIZE, fmt, __VA_ARGS__)

  static char text[TEXTSIZE];
  long sec, min, hour, day, old;

  old = diff;
  sec = diff % 60;
  diff /= 60;
  min = diff % 60;
  diff /= 60;
  hour = diff % 24;
  diff /= 24;
  day = diff;

  if (old < 60)
    SET_TEXT ("%ld sec", sec);
  else if (old < 60 * 60)
    SET_TEXT ("%ld min", min);
  else if (old < 60 * 60 * 24)
    SET_TEXT ("%ld hours, %ld min", hour, min);
  else
    SET_TEXT ("%ld days, %ld:%02ld", day, hour, min);

  return text;

#undef SET_TEXT
#undef TEXTSIZE
}

/*
 * General statistics about Serveez.  Here we display all the information
 * we could get from the system and the process itself.
 * Furthermore we check if the command is something about a certain
 * server and give information about it if so.
 */
int
ctrl_stat (svz_socket_t *sock, int flag, char *arg)
{
  svz_server_t *server;
  char *p;
  long ut = svz_uptime ();

  /* find an appropriate server instance */
  p = arg;
  while (*p && *p != '\r' && *p != '\n')
    p++;
  if (*p)
    *p = '\0';
  if ((server = svz_server_get (arg)) != NULL)
    {
      svz_sock_printf (sock, "\r\n%s (%s):\r\n",
                       server->description, server->name);
      if (server->info_server && (p = server->info_server (server)) != NULL)
        {
          svz_sock_printf (sock, "%s\r\n", p);
        }
      svz_sock_printf (sock, "\r\n");
      return flag;
    }

  /* print a standard output */
  svz_sock_printf (sock,
                   "\r\nThis is %s running since %s.\r\n",
                   PACKAGE_STRING, svz_time (time (NULL) - ut));

  /* display compile time feature list */
  svz_sock_printf (sock, "Features  : FOO"
#ifdef ENABLE_HTTP_PROTO
                   " HTTP"
#endif /* ENABLE_HTTP_PROTO */
#ifdef ENABLE_IRC_PROTO
                   " IRC"
#endif /* ENABLE_IRC_PROTO */
                   " CTRL"
#if ENABLE_SNTP_PROTO
                   " SNTP"
#endif /* ENABLE_SNTP_PROTO */
#if ENABLE_GNUTELLA
                   " NUT"
#endif /* ENABLE_GNUTELLA */
#if ENABLE_TUNNEL
                   " TUNNEL"
#endif /* ENABLE_TUNNEL */
#if ENABLE_FAKEIDENT
                   " IDENTD"
#endif /* ENABLE_FAKEIDENT */
#if ENABLE_PROG_SERVER
                   " PROG"
#endif /* ENABLE_PROG_SERVER */
                   "\r\n");

  /* second feature line */
  svz_sock_printf (sock, "           "
                   " IDENT"
                   " REVERSE-DNS"
                   " DNS"
#ifdef ENABLE_FLOOD_PROTECTION
                   " FLOOD"
#endif /* ENABLE_FLOOD_PROTECTION */
#ifdef ENABLE_DEBUG
                   " DEBUG"
#endif /* ENABLE_DEBUG */
#if defined (__MINGW32__) || defined (__CYGWIN__)
                   " WIN32"
#endif /* __MINGW32__, __CYGWIN__ */
                   "\r\n");

  /* display system and process information */
  svz_sock_printf (sock, "Os        : %s\r\n", svz_sys_version ());
  svz_sock_printf (sock, "Sys-Load  : %s\r\n", cpu_state.info);
  svz_sock_printf (sock, "Proc-Load : %s\r\n", cpu_state.pinfo);

  /* show general state */
  svz_sock_printf (sock, "\r\n * %d connected sockets (hard limit is %d)\r\n",
                   svz_sock_nconnections (), SVZ_RUNPARM (MAX_SOCKETS));
  svz_sock_printf (sock, " * uptime is %s\r\n", uptime (ut));
#if ENABLE_DEBUG
  {
    size_t cur[2];

    svz_get_curalloc (cur);
    svz_sock_printf (sock, " * %d bytes of memory in %d blocks allocated\r\n",
                     cur[0], cur[1]);
  }
#endif /* ENABLE_DEBUG */
  svz_sock_printf (sock, "\r\n");

  return flag;
}

static int
stat_con_internal (svz_socket_t *sock, void *closure)
{
  svz_socket_t *to = closure;
  char *id;
  char linet[64];
  char rinet[64];
  svz_server_t *server;

  if (sock->flags & SVZ_SOFLG_LISTENING)
    id = "Listener";
  else if (sock->flags & SVZ_SOFLG_COSERVER)
    id = "Co-Server";
  else if ((server = svz_server_find (sock->cfg)) != NULL)
    id = server->name;
  else
    id = "None";

  svz_sock_printf (to,
                   "%-16s %4d %6d %6d "
                   "%-20s %-20s"        /* FIXME: IPv4 */
                   "\r\n", id,
                   sock->id, sock->recv_buffer_fill,
                   sock->send_buffer_fill,
                   SVZ_PP_ADDR_PORT (linet, sock->local_addr, sock->local_port),
                   SVZ_PP_ADDR_PORT (rinet, sock->remote_addr, sock->remote_port));
  return 0;
}

/*
 * Connection statistics.  This function displays basic information about
 * each socket structure currently within the socket list.
 */
int
ctrl_stat_con (svz_socket_t *sock, int flag, UNUSED char *arg)
{
  svz_sock_printf (sock, "\r\n%s",
                   "Proto              Id  RecvQ  SendQ "
                   "Local                Foreign\r\n");
  svz_foreach_socket (stat_con_internal, sock);
  svz_sock_printf (sock, "\r\n");

  return flag;
}

#if ENABLE_HTTP_PROTO
/*
 * HTTP cache statistics.  The following displayed information is a
 * visual representation of the http cache structures.
 */
int
ctrl_stat_cache (svz_socket_t *sock, int flag, UNUSED char *arg)
{
  int n, total, files;
  char *p;
  http_cache_entry_t *cache;

  svz_sock_printf (sock, "\r\n%s",
                   "File                             "
                   "Size  Usage  Hits Recent Ready\r\n");

  files = total = n = 0;
  /* go through each cache entry */
  for (cache = http_cache_first; cache; cache = cache->next, n++)
    {
      files++;
      total += cache->size;
      p = cache->file;
      p += strlen (cache->file);
      while (*p != '/' && *p != '\\' && p != cache->file) p--;
      if (p != cache->file) p++;
      svz_sock_printf (sock, "%-30s %6d %6d %5d %6d %-5s\r\n", p,
                       cache->size, cache->usage, cache->hits, n,
                       cache->ready ? "Yes" : "No");
    }

  /* print cache summary */
  svz_sock_printf (sock, "\r\nTotal : %d byte in %d cache entries\r\n\r\n",
                   total, files);

  return flag;
}

/*
 * Free all HTTP cache entries.
 */
int
ctrl_kill_cache (svz_socket_t *sock, int flag, UNUSED char *arg)
{
  svz_sock_printf (sock, "%d HTTP cache entries reinitialized.\r\n",
                   http_cache_entries);
  http_free_cache ();
  http_alloc_cache (http_cache_entries);
  return flag;
}
#endif /* ENABLE_HTTP_PROTO */

static int
stat_coservers_internal (const svz_coserver_t *coserver,
                         void *closure)
{
  svz_socket_t *sock = closure;

  svz_sock_printf (sock, "\r\ninternal %s coserver:\r\n",
                   svz_coserver_type_name (coserver));
  svz_sock_printf (sock,
                   " socket id  : %d\r\n"
                   " %s %d\r\n"
                   " requests   : %d\r\n",
                   coserver->sock->id,
#ifndef __MINGW32__
                   "process id :", coserver->pid,
#else /* __MINGW32__ */
                   "thread id  :", coserver->tid,
#endif /* __MINGW32__ */
                   coserver->busy);
  return 0;
}

/*
 * Show all Co-Server instances statistics.
 */
int
ctrl_stat_coservers (svz_socket_t *sock, int flag, UNUSED char *arg)
{
  /* go through all internal coserver instances */
  svz_foreach_coserver (stat_coservers_internal, sock);
  svz_sock_printf (sock, "\r\n");
  return flag;
}

static void
stat_all_internal (svz_server_t *server, void *closure)
{
  svz_socket_t *sock = closure;

  svz_sock_printf (sock, "\r\n%s (%s):\r\n",
                   server->description, server->name);
  if (server->info_server)
    svz_sock_printf (sock, "%s\r\n", server->info_server (server));
}

/*
 * Server and Co-Server instance statistics.
 */
int
ctrl_stat_all (svz_socket_t *sock, int flag, char *arg)
{
  /* go through all server instances */
  svz_foreach_server (stat_all_internal, sock);

  /* show coserver statistics */
  ctrl_stat_coservers (sock, flag, arg);

  return flag;
}

/*
 * Shutdown a specified network connection.  This might even be used to
 * kill your own (the control client's) connection, coservers and servers.
 * So you want to be *very* careful with this command.
 */
int
ctrl_kill_id (svz_socket_t *sock, int flag, char *arg)
{
  int id;
  svz_socket_t *xsock;

  id = atoi (arg);
  if ((xsock = svz_sock_find (id, -1)) == NULL)
    {
      svz_sock_printf (sock, "no such connection: %d\r\n", id);
      return flag;
    }

  svz_sock_schedule_for_shutdown (xsock);
  svz_sock_printf (sock, "scheduled socket id %d for shutdown\r\n", id);
  return flag;
}

struct killall_closure
{
  svz_socket_t *to;
  int n;
};

static int
killall_internal (svz_socket_t *sock, void *closure)
{
  struct killall_closure *x = closure;

  if (x->to != sock && !(sock->flags & (SVZ_SOFLG_LISTENING
                                        | SVZ_SOFLG_COSERVER
                                        | SVZ_SOFLG_PRIORITY)))
    {
      svz_sock_schedule_for_shutdown (sock);
      (x->n)++;
    }
  return 0;
}

/*
 * Shutdown all network connections except listening, control connections,
 * coservers and sockets with the priority flag set.
 */
int
ctrl_killall (svz_socket_t *sock, int flag, UNUSED char *arg)
{
  struct killall_closure closure = { sock, 0 };

  svz_foreach_socket (killall_internal, &closure);
  svz_sock_printf (sock, "killed %d network connections\r\n", closure.n);

  return flag;
}

struct restart_closure
{
  svz_socket_t *sock;
  int type;
};

static int
restart_coservers_internal (const svz_coserver_t *coserver,
                            void *closure)
{
  struct restart_closure *x = closure;
  svz_socket_t *sock = x->sock;
  int type = x->type;

  if (coserver->type != type)
    return 0;

  svz_coserver_destroy (type);
  svz_coserver_create (type);
  svz_sock_printf (sock, "internal %s coserver restarted\r\n",
                   svz_coserver_type_name (coserver));
  return -1;
}

/*
 * Restart coservers.
 */
int
ctrl_restart (svz_socket_t *sock, int type, UNUSED char *arg)
{
  struct restart_closure closure = { sock, type };
  svz_coserver_t *coserver;

  /* find an appropriate coserver to kill */
  if (0 > svz_foreach_coserver (restart_coservers_internal, &closure))
    return 0;

  /* start a new internal coserver if there has none found */
  coserver = svz_coserver_create (type);
  svz_sock_printf (sock, "internal %s coserver invoked\r\n",
                   svz_coserver_type_name (coserver));
  return 0;
}

/*
 * This structure defines the calling conventions for the various
 * control protocol commands.
 */
struct
{
  char *command;                            /* the complete command string */
  int (*func)(svz_socket_t *, int, char *); /* callback routine */
  int flag;                                 /* second argument */
}
ctrl[] =
{
  { CTRL_CMD_HELP,          ctrl_help, 0 },
  { CTRL_CMD_QUIT,          ctrl_quit, -1 },
  { CTRL_CMD_EXIT,          ctrl_quit, -1 },
  { CTRL_CMD_BYE,           ctrl_quit, -1 },
  { CTRL_CMD_STAT_COSERVER, ctrl_stat_coservers, 0 },
  { CTRL_CMD_STAT_CON,      ctrl_stat_con, 0 },
  { CTRL_CMD_STAT_ID,       ctrl_stat_id, 0 },
  { CTRL_CMD_STAT_ALL,      ctrl_stat_all, 0 },
#if ENABLE_HTTP_PROTO
  { CTRL_CMD_STAT_CACHE,    ctrl_stat_cache, 0 },
  { CTRL_CMD_KILL_CACHE,    ctrl_kill_cache, 0 },
#endif
  { CTRL_CMD_STAT,          ctrl_stat, 0 },
  { CTRL_CMD_KILLALL,       ctrl_killall, 0 },
  { CTRL_CMD_KILL_ID,       ctrl_kill_id, 0 },
  { CTRL_CMD_RESTART_RDNS,  ctrl_restart, SVZ_COSERVER_REVERSE_DNS },
  { CTRL_CMD_RESTART_IDENT, ctrl_restart, SVZ_COSERVER_IDENT },
  { CTRL_CMD_RESTART_DNS,   ctrl_restart, SVZ_COSERVER_DNS },
  { NULL, NULL, 0 }
};

/*
 * The ctrl_handle_request routine gets called by the check_request
 * routine of the control protocol.
 */
int
ctrl_handle_request (svz_socket_t *sock, char *request, int len)
{
  static char last_request[CTRL_RECV_BUFSIZE];
  static int last_len;
  int n;
  int ret = 0;
  int l;

  /* search through if there is an input line */
  while (request[len - 1] == '\r' || request[len - 1] == '\n')
    len--;

  /* password given?  */
  if (!(sock->userflags & CTRL_FLAG_PASSED))
    {
      /*
       * check here the control protocol password
       */
      if (len <= 2) return -1;
#if defined HAVE_CRYPT
      request[len] = '\0';
      if (control_protocol_password == NULL ||
          !strcmp (crypt (request, control_protocol_password),
                   control_protocol_password))
#else
      if (control_protocol_password == NULL ||
          (!memcmp (request, control_protocol_password, len) &&
           (unsigned) len >= strlen (control_protocol_password)))
#endif
        {
          sock->userflags |= CTRL_FLAG_PASSED;
          svz_sock_printf (sock, "Login ok.\r\n%s", CTRL_PROMPT);
        }
      else return -1;
    }
  /* yes, already logged in */
  else if (len > 0)
    {
      /* repeat last command */
      if (!memcmp (request, "/\r\n", 3))
        {
          memcpy (request, last_request, len = last_len);
        }
      /* go through all commands */
      n = 0;
      while (ctrl[n].command != NULL)
        {
          l = strlen (ctrl[n].command);
          if (!memcmp (request, ctrl[n].command, l))
            {
              /* save this command for repetition */
              memcpy (last_request, request, last_len = len);

              /* execute valid command and give the prompt */
              ret = ctrl[n].func (sock, ctrl[n].flag, &request[l + 1]);
              svz_sock_printf (sock, "%s", CTRL_PROMPT);
              return ret;
            }
          n++;
        }
      l = 0;
      while (l < len && request[l] >= ' ') l++;
      request[l] = 0;
      svz_sock_printf (sock, "no such command: %s\r\n", request);
      svz_sock_printf (sock, "%s", CTRL_PROMPT);
    }
  else
    {
      svz_sock_printf (sock, "%s", CTRL_PROMPT);
    }
  return ret;
}

/*
 * Depending on the systems this routine gets the cpu load.
 * Returns -1 if an error occurred.
 * Linux   -- /proc/stat
 * HP-Unix -- ‘pstat_getdynamic’
 * Solaris -- ‘kstat_read’
 * Irix    -- ‘sysget’
 * MacOS   -- ‘host_statistics’
 */
static int
ctrl_get_cpu_state (void)
{
  int n;

#if HAVE_LIBKSTAT
  static kstat_ctl_t *kc;
  static kstat_t *ksp = NULL;
  static cpu_stat_t cs;
#elif HAVE_PROC_STAT
  FILE *f;
  static char stat[STAT_BUFFER_SIZE];
#elif HAVE_PSTAT
  struct pst_dynamic stats;
#elif HAVE_SYSGET
  struct sysinfo_cpu info;
  sgt_cookie_t cookie;
#elif HAVE_HOST_STATISTICS
  host_cpu_load_info_data_t info;
  mach_msg_type_number_t count;
#endif

#if HAVE_TIMES
  struct tms proc_tms;
#endif

  n = (cpu_state.index + 1) & 1;

#if HAVE_TIMES
  cpu_state.ptotal[n] = times (&proc_tms);
  cpu_state.proc[n][0] = proc_tms.tms_utime;
  cpu_state.proc[n][1] = proc_tms.tms_stime;
  cpu_state.proc[n][2] = proc_tms.tms_cutime;
  cpu_state.proc[n][3] = proc_tms.tms_cstime;
#else /* not HAVE_TIMES */
  cpu_state.ptotal[n] = cpu_state.ptotal[cpu_state.index] +
    (CLOCKS_PER_SEC * CTRL_LOAD_UPDATE);
  cpu_state.proc[n][0] = clock ();
#endif /* not HAVE_TIMES */

#if HAVE_LIBKSTAT /* Solaris */

  if (ksp == NULL)
    {
      kc = kstat_open ();

      for (ksp = kc->kc_chain; ksp != NULL; ksp = ksp->ks_next)
        if (strncmp (ksp->ks_name, "cpu_stat", 8) == 0)
          break;
    }
  else
    {
      if (kstat_read (kc, ksp, &cs) == -1)
        {
          snprintf (cpu_state.info, STAT_BUFFER_SIZE,
                    "kstat_read(2) failed");
          return -1;
        }

      cpu_state.cpu[n][0] = cs.cpu_sysinfo.cpu[CPU_USER];
      cpu_state.cpu[n][1] = cs.cpu_sysinfo.cpu[CPU_KERNEL];
      cpu_state.cpu[n][2] = cs.cpu_sysinfo.cpu[CPU_WAIT];
      cpu_state.cpu[n][3] = cs.cpu_sysinfo.cpu[CPU_IDLE];
    }

#elif HAVE_PROC_STAT /* Linux */

  /* open the statistics file */
  if ((f = svz_fopen (cpu_state.cpufile, "r")) == NULL)
    {
      snprintf (cpu_state.info, STAT_BUFFER_SIZE,
                "%s not available", cpu_state.cpufile);
      return -1;
    }

  /* find the appropriate cpu statistics line */
  while (fgets (stat, STAT_BUFFER_SIZE, f))
    {
      if (4 == sscanf (stat, cpu_state.cpuline,
                       &cpu_state.cpu[n][0],
                       &cpu_state.cpu[n][1],
                       &cpu_state.cpu[n][2],
                       &cpu_state.cpu[n][3]))
        {
          svz_fclose (f);
          return 0;
        }
    }

  /* cpu line not found */
  snprintf (cpu_state.info, STAT_BUFFER_SIZE,
            "cpu line not found in %s", cpu_state.cpufile);
  svz_fclose (f);

#elif HAVE_PSTAT /* HP Unix */

  pstat_getdynamic (&stats, sizeof (struct pst_dynamic), 1, 0);

  cpu_state.cpu[n][0] = stats.psd_cpu_time[0];
  cpu_state.cpu[n][1] = stats.psd_cpu_time[1];
  cpu_state.cpu[n][2] = stats.psd_cpu_time[2];
  cpu_state.cpu[n][3] = stats.psd_cpu_time[3];

#elif HAVE_SYSGET /* Irix */

  SGT_COOKIE_INIT (&cookie);
  sysget (SGT_SINFO_CPU, (char *) &info, sizeof (info), SGT_READ, &cookie);

  cpu_state.cpu[n][0] = info.cpu[CPU_USER];
  cpu_state.cpu[n][1] = info.cpu[CPU_KERNEL];
  cpu_state.cpu[n][2] = info.cpu[CPU_WAIT];
  cpu_state.cpu[n][3] = info.cpu[CPU_IDLE];

#elif HAVE_HOST_STATISTICS /* MacOS */

  host_statistics (mach_host_self (),
                   HOST_CPU_LOAD_INFO, (host_info_t) &info, &count);

  cpu_state.cpu[n][0] = info.cpu_ticks[CPU_STATE_USER];
  cpu_state.cpu[n][1] = info.cpu_ticks[CPU_STATE_SYSTEM];
  cpu_state.cpu[n][2] = info.cpu_ticks[CPU_STATE_IDLE];
  cpu_state.cpu[n][3] = info.cpu_ticks[CPU_STATE_NICE];

#endif
  return 0;
}

/*
 * Within the CTRL_IDLE function the server gets the CPU
 * load.  This procedure differs on different platforms.
 */

#define PROC_DIFF(x) (c->proc[n][x] - c->proc[old][x])
#define CPU_DIFF(x) (c->cpu[n][x] - c->cpu[old][x])

int
ctrl_idle (svz_socket_t *sock)
{
  int n, old, ret;
  unsigned long all;
  cpu_state_t *c = &cpu_state;

  old = c->index;
  n = (c->index + 1) & 1;

  /* get status of the cpu and process */
  ret = ctrl_get_cpu_state ();

  /* calculate process specific info */
  all = c->ptotal[n] - c->ptotal[old];
  if (all != 0)
    {
      snprintf (c->pinfo, STAT_BUFFER_SIZE, PROC_FORMAT,
                PROC_DIFF (0) * 100 / all,
                PROC_DIFF (0) * 1000 / all % 10,
                PROC_DIFF (1) * 100 / all,
                PROC_DIFF (1) * 1000 / all % 10,
                PROC_DIFF (2) * 100 / all,
                PROC_DIFF (2) * 1000 / all % 10,
                PROC_DIFF (3) * 100 / all,
                PROC_DIFF (3) * 1000 / all % 10);
    }

  if (ret != -1)
    {
      /* calculate cpu specific info */
      c->total[n] = c->cpu[n][0] + c->cpu[n][1] + c->cpu[n][2] + c->cpu[n][3];
      all = c->total[n] - c->total[old];
      if (all != 0)
        {
          snprintf (c->info, STAT_BUFFER_SIZE, c->cpuinfoline,
                    CPU_DIFF (0) * 100 / all,
                    CPU_DIFF (0) * 1000 / all % 10,
                    CPU_DIFF (1) * 100 / all,
                    CPU_DIFF (1) * 1000 / all % 10,
                    CPU_DIFF (2) * 100 / all,
                    CPU_DIFF (2) * 1000 / all % 10,
                    CPU_DIFF (3) * 100 / all,
                    CPU_DIFF (3) * 1000 / all % 10);
        }
    }

  c->index = n;
  sock->idle_counter = CTRL_LOAD_UPDATE;
  return 0;
}
