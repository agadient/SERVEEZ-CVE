/*
 * http-proto.c - http protocol implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
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
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>

#if HAVE_FLOSS_H
# include <floss.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#include "o-binary.h"
#include "networking-headers.h"

#ifdef __MINGW32__
# include <io.h>
# include "woe-statpred.h"
#endif

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
#endif

#include "libserveez.h"
#include "misc-macros.h"
#include "http-proto.h"
#include "http-core.h"
#include "http-cgi.h"
#include "http-dirlist.h"
#include "http-cache.h"
#include "unused.h"

/*
 * The HTTP server instance configuration.
 */
http_config_t http_config =
{
  "index.html",       /* standard index file  for GET request */
  "../show",          /* document root */
  "/cgi-bin",         /* how cgi-requests are detected */
  "./cgibin",         /* cgi script root */
  MAX_CACHE_SIZE,     /* maximum file size to cache them */
  MAX_CACHE,          /* maximum amount of cache entries */
  HTTP_TIMEOUT,       /* server shuts connection down after x seconds */
  HTTP_MAXKEEPALIVE,  /* how many files when using keep-alive */
  "text/plain",       /* standard content type */
  "/etc/mime.types",  /* standard content type file */
  NULL,               /* current content type hash */
  NULL,               /* cgi application associations */
  "root@localhost",   /* email address of server administrator */
  NULL,               /* host name of which is sent back to clients */
  "public_html",      /* appended onto a user's home (~user request) */
  0,                  /* enable reverse DNS lookups */
  0,                  /* enable identd requests */
  "http-access.log",  /* log file name */
  HTTP_CLF,           /* custom log file format string */
  NULL                /* log file stream */
};

/*
 * Definition of the configuration items processed by the configuration
 * callbacks.
 */
svz_key_value_pair_t http_config_prototype[] =
{
  SVZ_REGISTER_STR ("indexfile", http_config.indexfile, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("docs", http_config.docs, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("cgi-url", http_config.cgiurl, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("cgi-dir", http_config.cgidir, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("cache-size", http_config.cachesize, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("cache-entries", http_config.cacheentries,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("timeout", http_config.timeout, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_INT ("keepalive", http_config.keepalive, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("default-type", http_config.default_type,
                    SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("type-file", http_config.type_file, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_HASH ("types", http_config.types, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_HASH ("cgi-application", http_config.cgiapps,
                     SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("admin", http_config.admin, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("host", http_config.host, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("logfile", http_config.logfile, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("logformat", http_config.logformat, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_STR ("userdir", http_config.userdir, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_BOOL ("nslookup", http_config.nslookup, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_BOOL ("ident", http_config.ident, SVZ_ITEM_DEFAULTABLE),
  SVZ_REGISTER_END ()
};

/*
 * Definition of the http server.
 */
svz_servertype_t http_server_definition =
{
  "http server",         /* long server description */
  "http",                /* short server description */
  http_global_init,      /* global initializer */
  http_init,             /* instance initializer */
  http_detect_proto,     /* protocol detection routine */
  http_connect_socket,   /* connection routine */
  http_finalize,         /* instance finalization routine */
  http_global_finalize,  /* global finalizer */
  http_info_client,      /* client info */
  http_info_server,      /* server info */
  NULL,                  /* server timer */
  NULL,                  /* server reset */
  NULL,                  /* handle request callback */
  SVZ_CONFIG_DEFINE ("http", http_config, http_config_prototype)
};

/*
 * HTTP request types, their identification string (including its length)
 * and the appropriate callback routine itself.  This array is used in
 * the HTTP_HANDLE_REQUEST function.
 */
struct
{
  char *ident;                                  /* identification string */
  int len;                                      /* the length of this string */
  int (*response)(svz_socket_t *, char *, int); /* the callback routine */
}
http_request [HTTP_REQUESTS] =
{

  { "GET",     3, http_get_response     },
  { "HEAD",    4, http_head_response    },
  { "POST",    4, http_post_response    },
  { "PUT",     3, http_default_response },
  { "OPTIONS", 7, http_default_response },
  { "DELETE",  6, http_default_response },
  { "TRACE",   5, http_default_response },
  { "CONNECT", 7, http_default_response }

};

/*
 * Global http server initializer.
 */
int
http_global_init (UNUSED svz_servertype_t *server)
{
#ifdef __MINGW32__
  http_start_netapi ();
#endif /* __MINGW32__ */
  http_alloc_cache (MAX_CACHE);
  return 0;
}

/*
 * Global http server finalizer.
 */
int
http_global_finalize (UNUSED svz_servertype_t *server)
{
  http_free_cache ();
#ifdef __MINGW32__
  http_stop_netapi ();
#endif /* __MINGW32__ */
  return 0;
}

/*
 * Local http server instance initializer.
 */
int
http_init (svz_server_t *server)
{
  size_t types = 0;
  char *p;
  in_addr_t host = INADDR_ANY;
  http_config_t *cfg = server->cfg;
  svz_array_t *ports;
  struct sockaddr_in *addr;

  /* resolve localhost if server name is not set */
  if (!cfg->host)
    {
      svz_address_t *address;

      if ((ports = svz_server_portcfgs (server)) != NULL)
        {
          addr = svz_portcfg_addr ((svz_portcfg_t *) svz_array_get (ports, 0));
          host = addr->sin_addr.s_addr;
          svz_array_destroy (ports);
        }
      if (host == INADDR_ANY)
        host = htonl (INADDR_LOOPBACK);
      address = svz_address_make (AF_INET, &host);
      svz_coserver_rdns_invoke (address, http_localhost, cfg);
      svz_free (address);
    }

  /* start http logging system */
  if (cfg->logfile)
    {
      if ((cfg->log = svz_fopen (cfg->logfile, "at")) == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "http: cannot open access logfile %s\n",
                   cfg->logfile);
        }
    }

  /* create content type hash */
  if (cfg->types)
    types = svz_hash_size (cfg->types);

  if (http_read_types (cfg))
    {
      svz_log (SVZ_LOG_ERROR, "http: unable to load %s\n", cfg->type_file);
    }
  svz_log (SVZ_LOG_NOTICE, "http: %d+%d known content types\n",
           types, svz_hash_size (cfg->types) - types);

  /* check user directory path, snip trailing '/' or '\' */
  if (!cfg->userdir || !strlen (cfg->userdir))
    {
      svz_log (SVZ_LOG_ERROR, "http: not a valid user directory\n");
      return -1;
    }
  p = cfg->userdir + strlen (cfg->userdir) - 1;
  if (*p == '/' || *p == '\\')
    *p = '\0';

  /* check document root path */
  if (!strlen (cfg->docs))
    {
      svz_log (SVZ_LOG_ERROR, "http: not a valid document root\n");
      return -1;
    }

  /* checking whether http doc root path ends in '/' or '\'.  */
  p = cfg->docs + strlen (cfg->docs) - 1;
  if (*p == '/' || *p == '\\')
    *p = '\0';

  if (cfg->cacheentries > 0)
    http_alloc_cache (cfg->cacheentries);

  /* generate cgi associations */
  http_gen_cgi_apps (cfg);

  return 0;
}

/*
 * Local http server instance finalizer.
 */
int
http_finalize (svz_server_t *server)
{
  http_config_t *cfg = server->cfg;

  if (cfg->log)
    svz_fclose (cfg->log);

  return 0;
}

/*
 * This function frees all HTTP request properties previously reserved
 * and frees the cache structure if necessary.  Nevertheless the
 * socket structure SOCK should still be usable for keep-alive connections.
 */
void
http_free_socket (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;
  int n;

  /* log this entry and free the request string */
  http_log (sock);
  if (http->request)
    {
      svz_free (http->request);
      http->request = NULL;
    }
  http->timestamp = 0;
  http->response = 0;
  http->length = 0;

  /* any property at all?  */
  if (http->property)
    {
      /* go through all properties */
      n = 0;
      while (http->property[n])
        {
          svz_free (http->property[n]);
          n++;
        }
      svz_free (http->property);
      http->property = NULL;
    }

  /* decrement usage counter of the cache entry */
  if (sock->userflags & HTTP_FLAG_CACHE)
    {
      http->cache->entry->usage--;
    }

  /* is the cache entry used?  */
  if (http->cache)
    svz_free_and_zero (http->cache);

  /* close the file descriptor for usual http file transfer */
  if (sock->file_desc != -1)
    {
      if (svz_close (sock->file_desc) == -1)
        svz_log_sys_error ("close");
      sock->file_desc = -1;
    }
}

/*
 * Disconnects a HTTP connection.  Callback routine for the
 * socket structure entry "disconnected_socket".
 */
int
http_disconnect (svz_socket_t *sock)
{
  /* get http socket structure */
  http_socket_t *http = sock->data;

  /* free the http socket structures */
  http_free_socket (sock);

  if (http)
    {
      if (http->host)
        svz_free (http->host);
      if (http->ident)
        svz_free (http->ident);
      svz_free (http);
      sock->data = NULL;
    }

  return 0;
}

/*
 * Idle function for HTTP Keep-Alive connections.  It simply returns -1
 * if a certain time has elapsed and the main server loop will shutdown
 * the connection therefore.
 */
int
http_idle (svz_socket_t *sock)
{
  time_t now;
  http_config_t *cfg = sock->cfg;

  now = time (NULL);
  if (now - sock->last_recv > cfg->timeout &&
      now - sock->last_send > cfg->timeout)
    return -1;
  sock->idle_counter = 1;

  return http_cgi_died (sock);
}

#if ENABLE_SENDFILE
#if defined (HAVE_SENDFILE) || defined (__MINGW32__)
/*
 * This routine is using ‘sendfile’ to transport large file's content
 * to a network socket.  It is replacing HTTP_DEFAULT_WRITE on systems where
 * this function is implemented.  Furthermore you do not need to set
 * the READ_SOCKET callback HTTP_FILE_READ.
 */
int
http_send_file (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;
  int num_written, do_write;

  /* Limitate the number of bytes to write at once.  */
  do_write = http->filelength > SVZ_SOCK_MAX_WRITE
    ? SVZ_SOCK_MAX_WRITE : http->filelength;

  /* Try sending throughout file descriptor to socket.  */
  num_written = svz_sendfile (sock->sock_desc, sock->file_desc,
                              &http->fileoffset, do_write);

  /* Some error occurred.  */
  if (num_written < 0)
    {
      svz_log_sys_error ("http: sendfile");
      return -1;
    }

  /* Bogus file.  File size from ‘stat’ was not true.  */
  if (num_written == 0 && http->filelength != 0)
    {
      return -1;
    }

  /* Data has been read or EOF reached, set the appropriate flags.  */
  http->filelength -= num_written;
  http->length += num_written;

  /* Read all file data?  */
  if (http->filelength <= 0)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "http: file successfully sent\n");
#endif
      /*
       * no further ‘read’s from the file descriptor, signaling
       * the writers there will not be additional data from now on
       */
      sock->read_socket = svz_tcp_read_socket;
      sock->recv_buffer_fill = 0;
      sock->send_buffer_fill = 0;
      sock->write_socket = http_default_write;
      sock->userflags &= ~HTTP_FLAG_SENDFILE;
      num_written = http_keep_alive (sock);
      svz_tcp_cork (sock->sock_desc, 0);
    }

  return (num_written < 0) ? -1 : 0;
}
#endif /* HAVE_SENDFILE */
#endif /* ENABLE_SENDFILE */

/*
 * HTTP_DEFAULT_WRITE will shutdown the connection immediately when
 * the whole response has been sent (indicated by the HTTP_FLAG_DONE
 * flag) with two exceptions.  It will keep the connection if the
 * actual file is within the cache and if this is a keep-alive connection.
 */
int
http_default_write (svz_socket_t *sock)
{
  int num_written;

  /*
   * Write as many bytes as possible, remember how many
   * were actually sent.
   */
  num_written = send (sock->sock_desc, sock->send_buffer,
                      sock->send_buffer_fill, 0);

  /* some data has been written */
  if (num_written > 0)
    {
      sock->last_send = time (NULL);

      if (sock->send_buffer_fill > num_written)
        {
          memmove (sock->send_buffer,
                   sock->send_buffer + num_written,
                   sock->send_buffer_fill - num_written);
        }
      sock->send_buffer_fill -= num_written;
    }

  /* write error occurred */
  else if (num_written < 0)
    {
      svz_log_net_error ("http: send");
      if (svz_wait_if_unavailable (sock, 1))
        num_written = 0;
    }

  /*
   * Check if the http response has (success)fully been sent.
   * If yes then return non-zero in order to shutdown the socket SOCK
   * and return zero if it is a keep-alive connection.
   */
  if ((sock->userflags & HTTP_FLAG_DONE) && sock->send_buffer_fill == 0)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "http: response successfully sent\n");
#endif
      num_written = http_keep_alive (sock);
    }

  /*
   * If the requested file is within the cache then start now the
   * cache writer.  Set SEND_BUFFER_FILL to something greater than zero.
   */
  if (sock->send_buffer_fill == 0)
    {
      if (sock->userflags & HTTP_FLAG_CACHE)
        {
          sock->send_buffer_fill = 42;
          sock->write_socket = http_cache_write;
        }
#if ENABLE_SENDFILE
#if defined (HAVE_SENDFILE) || defined (__MINGW32__)
# ifdef __MINGW32__
      else if (sock->userflags & HTTP_FLAG_SENDFILE &&
               svz_mingw_at_least_nt4_p ())
# else
      else if (sock->userflags & HTTP_FLAG_SENDFILE)
# endif
        {
          sock->send_buffer_fill = 42;
          sock->write_socket = http_send_file;
        }
#endif /* HAVE_SENDFILE || __MINGW32__ */
#endif /* ENABLE_SENDFILE */
    }

  /*
   * Return a non-zero value if an error occurred.
   */
  return (num_written < 0) ? -1 : 0;
}

/*
 * The HTTP_FILE_READ reads as much data from a file as possible directly
 * into the send buffer of the socket SOCK.  It returns a non-zero value
 * on read errors.  When all the file has been read then the socket flag
 * HTTP_FLAG_DONE is set.
 */
int
http_file_read (svz_socket_t *sock)
{
  int num_read;
  int do_read;
  http_socket_t *http;

  http = sock->data;
  do_read = sock->send_buffer_size - sock->send_buffer_fill;

  /*
   * This means the send buffer is currently full, we have to
   * wait until some data has been send via the socket.
   */
  if (do_read <= 0)
    {
      return 0;
    }

#ifndef __MINGW32__
  /*
   * Try to read as much data as possible from the file.
   */
  num_read = read (sock->file_desc,
                   sock->send_buffer + sock->send_buffer_fill, do_read);

  /* Read error occurred.  */
  if (num_read < 0)
    {
      svz_log_sys_error ("http: read");
      return -1;
    }
#else
  if (!ReadFile ((HANDLE) sock->file_desc,
                 sock->send_buffer + sock->send_buffer_fill,
                 do_read, (DWORD *) &num_read, NULL))
    {
      svz_log_sys_error ("http: ReadFile");
      return -1;
    }
#endif

  /* Bogus file.  File size from ‘stat’ was not true.  */
  if (num_read == 0 && http->filelength != 0)
    {
      return -1;
    }

  /* Data has been read or EOF reached, set the appropriate flags.  */
  sock->send_buffer_fill += num_read;
  http->filelength -= num_read;
  http->length += num_read;

  /* Read all file data?  */
  if (http->filelength <= 0)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "http: file successfully read\n");
#endif
      /*
       * no further ‘read’s from the file descriptor, signaling
       * the writers there will not be additional data from now on
       */
      sock->read_socket = svz_tcp_read_socket;
      sock->userflags |= HTTP_FLAG_DONE;
      sock->flags &= ~SVZ_SOFLG_FILE;
    }

  return 0;
}

/*
 * This function gets called for new sockets which are not yet
 * identified.  It returns a non-zero value when the content in
 * the receive buffer looks like an HTTP request.
 */
int
http_detect_proto (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  int n;

  /* go through all possible request types */
  for (n = 0; n < HTTP_REQUESTS; n++)
    {
      if (sock->recv_buffer_fill >= http_request[n].len)
        {
          if (!memcmp (sock->recv_buffer, http_request[n].ident,
                       http_request[n].len))
            {
#if ENABLE_DEBUG
              svz_log (SVZ_LOG_DEBUG, "http client detected\n");
#endif
              return -1;
            }
        }
    }

  return 0;
}

#define ENQ_COSERVER_REQUEST(req,coserver,cb)   \
  svz_coserver_ ## coserver ## _invoke          \
  (req, http_ ## cb,                            \
   svz_make_sock_iv (sock))

/*
 * When the http_detect_proto returns successfully this function must
 * be called to set all the appropriate callbacks and socket flags.
 */
int
http_connect_socket (svz_server_t *server, svz_socket_t *sock)
{
  http_socket_t *http;
  http_config_t *cfg = server->cfg;

  /*
   * initialize the http socket structure
   */
  http = svz_calloc (sizeof (http_socket_t));
  svz_invalidate_handle (&http->pid);
  http->keepalive = cfg->keepalive;
  sock->data = http;

  /* start reverse dns lookup for logging purposes if necessary */
  if (cfg->nslookup)
    {
      ENQ_COSERVER_REQUEST (sock->remote_addr, rdns, remotehost);
    }
  /* start user identification if necessary */
  if (cfg->ident)
    {
      ENQ_COSERVER_REQUEST (sock, ident, identification);
    }

  /*
   * set the socket flag, disable flood protection and
   * set all the callback routines
   */
  sock->flags |= SVZ_SOFLG_NOFLOOD;
  sock->check_request = http_check_request;
  sock->write_socket = http_default_write;
  sock->disconnected_socket = http_disconnect;
  sock->idle_func = http_cgi_died;
  sock->idle_counter = 1;

  return 0;
}

/*
 * This routine is called from http_check_request if there was
 * seen a full HTTP request (ends with a double CRLF).
 */
int
http_handle_request (svz_socket_t *sock, int len)
{
  http_socket_t *http = sock->data;
  int n;
  char *p, *line, *end;
  char *request;
  char *uri;
  int flag;
  int version[2];

  line = sock->recv_buffer;
  end = sock->recv_buffer + len;
  p = line;
  flag = 0;

  /* scan the request type */
  while (*p != ' ' && p < end - 1)
    p++;
  if (p == end || *(p + 1) != '/')
    {
      return -1;
    }
  *p = 0;
  request = svz_malloc (p - line + 1);
  strcpy (request, line);
  line = p + 1;

  /* scan the URI (file), `line' points to first character */
  while (*p != '\r' && p < end)
    p++;
  if (p == end)
    {
      svz_free (request);
      return -1;
    }

  /* scan back until beginning of HTTP version */
  while (*p != ' ' && *p)
    p--;

  /* is this a HTTP/0.9 request?  */
  if (!memcmp (request, "GET", 3) && memcmp (p + 1, "HTTP/", 5))
    {
      flag |= HTTP_FLAG_SIMPLE;
      while (*p != '\r')
        p++;
      uri = svz_malloc (p - line + 1);
      strncpy (uri, line, p - line);
      uri[p - line] = 0;
      line = p;
      version[MAJOR_VERSION] = 0;
      version[MINOR_VERSION] = 9;
    }
  /* no, it is a real HTTP/1.x request */
  else
    {
      if (p <= line)
        {
          svz_free (request);
          return -1;
        }
      *p = 0;
      uri = svz_malloc (p - line + 1);
      strcpy (uri, line);
      line = p + 1;

      /* scan the version string of the HTTP request */
      if (memcmp (line, "HTTP/", 5))
        {
          svz_free (request);
          svz_free (uri);
          return -1;
        }
      line += 5;
      version[MAJOR_VERSION] = *line - '0';
      line += 2;
      version[MINOR_VERSION] = *line - '0';
      line++;
    }

  /* check the remaining part of the first line the version */
  if (((version[MAJOR_VERSION] != HTTP_MAJOR_VERSION ||
        version[MINOR_VERSION] > 1 || *(line - 2) != '.') && !flag) ||
      !EOL1_P (line))
    {
      svz_free (request);
      svz_free (uri);
      return -1;
    }
  line += 2;

  /* find out properties */
  http_parse_property (sock, line, end);

  /* convert URI if necessary */
  http_process_uri (uri);

  /* assign request properties to http structure */
  http->timestamp = time (NULL);
  http->request = svz_malloc (strlen (request) + strlen (uri) + 11);
  sprintf (http->request, "%s %s HTTP/%d.%d",
           request, uri, version[MAJOR_VERSION], version[MINOR_VERSION]);

  /* find an appropriate request callback */
  for (n = 0; n < HTTP_REQUESTS; n++)
    {
      if (!memcmp (request, http_request[n].ident, http_request[n].len))
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "http: %s received\n", request);
#endif
          http_request[n].response (sock, uri, flag);
          break;
        }
    }

  /* Return a "404 Bad Request" if the request type is unknown.  */
  if (n == HTTP_REQUESTS)
    {
      http_default_response (sock, uri, 0);
    }

  svz_free (request);
  svz_free (uri);
  return 0;
}

/*
 * Check in the receive buffer of socket SOCK for full
 * http request and call http_handle_request if necessary.
 */
int
http_check_request (svz_socket_t *sock)
{
  char *p;
  int len;

  p = sock->recv_buffer;

  while (p < sock->recv_buffer + sock->recv_buffer_fill - 3 &&
         !EOL2_P (p))
    p++;

  if (EOL2_P (p) &&
      p < sock->recv_buffer + sock->recv_buffer_fill - 3)
    {
      len = p - sock->recv_buffer + 4;
      if (http_handle_request (sock, len))
        return -1;

      if (sock->recv_buffer_fill > len)
        {
          memmove (sock->recv_buffer, sock->recv_buffer + len,
                   sock->recv_buffer_fill - len);
        }
      sock->recv_buffer_fill -= len;
    }

  return 0;
}

/*
 * Server info callback for the http protocol.  We are currently using
 * it for displaying the server configuration within the control protocol.
 */
char *
http_info_server (svz_server_t *server)
{
  http_config_t *cfg = server->cfg;
  char bindings[256];
  static char info[80 * 12];

  svz_pp_server_bindings (bindings, 256, server);
  sprintf (info,
           " tcp bindings    : %s\r\n"
           " index file      : %s\r\n"
           " document root   : %s/\r\n"
           " cgi url         : %s/\r\n"
           " cgi directory   : %s/\r\n"
           " cache file size : %d byte\r\n"
           " cache entries   : %d files\r\n"
           " timeout         : after %d secs\r\n"
           " keep alive      : for %d requests\r\n"
           " default type    : %s\r\n"
           " type file       : %s\r\n"
           " content types   : %zu",
           bindings,
           cfg->indexfile,
           cfg->docs,
           cfg->cgiurl,
           cfg->cgidir,
           cfg->cachesize,
           cfg->cacheentries,
           cfg->timeout,
           cfg->keepalive,
           cfg->default_type,
           cfg->type_file,
           svz_hash_size (cfg->types));

  return info;
}

/*
 * Client info callback for the http protocol.
 */
char *
http_info_client (UNUSED svz_server_t *server, svz_socket_t *sock)
{
  http_socket_t *http = sock->data;
  http_cache_t *cache = http->cache;
  static char info[80 * 32], text[80 * 8];
  int n;

  sprintf (info, "This is a http client connection.\r\n\r\n");
#if ENABLE_SENDFILE
#if defined (HAVE_SENDFILE) || defined (__MINGW32__)
  if (sock->userflags & HTTP_FLAG_SENDFILE)
    {
      sprintf (text, "  * delivering via sendfile(2) (offset: %lu)\r\n",
               (unsigned long) http->fileoffset);
      strcat (info, text);
    }
#endif /* HAVE_SENDFILE || __MINGW32__ */
#endif /* ENABLE_SENDFILE */
  if (sock->userflags & HTTP_FLAG_KEEP)
    {
      sprintf (text,
               "  * keeping connection alive, "
               "%d requests and %d secs left\r\n",
               http->keepalive, sock->idle_counter);
      strcat (info, text);
    }
  if (sock->userflags & HTTP_FLAG_CACHE)
    {
      sprintf (text,
               "  * sending cache entry\r\n"
               "    file    : %s\r\n"
               "    size    : %d of %d bytes sent\r\n"
               "    usage   : %d\r\n"
               "    hits    : %d\r\n"
               "    urgency : %d\r\n"
               "    ready   : %s\r\n"
               "    date    : %s\r\n",
               cache->entry->file,
               cache->entry->size - cache->size, cache->entry->size,
               cache->entry->usage,
               cache->entry->hits,
               http_cache_urgency (cache->entry) + 1,
               cache->entry->ready ? "yes" : "no",
               http_asc_date (cache->entry->date));
      strcat (info, text);
    }
  if (sock->userflags & HTTP_FLAG_CGI)
    {
      sprintf (text, "  * sending cgi output (pid: %d)\r\n", (int) http->pid);
      strcat (info, text);
    }
  if (sock->userflags & HTTP_FLAG_POST)
    {
      sprintf (text,
               "  * receiving cgi input\r\n"
               "    pid            : %d\r\n"
               "    content-length : %d bytes left\r\n",
               (int) http->pid, http->contentlength);
      strcat (info, text);
    }
  sprintf (text, "  * %d bytes left of original file size\r\n",
           http->filelength);
  strcat (info, text);

  /* append http header properties is possible */
  if (http->property)
    {
      strcat (info, "  * request property list:\r\n");
      n = 0;
      while (http->property[n])
        {
          sprintf (text, "    %s => %s\r\n",
                   http->property[n], http->property[n + 1]);
          n += 2;
          strcat (info, text);
        }
    }

  return info;
}

/*
 * Respond to a http GET request.  This could be either a usual file
 * request or a CGI request.
 */
int
http_get_response (svz_socket_t *sock, char *request, int flags)
{
  int fd;
  int size, status;
  struct stat buf;
  char *dir, *host, *p, *file;
  time_t date;
  http_cache_t *cache;
  http_socket_t *http = sock->data;
  http_config_t *cfg = sock->cfg;

  /* reset current http header */
  http_reset_header ();

  /* check if this is a cgi request */
  if ((status = http_cgi_get_response (sock, request, 0)) != -2)
    return status;

  /* check for "~user" syntax here */
  if ((p = http_userdir (sock, request)) != NULL)
    {
      size = strlen (p) + strlen (cfg->indexfile) + 1;
      file = svz_malloc (size);
      strcpy (file, p);
      svz_free (p);
      status = 1;
    }
  /* this is a usual file request */
  else
    {
      size =
        strlen (cfg->docs) + strlen (request) + strlen (cfg->indexfile) + 4;

      file = svz_malloc (size);
      strcpy (file, cfg->docs);
      strcat (file, request);
      status = 0;
    }

  /* concate the IndexFile if necessary */
  if (file[strlen (file) - 1] == '/')
    {
      p = file + strlen (file);
      strcat (file, cfg->indexfile);

      /* get directory listing if there is no index file */
      if ((fd = open (file, O_RDONLY)) == -1)
        {
          *p = '\0';
          if ((dir = http_dirlist (file, cfg->docs,
                                   status ? request : NULL)) == NULL)
            {
              svz_log_sys_error ("http: dirlist: %s", file);
              svz_sock_printf (sock, HTTP_FILE_NOT_FOUND "\r\n");
              http_error_response (sock, 404);
              sock->userflags |= HTTP_FLAG_DONE;
              svz_free (file);
              return -1;
            }
          /* send the directory listing */
          http->response = 200;
          http->length = strlen (dir);
          svz_free (sock->send_buffer);
          sock->send_buffer = dir;
          sock->send_buffer_size = http_dirlist_size;
          sock->send_buffer_fill = strlen (dir);
          sock->userflags |= HTTP_FLAG_DONE;
          svz_free (file);
          return 0;
        }
      close (fd);
    }

  /* check if there are '..' in the requested file's path */
  if (strstr (request, "..") != NULL)
    {
      svz_sock_printf (sock, HTTP_ACCESS_DENIED "\r\n");
      http_error_response (sock, 403);
      sock->userflags |= HTTP_FLAG_DONE;
      svz_free (file);
      return -1;
    }

  /* get length of file and other properties */
  if (stat (file, &buf) == -1)
    {
      svz_log_sys_error ("stat (%s)", file);
      svz_sock_printf (sock, HTTP_FILE_NOT_FOUND "\r\n");
      http_error_response (sock, 404);
      sock->userflags |= HTTP_FLAG_DONE;
      svz_free (file);
      return -1;
    }

  /* make sure we do not send any devices or strange files */
  if (!(S_ISREG (buf.st_mode) ||
#ifdef S_ISLNK
        S_ISLNK (buf.st_mode) ||
#endif /* S_ISLNK */
        S_ISDIR (buf.st_mode)))
    {
      svz_log (SVZ_LOG_ERROR, "http: %s is not a regular file\n", file);
      svz_sock_printf (sock, HTTP_ACCESS_DENIED "\r\n");
      http_error_response (sock, 403);
      sock->userflags |= HTTP_FLAG_DONE;
      svz_free (file);
      return -1;
    }

  /* if directory then relocate to it */
  if (S_ISDIR (buf.st_mode))
    {
      host = http_find_property (http, "Host");
      http->response = 302;
      http_set_header (HTTP_RELOCATE);
      http_add_header ("Location: %s%s%s/\r\n",
                       host ? "http://" : "", host ? host : "", request);
      http_send_header (sock);
      sock->userflags |= HTTP_FLAG_DONE;
      svz_free (file);
      return 0;
    }

  /* open the file for reading */
  if ((fd = svz_open (file, O_RDONLY | O_BINARY, 0)) == -1)
    {
      svz_sock_printf (sock, HTTP_FILE_NOT_FOUND "\r\n");
      http_error_response (sock, 404);
      sock->userflags |= HTTP_FLAG_DONE;
      svz_free (file);
      return -1;
    }

  /* check if this it could be a Keep-Alive connection */
  if ((p = http_find_property (http, "Connection")) != NULL)
    {
      if (strstr (p, "Keep-Alive"))
        {
          sock->userflags |= HTTP_FLAG_KEEP;
        }
    }

  /* check if this a If-Modified-Since request */
  if ((p = http_find_property (http, "If-Modified-Since")) != NULL)
    {
      date = http_parse_date (p);
      if (date >= buf.st_mtime)
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "http: %s not changed\n", file);
#endif
          http->response = 304;
          http_set_header (HTTP_NOT_MODIFIED);
          http_check_keepalive (sock);
          http_send_header (sock);
          svz_close (fd);
          sock->userflags |= HTTP_FLAG_DONE;
          svz_free (file);
          return 0;
        }
    }

  /* check content range requests */
  if ((p = http_find_property (http, "Range")) != NULL)
    {
      if (http_get_range (p, &http->range) != -1)
        flags |= HTTP_FLAG_PARTIAL;
    }
  else if ((p = http_find_property (http, "Request-Range")) != NULL)
    {
      if (http_get_range (p, &http->range) != -1)
        flags |= HTTP_FLAG_PARTIAL;
    }

  /* check if partial content can be delivered or not */
  if (flags & HTTP_FLAG_PARTIAL)
    {
      if (http_check_range (&http->range, buf.st_size))
        flags &= ~HTTP_FLAG_PARTIAL;
      else
        {
          /* recheck content range */
          http->range.length = buf.st_size;
          if (http->range.last == 0)
            http->range.last = http->range.length - 1;

#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "http: partial content: %ld-%ld/%ld\n",
                   http->range.first, http->range.last, http->range.length);
#endif

          /* setup file descriptor and size */
          buf.st_size = http->range.last - http->range.first + 1;
          if (lseek (fd, http->range.first, SEEK_SET) != http->range.first)
            {
              svz_log_sys_error ("http: lseek");
              flags &= ~HTTP_FLAG_PARTIAL;
            }
        }

      /* return an error reponse if necessary */
      if (!(flags & HTTP_FLAG_PARTIAL))
        {
          svz_sock_printf (sock, HTTP_INVALID_RANGE "\r\n");
          http_error_response (sock, 416);
          sock->userflags |= HTTP_FLAG_DONE;
          svz_close (fd);
          svz_free (file);
          return -1;
        }
    }

  /* send a http header to the client */
  if (!(flags & HTTP_FLAG_SIMPLE))
    {
      /* repond via partial or full content */
      if (flags & HTTP_FLAG_PARTIAL)
        {
          http->response = 206;
          http_set_header (HTTP_PARTIAL);
        }
      else
        {
          http->response = 200;
          http_set_header (HTTP_OK);
        }

      http_add_header ("Content-Type: %s\r\n",
                       http_find_content_type (sock, file));

      /* set content range if possible */
      if (flags & HTTP_FLAG_PARTIAL)
        {
          http_add_header ("Content-Length: %ld\r\n",
                           http->range.last - http->range.first + 1);
          http_add_header ("Content-Range: bytes %ld-%ld/%ld\r\n",
                           http->range.first, http->range.last,
                           http->range.length);
        }
      else if (buf.st_size > 0)
        http_add_header ("Content-Length: %ld\r\n", buf.st_size);

      http_add_header ("Last-Modified: %s\r\n", http_asc_date (buf.st_mtime));
      http_add_header ("Accept-Ranges: bytes\r\n");
      http_check_keepalive (sock);
      http_send_header (sock);
    }

  /* just a HEAD response handled by this GET handler */
  if (flags & HTTP_FLAG_NOFILE)
    {
      svz_close (fd);
      sock->userflags |= HTTP_FLAG_DONE;
      svz_free (file);
      return 0;
    }

  /* create a cache structure for the http socket structure */
  cache = svz_calloc (sizeof (http_cache_t));
  http->cache = cache;

  /* disable caching if delivering partial content */
  if (flags & HTTP_FLAG_PARTIAL)
    {
      status = HTTP_CACHE_INHIBIT;
    }
  else
    {
      /* return the file's current cache status */
      status = http_check_cache (file, cache);
    }

  /* is the requested file already fully in the cache?  */
  if (status == HTTP_CACHE_COMPLETE)
    {
      if (buf.st_mtime > cache->entry->date ||
          buf.st_size != cache->entry->size)
        {
          /* the file on disk has changed?  */
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "cache: %s has changed\n", file);
#endif
          http_refresh_cache (cache);
          cache->entry->date = buf.st_mtime;
          sock->flags |= SVZ_SOFLG_FILE;
          sock->file_desc = fd;
          http->filelength = buf.st_size;
          sock->read_socket = http_cache_read;
          sock->disconnected_socket = http_cache_disconnect;
        }
      else
        {
          /* no, initialize the cache routines */
          cache->entry->hits++;
          cache->entry->usage++;
          sock->userflags |= HTTP_FLAG_CACHE;
          if (flags & HTTP_FLAG_SIMPLE)
            {
              sock->send_buffer_fill = 42;
              sock->write_socket = http_cache_write;
            }
          svz_close (fd);
        }
    }
  /* the file is not in the cache structures yet */
  else
    {
      sock->file_desc = fd;
      http->filelength = buf.st_size;
      sock->flags |= SVZ_SOFLG_FILE;

      /*
       * find a free slot for the new file if it is not larger
       * than a certain size and is not "partly" in the cache
       */
      if (status == HTTP_CACHE_NO &&
          buf.st_size > 0 && buf.st_size < cfg->cachesize &&
          http_init_cache (file, cache) != -1)
        {
          sock->read_socket = http_cache_read;
          sock->disconnected_socket = http_cache_disconnect;
          cache->entry->date = buf.st_mtime;
        }
      /*
       * either the file is not cacheable or it is currently
       * going to be in the http cache (not yet cache->ready)
       */
      else
        {
#if ENABLE_SENDFILE && (HAVE_SENDFILE || defined (__MINGW32__))
# ifdef __MINGW32__
          if (svz_mingw_at_least_nt4_p ())
            {
              sock->read_socket = NULL;
              sock->flags &= ~SVZ_SOFLG_FILE;
              sock->userflags |= HTTP_FLAG_SENDFILE;
            }
          else
            sock->read_socket = http_file_read;
# else
          sock->read_socket = NULL;
          sock->flags &= ~SVZ_SOFLG_FILE;
          sock->userflags |= HTTP_FLAG_SENDFILE;
          svz_tcp_cork (sock->sock_desc, 1);
# endif
#else /* not HAVE_SENDFILE */
          sock->read_socket = http_file_read;
#endif /* HAVE_SENDFILE || __MINGW32__ && ENABLE_SENDFILE */
        }
    }

  svz_free (file);
  return 0;
}

/*
 * Respond to a http HEAD request.  This is in particular the same as
 * GET but you do not respond with the file but with the header and all
 * the file info.
 */
int
http_head_response (svz_socket_t *sock, char *request, int flags)
{
  http_get_response (sock, request, flags | HTTP_FLAG_NOFILE);
  return 0;
}

/*
 * The default http response (Bad Request).
 */
int
http_default_response (svz_socket_t *sock,
                       UNUSED char *request, UNUSED int flags)
{
  svz_sock_printf (sock, HTTP_NOT_IMPLEMENTED "\r\n");
  http_error_response (sock, 501);
  sock->userflags |= HTTP_FLAG_DONE;
  return 0;
}
