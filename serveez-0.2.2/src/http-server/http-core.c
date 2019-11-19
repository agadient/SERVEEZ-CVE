/*
 * http-core.c - http core functionality
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
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#if HAVE_PWD_H && !defined (__MINGW32__)
# include <pwd.h>
#endif
#include "changedir.h"
#include "networking-headers.h"

#ifdef __MINGW32__
# include <io.h>
# include <lm.h>
# include <lmerr.h>
#endif

#include "cpp-tricks.h"

#include "libserveez.h"
#include "http-proto.h"
#include "http-core.h"

/* the current http header structure */
http_header_t http_header;

/*
 * In Win32 OS's both of these defines are necessary for portability.
 */
#if defined (__CYGWIN__) || defined (__MINGW32__)
# define timezone _timezone
# ifndef daylight
#  define daylight _daylight
# endif

#elif !HAVE_DECL_TIMEZONE || !HAVE_DECL_DAYLIGHT
/*
 * For some reason FreeBSD 3.2 does not provide `timezone' and `daylight'.
 */
# define timezone ((long int) 0)
# define daylight ((int) 0)
#endif

#ifdef __MINGW32__

/*
 * Handle and function definitions for the NetApi interface.
 */
typedef NET_API_STATUS (__stdcall *GetUserInfoProc) (WCHAR *, WCHAR *,
                                                     DWORD, LPBYTE *);
typedef NET_API_STATUS (__stdcall *FreeUserInfoProc) (void *);
static FreeUserInfoProc FreeUserInfo = NULL;
static GetUserInfoProc GetUserInfo = NULL;
static HMODULE netapiHandle = NULL;

/*
 * Unload the `netapi32.dll'.
 */
void
http_stop_netapi (void)
{
  if (netapiHandle)
    {
      FreeLibrary (netapiHandle);
      netapiHandle = NULL;
    }
}

/*
 * Load `netapi32.dll' and get function pointers necessary to obtain
 * a user's home directory.
 */
void
http_start_netapi (void)
{
  if ((netapiHandle = LoadLibrary ("netapi32.dll")) != NULL)
    {
      FreeUserInfo = (FreeUserInfoProc)
        GetProcAddress (netapiHandle, "NetApiBufferFree");
      GetUserInfo = (GetUserInfoProc)
        GetProcAddress (netapiHandle, "NetUserGetInfo");
      if (GetUserInfo == NULL)
        {
          svz_log_sys_error ("http: GetProcAddress");
          FreeLibrary (netapiHandle);
          netapiHandle = NULL;
        }
    }
}
#endif /* not __MINGW32__ */

/*
 * If the given request has a leading `~' we try to get the appropriate
 * user's home directory and put it in front of the request.
 */
char *
http_userdir (svz_socket_t *sock, char *uri)
{
  http_config_t *cfg = sock->cfg;
  char *p = uri + 1;
  char *user, *file;
#if HAVE_GETPWNAM
  struct passwd *entry;
#elif defined (__MINGW32__)
  USER_INFO_1 *entry = NULL;
  NET_API_STATUS status;
#endif

  if (*uri && *p++ == '~' && cfg->userdir)
    {
      /* parse onto end of user name */
      while (*p && *p != '/')
        p++;
      if (p - uri <= 2)
        return NULL;

      user = svz_malloc (p - uri - 1);
      memcpy (user, uri + 2, p - uri - 2);
      user[p - uri - 2] = '\0';

#if HAVE_GETPWNAM
      if ((entry = getpwnam (user)) != NULL)
        {
          file = svz_malloc (strlen (entry->pw_dir) + strlen (cfg->userdir) +
                             strlen (p) + 2);
          sprintf (file, "%s/%s%s", entry->pw_dir, cfg->userdir, p);
          svz_free (user);
          return file;
        }
#elif defined (__MINGW32__)
      if (GetUserInfo == NULL)
        {
          svz_free (user);
          return NULL;
        }

      status = GetUserInfo (NULL,                       /* server name */
                            svz_windoze_asc2uni (user), /* user name */
                            1,                          /* type of info */
                            (LPBYTE *) &entry);         /* info buffer */

      if (status != NERR_Success)
        {
          char *error;
          switch (status)
            {
            case ERROR_ACCESS_DENIED:
              error = "The user does not have access to the requested "
                "information.";
              break;
            case NERR_InvalidComputer:
              error = "The computer name is invalid.";
              break;
            case NERR_UserNotFound:
              error = "The user name could not be found.";
              break;
            default:
              error = "Unknown error.";
              break;
            }
          svz_log (SVZ_LOG_ERROR, "NetUserGetInfo: %s\n", error);
        }
      /* successfully got the user information?  */
      else if (entry && entry->usri1_home_dir && entry->usri1_home_dir[0])
        {
          file =
            svz_malloc (strlen (svz_windoze_uni2asc (entry->usri1_home_dir)) +
                        strlen (cfg->userdir) + strlen (p) + 2);
          sprintf (file, "%s/%s%s",
                   svz_windoze_uni2asc (entry->usri1_home_dir),
                   cfg->userdir, p);
          FreeUserInfo (entry);
          svz_free (user);
          return file;
        }
#if ENABLE_DEBUG
      else if (entry)
        {
          svz_log (SVZ_LOG_DEBUG, "http: home directory for %s not set\n",
                   svz_windoze_uni2asc (entry->usri1_name));
        }
#endif /* ENABLE_DEBUG */
#endif /* not HAVE_GETPWNAM and not __MINGW32__ */

      svz_free (user);
    }
  return NULL;
}

/*
 * When the http server has been configured to invoke identd request
 * this function is called for each client connection after successful
 * identification.
 */
int
http_identification (char *ident, void *closure)
{
  http_socket_t *http;
  svz_sock_iv_t *x = closure;
  svz_socket_t *sock = svz_sock_find (x->id, x->version);

  svz_free (x);
  if (ident && sock)
    {
      http = sock->data;
      if (!http->ident)
        http->ident = svz_strdup (ident);
    }

  return 0;
}

/*
 * Each http client gets resolved by this callback.
 */
int
http_remotehost (char *host, void *closure)
{
  http_socket_t *http;
  svz_sock_iv_t *x = closure;
  svz_socket_t *sock = svz_sock_find (x->id, x->version);

  svz_free (x);
  if (host && sock)
    {
      http = sock->data;
      if (!http->host)
        http->host = svz_strdup (host);
    }

  return 0;
}

/*
 * When the localhost has been resolved to a hostname this callback is
 * invoked by the main loop.  Put the result into the http configuration.
 */
int
http_localhost (char *host, void *closure)
{
  http_config_t *cfg = closure;

  if (host && !cfg->host)
    {
      cfg->host = svz_strdup (host);
    }
  return 0;
}

/*
 * Write a logging notification to the access logfile if possible
 * and necessary.
 */
void
http_log (svz_socket_t *sock)
{
  http_config_t *cfg = sock->cfg;
  http_socket_t *http = sock->data;
  char buf[64];
  static char line[1024];
  char *referrer, *agent, *p, *start;

  if (cfg->log && http->request)
    {
      referrer = http_find_property (http, "Referer");
      agent = http_find_property (http, "User-Agent");

      /* access logging format given?  */
      if (cfg->logformat && *cfg->logformat)
        start = cfg->logformat;
      else
        start = HTTP_CLF;

      memset (line, 0, sizeof (line));
      while (*start)
        {
          /* parse until next format character */
          p = start;
          while (*p && *p != '%')
            p++;
          strncat (line, start, p - start);
          if (!*p)
            break;
          p++;
          switch (*p)
            {
              /* %i - identity information */
            case 'i':
              strcat (line, http->ident ? http->ident : "-");
              p++;
              break;
              /* %u - user authentication */
            case 'u':
              strcat (line, http->auth ? http->auth : "-");
              p++;
              break;
              /* %l - delivered content length */
            case 'l':
              strcat (line, svz_itoa (http->length));
              p++;
              break;
              /* %c - http response code */
            case 'c':
              strcat (line, svz_itoa (http->response));
              p++;
              break;
              /* %h - host name */
            case 'h':
              strcat (line, http->host ? http->host :
                      SVZ_PP_ADDR (buf, sock->remote_addr));
              p++;
              break;
              /* %t - request time stamp */
            case 't':
              strcat (line, http_clf_date (http->timestamp));
              p++;
              break;
              /* end of string */
            case '\0':
              break;
              /* %R - original http request uri */
            case 'R':
              strcat (line, http->request ? http->request : "-");
              p++;
              break;
              /* %r - referrer document */
            case 'r':
              strcat (line, referrer ? referrer : "-");
              p++;
              break;
              /* %a - user agent */
            case 'a':
              strcat (line, agent ? agent : "-");
              p++;
              break;
            default:
              p++;
              break;
            }
          start = p;
        }
      strcat (line, "\n");

      if (!ferror (cfg->log) && !feof (cfg->log))
        {
          fprintf (cfg->log, line);
          fflush (cfg->log);
        }
      else
        {
          svz_log (SVZ_LOG_ERROR, "http: access logfile died\n");
          svz_fclose (cfg->log);
          cfg->log = NULL;
        }
    }
}

/*
 * Reset the current http header structure.
 */
void
http_reset_header (void)
{
  http_header.code = 0;
  http_header.field[0] = '\0';
  http_header.response = NULL;
}

/*
 * Add a response header field to the current header.
 */
void
http_add_header (const char *fmt, ...)
{
  va_list args;
  int len = strlen (http_header.field);
  char *p = http_header.field + len;

  if (len >= HTTP_HEADER_SIZE)
    return;
  va_start (args, fmt);
  vsnprintf (p, HTTP_HEADER_SIZE - len, fmt, args);
  va_end (args);
}

/*
 * Send the http header.
 */
int
http_send_header (svz_socket_t *sock)
{
  int ret = 0;

  /* send first part of header including response field and static texts */
  ret = svz_sock_printf (sock,
                         "%s"
                         "Date: %s\r\n"
                         "Server: %s\r\n",
                         http_header.response,
                         http_asc_date (time (NULL)),
                         SERVER_STRING);
  if (ret)
    return ret;

  /* send header fields and trailing line break */
  ret = svz_sock_printf (sock, "%s\r\n", http_header.field);
  if (ret)
    return ret;

  return 0;
}

/*
 * Set the current http header response.
 */
void
http_set_header (char *response)
{
  http_header.response = response;
}

/*
 * Check if the a content range is valid for the given file size and return
 * zero on success, non-zero otherwise.
 */
int
http_check_range (http_range_t *range, off_t filesize)
{
  if ((range->first <= 0) ||
      (range->last != 0 && range->last <= range->first) ||
      (range->last >= filesize || range->length > filesize))
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG,
               "http: invalid content range (%ld-%ld/%ld not in %ld) \n",
               range->first, range->last, range->length, filesize);
#endif
      return -1;
    }
  return 0;
}

#define HTTP_BYTES        "bytes"
#define HTTP_BYTES_LENGTH 5

/*
 * Create a http content range if the given line specifies a valid one.
 * Return zero on success and -1 on errors.
 */
int
http_get_range (char *line, http_range_t *range)
{
  char *p = line;
  off_t n;

  /* check args */
  if (!line || !range)
    return -1;

  /* check identifier */
  if (strlen (p) >= HTTP_BYTES_LENGTH &&
      memcmp (p, HTTP_BYTES, HTTP_BYTES_LENGTH))
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "http: invalid byte-range specifier (%s)\n", p);
#endif
      return -1;
    }
  p += HTTP_BYTES_LENGTH;

  /* parse content range itself */
  range->first = range->last = range->length = 0;

  if (*p != '=')
    return 0;
  p++;
  n = 0;
  while (*p >= '0' && *p <= '9')
    {
      n *= 10;
      n += (*p - '0');
      p++;
    }
  range->first = n;

  if (*p != '-')
    return 0;
  p++;
  n = 0;
  while (*p >= '0' && *p <= '9')
    {
      n *= 10;
      n += (*p - '0');
      p++;
    }
  range->last = n;

  if (*p != '/')
    return 0;
  p++;
  n = 0;
  while (*p >= '0' && *p <= '9')
    {
      n *= 10;
      n += (*p - '0');
      p++;
    }
  range->length = n;

  return 0;
}

/*
 * Send an error message response body to the http client connection.
 * This is not actually necessary, because an appropriate response header
 * should work out fine.  But most browsers indicate "document contained
 * not data." if this occurs.
 */
int
http_error_response (svz_socket_t *sock, int response)
{
  http_config_t *cfg = sock->cfg;
  http_socket_t *http = sock->data;
  char *txt;
  char buf[64];

  /* Convert error code to text.  */
  switch (response)
    {
    case 400:
      txt = "Bad Request";
      break;
    case 401:
      txt = "Unauthorized";
      break;
    case 402:
      txt = "Payment Required";
      break;
    case 403:
      txt = "Forbidden";
      break;
    case 404:
      txt = "Not Found";
      break;
    case 405:
      txt = "Method Not Allowed";
      break;
    case 406:
      txt = "Not Acceptable";
      break;
    case 407:
      txt = "Proxy Authentication Required";
      break;
    case 408:
      txt = "Request Timeout";
      break;
    case 409:
      txt = "Conflict";
      break;
    case 410:
      txt = "Gone";
      break;
    case 411:
      txt = "Length Required";
      break;
    case 412:
      txt = "Precondition Failed";
      break;
    case 413:
      txt = "Request Entity Too Large";
      break;
    case 414:
      txt = "Request-URI Too Long";
      break;
    case 415:
      txt = "Unsupported Media Type";
      break;
    case 416:
      txt = "Requested Range Not Satisfiable";
      break;
    case 417:
      txt = "Expectation Failed";
      break;
    case 500:
      txt = "Internal Server Error";
      break;
    case 501:
      txt = "Not Implemented";
      break;
    case 502:
      txt = "Bad Gateway";
      break;
    case 503:
      txt = "Service Unavailable";
      break;
    case 504:
      txt = "Gateway Timeout";
      break;
    case 505:
      txt = "HTTP Version Not Supported";
      break;
    default:
      txt = "Bad Request";
    }
  http->response = response;

  /* Send some standard error message.  */
  return svz_sock_printf (sock,
                          "<html><body bgcolor=white text=black><br>"
                          "<h1>%d %s</h1>"
                          "<hr noshade><i>%s server at %s port %d, "
                          "please send email to <a href=\"mailto:%s\">%s</a> "
                          "for reporting errors</i>"
                          "</body></html>",
                          response, txt,
                          SERVER_STRING,
                          cfg->host ? cfg->host :
                          SVZ_PP_ADDR (buf, sock->local_addr),
                          ntohs (sock->local_port), cfg->admin, cfg->admin);
}

/*
 * This function is used to re-initialize a HTTP connection for
 * Keep-Alive connections.  Return -1 if it is not 'Keep'able.
 */
int
http_keep_alive (svz_socket_t *sock)
{
  if (sock->userflags & HTTP_FLAG_KEEP)
    {
      http_free_socket (sock);

      sock->userflags &= ~HTTP_FLAG;
      sock->read_socket = svz_tcp_read_socket;
      sock->check_request = http_check_request;
      sock->write_socket = http_default_write;
      sock->send_buffer_fill = 0;
      sock->idle_func = http_idle;
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "http: keeping connection alive\n");
#endif
      return 0;
    }
  return -1;
}

/*
 * This function is used to check if the connection in SOCK is a
 * Keep-Alive connection and sends the appropriate HTTP header property.
 */
void
http_check_keepalive (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;
  http_config_t *cfg = sock->cfg;

  if ((sock->userflags & HTTP_FLAG_KEEP) && http->keepalive > 0)
    {
      sock->idle_counter = cfg->timeout;
      http_add_header ("Connection: Keep-Alive\r\n");
      http_add_header ("Keep-Alive: timeout=%d, max=%d\r\n",
                       sock->idle_counter, cfg->keepalive);
      http->keepalive--;
    }
  /* tell HTTP/1.1 clients that the connection is closed after delivery */
  else
    {
      sock->userflags &= ~HTTP_FLAG_KEEP;
      http_add_header ("Connection: close\r\n");
    }
}

/*
 * Create a date format used within the Common Log Format.  That is as
 * follows: [DD/MMM/YYYY:HH:MM:SS +ZZZZ]
 */
char *
http_clf_date (time_t t)
{
  static char date[64];
  static char months[12][4] = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  };
  struct tm *tm;

  tm = localtime (&t);
  sprintf (date, "%02d/%s/%04d:%02d:%02d:%02d %c%02ld%02ld",
           tm->tm_mday, months[tm->tm_mon], tm->tm_year + 1900,
           tm->tm_hour, tm->tm_min, tm->tm_sec,
           timezone > 0 ? '+' : '-',
           timezone > 0 ? timezone / 3600 : -timezone / 3600,
           timezone > 0 ? (timezone / 60) % 60 : -(timezone / 60) % 60);
  return date;
}

/*
 * Produce a ASCTIME date without the trailing '\n' from a given time_t.
 */
char *
http_asc_date (time_t t)
{
  static char asc[64];
  struct tm * gm_time;

  gm_time = gmtime (&t);
  strftime (asc, 64, "%a, %d %b %Y %H:%M:%S GMT", gm_time);

  return asc;
}

#define MAX_WKDAY_SIZE  9

/*
 * Extract a date information from a given string and return a
 * UTC time (time_t) as ‘time’ does.
 */
time_t
http_parse_date (char *date)
{
  struct tm parse_time;
  int n;
  char _month[4];
  char _wkday[1 + MAX_WKDAY_SIZE];
  time_t ret;

  static char month[12][4] = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  };

  switch (date[3])
    {
      /* ASCTIME-Date */
    case ' ':
      sscanf (date, "%3s %3s %2d %02d:%02d:%02d %04d",
              _wkday, _month, &parse_time.tm_mday, &parse_time.tm_hour,
              &parse_time.tm_min, &parse_time.tm_sec, &parse_time.tm_year);

      break;
      /* RFC1123-Date */
    case ',':
      sscanf (date, "%3s, %02d %3s %04d %02d:%02d:%02d GMT",
              _wkday, &parse_time.tm_mday, _month, &parse_time.tm_year,
              &parse_time.tm_hour, &parse_time.tm_min, &parse_time.tm_sec);

      break;
      /* RFC850-Date */
    default:
      sscanf (date,
              PERCENT_N_S (MAX_WKDAY_SIZE)
              ", %02d-%3s-%02d %02d:%02d:%02d GMT",
              _wkday, &parse_time.tm_mday, _month, &parse_time.tm_year,
              &parse_time.tm_hour, &parse_time.tm_min, &parse_time.tm_sec);

      parse_time.tm_mon += parse_time.tm_mon >= 70 ? 1900 : 2000;

      break;
    }

  /* find the month identifier */
  for (n = 0; n < 12; n++)
    if (!memcmp (_month, month[n], 3))
      parse_time.tm_mon = n;

  parse_time.tm_isdst = daylight;
  parse_time.tm_year -= 1900;
  ret = mktime (&parse_time);
  ret -= timezone;
  if (daylight > 0)
    ret += 3600;
  return ret;
}

/*
 * Parse part of the receive buffer for HTTP request properties
 * and store it in the socket structure SOCK.  Return the amount of
 * properties found in the request.
 */
int
http_parse_property (svz_socket_t *sock, char *request, char *end)
{
  int properties, n;
  char *p;
  http_socket_t *http;

  /* get the http socket structure */
  http = sock->data;

  /* reserve data space for the http properties */
  http->property = svz_malloc (MAX_HTTP_PROPERTIES * 2 * sizeof (char *));
  properties = 0;
  n = 0;

  /* find out properties if necessary */
  while (!EOL1_P (request) && properties < MAX_HTTP_PROPERTIES - 1)
    {
      /* get property entity identifier */
      p = request;
      while (*p != ':' && p < end)
        p++;
      if (p == end)
        break;
      http->property[n] = svz_malloc (p - request + 1);
      strncpy (http->property[n], request, p - request);
      http->property[n][p - request] = 0;
      n++;
      request = p + 2;

      /* get property entity body */
      while (!EOL1_P (p) && p < end)
        p++;
      if (p == end || p <= request)
        break;
      http->property[n] = svz_malloc (p - request + 1);
      strncpy (http->property[n], request, p - request);
      http->property[n][p - request] = 0;
      n++;
      properties++;
      request = p + 2;

#if 0
      printf ("http header: {%s} = {%s}\n",
              http->property[n - 2], http->property[n - 1]);
#endif
    }

  request += 2;
  http->property[n] = NULL;

  return properties;
}

/*
 * Find a given property entity in the HTTP request properties.
 * Return a NULL pointer if not found.
 */
char *
http_find_property (http_socket_t *http, char *key)
{
  int n;
  size_t len;

  /* check if there are any properties */
  if (http->property == NULL)
    return NULL;

  /* search through all the http properties */
  n = 0;
  len = strlen (key);
  while (http->property[n])
    {
      if (!strncasecmp (http->property[n], key, len))
        {
          return http->property[n + 1];
        }
      n += 2;
    }
  return NULL;
}

#define ASC_TO_HEX(c)                             \
  if (c >= '0' && c <= '9') c -= '0';             \
  else if (c >= 'a' && c <= 'f') c -= ('a' - 10); \
  else if (c >= 'A' && c <= 'F') c -= ('A' - 10);

/*
 * Convert hexadecimal encoded characters within the URI.  This is
 * necessary for some special characters.  The URI is a Uniform Resource
 * Identifier meaning the requested file.
 */
void
http_process_uri (char *uri)
{
  char *p;
  char h, l;

  /* Test if there is any occurrence of the special character encoding.  */
  while ((p = strchr (uri, '%')) != NULL)
    {
      if ((h = *(p + 1)) != 0 && (l = *(p + 2)) != 0)
        {
          /* Convert to byte.  */
          ASC_TO_HEX (h);
          ASC_TO_HEX (l);
          *p = (char) ((h << 4) | l);

          /* Copy rest of URI.  */
          uri = ++p;
          while (*(p + 2))
            {
              *p = *(p + 2);
              p++;
            }
          *p = '\0';
        }
      else
        break;
    }
}

/*
 * This routine gets all available content types from a given
 * file which should have kind of "/etc/mime.types"s format.
 */
#define TYPES_LINE_SIZE 1024

int
http_read_types (http_config_t *cfg)
{
  FILE *f;
  char *line;
  char *p, *end;
  char *content;
  char *suffix;

  /* create the content type hash table if necessary */
  if (cfg->types == NULL)
    cfg->types = svz_hash_create (4, svz_free);

  /* try open the file */
  if ((f = svz_fopen (cfg->type_file, "rt")) == NULL)
    {
      return -1;
    }

  line = svz_malloc (TYPES_LINE_SIZE);

  /* read all lines within the file */
  while ((fgets (line, TYPES_LINE_SIZE, f)) != NULL)
    {
      /* delete all trailing newline characters, skip empty lines */
      p = line + strlen (line) - 1;
      while (p != line && (*p == '\r' || *p == '\n'))
        p--;
      if (p == line)
        continue;
      *(p + 1) = 0;

      p = line;
      end = line + strlen (line);

      /* parse content type */
      content = line;
      while (p < end && (*p != ' ' && *p != '\t'))
        p++;
      *p++ = 0;

      /* parse all file suffixes associated with this content type */
      while (p < end)
        {
          while (p < end && (*p == ' ' || *p == '\t'))
            p++;
          if (p == end)
            break;
          suffix = p;
          while (p < end && (*p != ' ' && *p != '\t'))
            p++;
          *p++ = 0;
          if (strlen (suffix))
            {
              /*
               * add the given content type to the hash if it does not
               * contain it already
               */
              if (!svz_hash_get (cfg->types, suffix))
                svz_hash_put (cfg->types, suffix, svz_strdup (content));
            }
        }
    }
  svz_fclose (f);
  svz_free (line);
  return 0;
}

/*
 * This routine delivers a valid content type for a given file.
 * It falls back to the socket's http configuration default content
 * type if the suffix could not be found.
 */
char *
http_find_content_type (svz_socket_t *sock, char *file)
{
  http_config_t *cfg = sock->cfg;
  char *suffix = file + strlen (file) - 1;
  char *type;

  /* parse file back until a trailing '.' */
  while (suffix > file && *suffix != '.')
    suffix--;
  if (suffix != file)
    suffix++;

  /* find this file suffix in the content type hash */
  if ((type = svz_hash_get (cfg->types, suffix)) != NULL)
    {
      return type;
    }

  return cfg->default_type;
}

/*
 * This routine converts a relative file/path name into an
 * absolute file/path name.  The given argument will be reallocated
 * if necessary.
 */
char *
http_absolute_file (char *file)
{
  char *savedir;
  char *p;
  char *savefile;
  char *dir;
  int have_path = 0;

  /* find any path separator in the file */
  p = file + strlen (file) - 1;
  while (p > file && *p != '/' && *p != '\\')
    p--;
  if (*p == '/' || *p == '\\')
    {
      have_path = 1;
      p++;
    }

  /* save the filename within a buffer */
  savefile = svz_strdup (p);

  /* get current work directory */
  savedir = svz_getcwd ();

  /*
   * If there was no path separator in the filename then just concate
   * current work directory and filename.
   */
  if (!have_path)
    {
      savedir = svz_realloc (savedir, strlen (savedir) + strlen (file) + 2);
      strcat (savedir, "/");
      strcat (savedir, savefile);
      svz_free (file);
      return savedir;
    }

  /* change to give directory (absolute or relative)  */
  *p = 0;
  if (chdir (file) == -1)
    {
      *p = '/';
      svz_log_sys_error ("chdir");
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "cannot change dir: %s\n", file);
#endif
      svz_free (savefile);
      svz_free (savedir);
      return file;
    }
  *p = '/';

  /* get now the current work directory */
  dir = svz_getcwd ();

  /* concate new work directory with given filename */
  dir = svz_realloc (dir, strlen (dir) + strlen (savefile) + 2);
  strcat (dir, "/");
  strcat (dir, savefile);
  svz_free (savefile);
  svz_free (file);

  /* change back to the original work directory */
  chdir (savedir);
  svz_free (savedir);
  return dir;
}
