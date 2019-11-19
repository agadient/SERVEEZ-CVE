/*
 * http-core.h - http core definitions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __HTTP_CORE_H__
#define __HTTP_CORE_H__

/* Some definitions.  */
#define HTTP_MAJOR_VERSION  1          /* accepted MajorVersion */
#define MAJOR_VERSION       0          /* MajorVersion index */
#define MINOR_VERSION       1          /* MinorVersion index */
#define MAX_HTTP_PROPERTIES 32         /* all http properties */
#define HTTP_REQUESTS       8          /* number of known request types */
#define HTTP_TIMEOUT        15         /* default timeout value */
#define HTTP_MAXKEEPALIVE   10         /* number of requests per connection */
#define HTTP_HEADER_SIZE    (1 * 1024) /* maximum header size */

#define STANDARD_EOL  "\r\n\r\n"
#define EOL1_P(p)     (!memcmp (STANDARD_EOL, (p), 2))
#define EOL2_P(p)     (!memcmp (STANDARD_EOL, (p), 4))

/* This is like ‘PACKAGE_STRING’, except delim is slash, not space,
   and the initial "GNU " is omitted, so that the result has the form
   TOKEN "/" TOKEN, per HTTP 1.0 (RFC 1945) and CGI 1.1 (RFC 3875).
   We could have used ‘PACKAGE_TARNAME’, but that's not as pretty.  */
#define SERVER_STRING   (4 + (PACKAGE_NAME "/" PACKAGE_VERSION))

/*
 * The following structure is meant to hold a http response headers
 * data.
 */
typedef struct
{
  char *response;               /* text representation of response */
  int code;                     /* response code */
  char field[HTTP_HEADER_SIZE]; /* holds header fields */
}
http_header_t;

/*
 * The content range structure defines a http content range for partial
 * entity content.
 */
typedef struct
{
  off_t first;  /* first byte in range */
  off_t last;   /* last byte (inclusive) */
  off_t length; /* total length of entity (can be zero "*") */
}
http_range_t;

/*
 * This structure is used to process a http connection.  It will be stored
 * within the original socket structure (sock->data).
 */
typedef struct http_socket http_socket_t;

struct http_socket
{
  http_cache_t *cache;   /* a http file cache structure */
  char **property;       /* property list of a http request */
  int contentlength;     /* the content length for the cgi pipe */
  int filelength;        /* content length for the http file */
  int keepalive;         /* how many requests left for a connection */
  off_t fileoffset;      /* file offset used by sendfile */
  svz_t_handle pid;      /* the pid of the cgi (process handle) */
  time_t timestamp;      /* connection access time */
  char *request;         /* the original request */
  char *host;            /* resolved host name of client */
  int response;          /* the server's response code */
  int length;            /* content length sent so far */
  char *ident;           /* identity information */
  char *auth;            /* user authentication */
  http_range_t range;    /* partial content range */
};

/* the current HTTP protocol version */
#define HTTP_VERSION "HTTP/1.0"

/* Common Log Format string */
#define HTTP_CLF "%h %i %u [%t] \"%R\" %c %l"

/* HTTP resonse header definitions */
#define HTTP_OK              HTTP_VERSION " 200 OK\r\n"
#define HTTP_ACCEPTED        HTTP_VERSION " 202 Accepted\r\n"
#define HTTP_PARTIAL         HTTP_VERSION " 206 Partial Content\r\n"
#define HTTP_RELOCATE        HTTP_VERSION " 302 Temporary Relocation\r\n"
#define HTTP_NOT_MODIFIED    HTTP_VERSION " 304 Not Modified\r\n"
#define HTTP_BAD_REQUEST     HTTP_VERSION " 400 Bad Request\r\n"
#define HTTP_ACCESS_DENIED   HTTP_VERSION " 403 Forbidden\r\n"
#define HTTP_FILE_NOT_FOUND  HTTP_VERSION " 404 Not Found\r\n"
#define HTTP_INVALID_RANGE   HTTP_VERSION " 416 Requested Range Not Satisfiable\r\n"
#define HTTP_INTERNAL_ERROR  HTTP_VERSION " 500 Internal Server Error\r\n"
#define HTTP_NOT_IMPLEMENTED HTTP_VERSION " 501 Not Implemented\r\n"

#define HTTP_FLAG_CACHE    0x0001 /* use cache if possible */
#define HTTP_FLAG_NOFILE   0x0002 /* do not send content, but header */
#define HTTP_FLAG_SIMPLE   0x0004 /* HTTP/0.9 simple GET */
#define HTTP_FLAG_DONE     0x0008 /* http request done */
#define HTTP_FLAG_POST     0x0010 /* http cgi pipe posting data */
#define HTTP_FLAG_CGI      0x0020 /* http cgi pipe getting data */
#define HTTP_FLAG_KEEP     0x0040 /* keep alive connection */
#define HTTP_FLAG_SENDFILE 0x0080 /* use sendfile for HTTP requests */
#define HTTP_FLAG_PARTIAL  0x0100 /* partial content requested */

/* all of the additional http flags */
#define HTTP_FLAG (HTTP_FLAG_DONE      | \
                   HTTP_FLAG_POST      | \
                   HTTP_FLAG_CGI       | \
                   HTTP_FLAG_CACHE     | \
                   HTTP_FLAG_KEEP      | \
                   HTTP_FLAG_SENDFILE  | \
                   HTTP_FLAG_PARTIAL)

/* exported http core functions */
int http_keep_alive (svz_socket_t *sock);
void http_check_keepalive (svz_socket_t *sock);

int http_read_types (http_config_t *cfg);
char *http_find_content_type (svz_socket_t *sock, char *file);

int http_parse_property (svz_socket_t *sock, char *request, char *end);
char *http_find_property (http_socket_t *sock, char *key);

int http_check_range (http_range_t *range, off_t filesize);
int http_get_range (char *line, http_range_t *range);
char *http_userdir (svz_socket_t *sock, char *uri);
int http_remotehost (char *host, void *closure);
int http_localhost (char *host, void *closure);
int http_identification (char *ident, void *closure);
void http_process_uri (char *uri);
int http_error_response (svz_socket_t *sock, int response);
void http_log (svz_socket_t *sock);
time_t http_parse_date (char *date);
char *http_asc_date (time_t t);
char *http_clf_date (time_t t);

void http_set_header (char *response);
int http_send_header (svz_socket_t *sock);
void http_reset_header (void);
void http_add_header (const char *fmt, ...);

#ifdef __MINGW32__
void http_start_netapi (void);
void http_stop_netapi (void);
#endif

#endif /* __HTTP_CORE_H__ */
