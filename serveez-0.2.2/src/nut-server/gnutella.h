/*
 * gnutella.h - gnutella protocol header file
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __GNUTELLA_H__
#define __GNUTELLA_H__ 1

/* general defines */
#define NUT_VERSION   "0.48"
#define NUT_CONNECT   "GNUTELLA CONNECT/0.4\n\n"
#define NUT_OK        "GNUTELLA OK\n\n"
#define NUT_HOSTS     "GET /%s HTTP/1."
#define NUT_GIVE      "GIV "

/* default values */
#define NUT_PORT             6346         /* gnutella default tcp port */
#define NUT_GUID             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
#define NUT_GUID_SIZE        16           /* GUID length in bytes */
#define NUT_SEARCH_INTERVAL  10           /* send search queries */
#define NUT_TTL              5            /* default packet TTL */
#define NUT_MAX_TTL          5            /* default maximum packet TTL */
#define NUT_CONNECT_INTERVAL 2            /* reconnect to gnutella hosts */
#define NUT_SEND_BUFSIZE     (1024 * 100) /* host list buffer size */
#define NUT_CONNECT_TIMEOUT  20           /* close connection then */
#define NUT_ENTRY_AGE        (60 * 3)     /* maximum hash entry age */

/* function IDs */
#define NUT_PING_REQ   0x00 /* ping */
#define NUT_PING_ACK   0x01 /* ping response */
#define NUT_PUSH_REQ   0x40 /* client push request */
#define NUT_SEARCH_REQ 0x80 /* search request */
#define NUT_SEARCH_ACK 0x81 /* search response */

/* protocol flags */
#define NUT_FLAG_DNLOAD 0x0001 /* downloading a file */
#define NUT_FLAG_HDR    0x0002 /* http header received?  */
#define NUT_FLAG_HOSTS  0x0004 /* sending host catcher list (http proto) */
#define NUT_FLAG_CLIENT 0x0008 /* normal gnutella host */
#define NUT_FLAG_UPLOAD 0x0010 /* uploading a file */
#define NUT_FLAG_SELF   0x0020 /* connecting to a gnutella host */
#define NUT_FLAG_GIVEN  0x0040 /* push request reply (GIV) */

/* guid:
 * The header contains a Microsoft GUID (Globally Unique Identifier for
 * you nonWinblows people) which is the message identifer.  My crystal ball
 * reports that "the GUIDs only have to be unique on the client", which
 * means that you can really put anything here, as long as you keep track
 * of it (a client won't respond to you if it sees the same message id
 * again).  If you're responding to a message, be sure you haven't seen the
 * message id (from that host) before, copy their message ID into your
 * response and send it on it's way.
 */

/*
 * The Gnutella packets are all in little endian byte order except
 * ip adresses which are in network byte order (big endian).  So they
 * need to be converted to host byte order if necessary.
 */

/* gnutella header */
typedef struct
{
  uint8_t id[NUT_GUID_SIZE];     /* message ID */
  uint8_t function;              /* function ID */
  uint8_t ttl;                   /* remaining TTL */
  uint8_t hop;                   /* hop count */
  unsigned int length;           /* data length */
}
nut_header_t;
#define SIZEOF_NUT_HEADER (NUT_GUID_SIZE + 7)

/* ping response structure */
typedef struct
{
  in_port_t port;      /* port number of the listening host */
  in_addr_t ip;        /* address of the listening host, network byte order */
  unsigned int files;  /* number of files shared by the host */
  unsigned int size;   /* total size of files shared by the host, in KB */
}
nut_pong_t;
#define SIZEOF_NUT_PONG (14)

/* search query header */
typedef struct
{
  uint16_t speed;       /* minimum speed (in kbps) */
  char search[1];       /* search request (NULL terminated) */
}
nut_query_t;
#define SIZEOF_NUT_QUERY (2)

/* search record structure */
typedef struct
{
  unsigned int index; /* file index */
  unsigned int size;  /* file size */
  char file[1];       /* file name (double-NULL terminated) */
}
nut_record_t;
#define SIZEOF_NUT_RECORD (8)

/* search reply header */
typedef struct
{
  uint8_t records;        /* number of records which follow this header */
  in_port_t port;         /* listening port number of the host */
  in_addr_t ip;           /* ip address of the host, network byte order */
  uint16_t speed;         /* speed of the host which found the results */
  uint16_t pad;           /* dunno */
  nut_record_t record[1]; /* array of records */
  uint8_t id[NUT_GUID_SIZE]; /* clientID128 sending the reply */
}
nut_reply_t;
#define SIZEOF_NUT_REPLY (11)

/* client push request structure */
typedef struct
{
  /* servers GUID the client wishes the push from */
  uint8_t id[NUT_GUID_SIZE];
  unsigned int index;     /* index of file requested */
  in_addr_t ip;           /* ip address of the host requesting the push */
  in_port_t port;         /* port number of the host requesting the push */
}
nut_push_t;
#define SIZEOF_NUT_PUSH (26)

/* gnutella host structure */
typedef struct
{
  uint8_t id[NUT_GUID_SIZE];     /* clientID128 GUID */
  in_addr_t ip;                  /* IP address */
  in_port_t port;                /* TCP port */
  time_t last_reply;             /* last packet received */
}
nut_host_t;

/* each gnutella host connection gets such a structure */
typedef struct
{
  unsigned dropped; /* number of dropped packets */
  unsigned packets; /* number of received packets */
  unsigned invalid; /* number of invalid packet types */
  unsigned queries; /* number of queries */
  unsigned files;   /* files at this connection */
  unsigned size;    /* file size (in KB) here */
  unsigned nodes;   /* number of hosts at this connection */
}
nut_client_t;

/* sent packet structure */
typedef struct
{
  time_t sent;        /* when was this packet sent */
  svz_socket_t *sock; /* sent to this socket */
}
nut_packet_t;

/* reply structure */
typedef struct
{
  svz_socket_t *sock; /* routing information */
  unsigned int index; /* file index to push */
}
nut_push_reply_t;

/* files in the sharing directory */
typedef struct
{
  off_t size;     /* file size */
  unsigned index; /* database index */
  char *file;     /* filename */
  char *path;     /* path to file */
  void *next;     /* pointer to next file entry */
}
nut_file_t;

/*
 * Protocol server specific configuration.
 */
typedef struct
{
  int disable;              /* if set we do not listen on any port cfg */
  int max_ttl;              /* maximum ttl for a gnutella packet */
  int ttl;                  /* initial ttl for a gnutella packet */
  svz_array_t *hosts;       /* array of initial hosts */
  uint8_t guid[NUT_GUID_SIZE]; /* this servers GUID */
  svz_hash_t *route;        /* routing table */
  svz_hash_t *conn;         /* connected hosts hash */
  svz_array_t *search;      /* search pattern array */
  size_t search_index;      /* current search pattern index */
  int search_limit;         /* limit amount of search reply records */
  svz_hash_t *packet;       /* this servers created packets */
  unsigned errors;          /* routing errors */
  unsigned files;           /* files within connected network */
  unsigned size;            /* file size (in KB) */
  unsigned nodes;           /* hosts within the connected network */
  char *save_path;          /* where to store downloaded files */
  char *share_path;         /* local search database path */
  int dnloads;              /* concurrent downloads */
  int max_dnloads;          /* maximum concurrent downloads */
  int speed;                /* connection speed (KBit/s) */
  int min_speed;            /* minimum connection speed for searching */
  svz_array_t *extensions;  /* file extensions */
  svz_hash_t *net;          /* host catcher */
  size_t connections;       /* number of connections to keep up */
  char *force_ip;           /* force the local ip to this value */
  in_addr_t ip;             /* calculated from `force_ip' */
  int force_port;           /* force the local port to this value */
  in_port_t port;           /* calculated from `force_port' */
  svz_hash_t *query;        /* recent query hash */
  svz_hash_t *reply;        /* reply hash for routing push requests */
  svz_hash_t *push;         /* push request hash */
  nut_file_t *database;     /* shared file array */
  unsigned db_files;        /* number of database files */
  unsigned db_size;         /* size of database in bytes */
  int uploads;              /* current number of uploads */
  int max_uploads;          /* maximum number of uploads */
  char *net_url;            /* configurable gnutella net url */
  char *net_detect;         /* detection string for the above value */
}
nut_config_t;

/*
 * Basic server callback definitions.
 */

/* detection routines */
int nut_detect_proto (svz_server_t *server, svz_socket_t *sock);
int nut_detect_connect (svz_socket_t *sock);

/* connection routine */
int nut_connect_socket (svz_server_t *server, svz_socket_t *sock);

/* check request routine */
int nut_check_request (svz_socket_t *sock);

/* disconnection routine */
int nut_disconnect (svz_socket_t *sock);

/* idle routines */
int nut_idle_searching (svz_socket_t *sock);
int nut_connect_timeout (svz_socket_t *sock);

/* server functions */
int nut_init (svz_server_t *server);
int nut_global_init (svz_servertype_t *server);
int nut_finalize (svz_server_t *server);
int nut_global_finalize (svz_servertype_t *server);
int nut_server_notify (svz_server_t *server);
char *nut_info_server (svz_server_t *server);
char *nut_info_client (svz_server_t *server, svz_socket_t *sock);

/*
 * This server's definition.
 */
extern svz_servertype_t nut_server_definition;

#endif /* __GNUTELLA_H__ */
