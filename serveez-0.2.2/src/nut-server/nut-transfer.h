/*
 * nut-transfer.h - gnutella file transfer definitions
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

#ifndef __NUT_TRANSFER_H__
#define __NUT_TRANSFER_H__ 1

#include "gnutella.h"

/* general definitions */
#define NUT_GET       "GET /get/"
#define NUT_AGENT     "User-agent: Gnutella\r\n"
#define NUT_HTTP      "HTTP/"
#define NUT_RANGE     "Content-range"
#define NUT_LENGTH    "Content-length"
#define NUT_CONTENT   "Content-type"
#define NUT_GET_OK    "HTTP 200 OK\r\n"
#define NUT_SEPERATOR "\r\n\r\n"

#define NUT_PATH_SIZE  1024 /* maximum path length */
#define NUT_PATH_DEPTH 10   /* directory recursion depth */

/* gnutella transfer data structure */
typedef struct
{
  int original_size; /* the file's size in the search reply */
  int size;          /* content length */
  char *file;        /* filename */
  time_t start;      /* when the upload started */
  int id;            /* original socket id */
  int version;       /* original socket version */
  int index;         /* file index */
  uint8_t guid[NUT_GUID_SIZE]; /* guid of host providing the file */
}
nut_transfer_t;

/* gnutella transfer functions */
int nut_init_transfer (svz_socket_t *, nut_reply_t *, nut_record_t *, char *);
int nut_init_upload (svz_socket_t *sock, nut_file_t *entry);
int nut_send_push (nut_config_t *cfg, nut_transfer_t *transfer);
void nut_free_transfer (nut_transfer_t *transfer);
void nut_read_database_r (nut_config_t *cfg, char *dirname, int depth);
void nut_add_database (nut_config_t *cfg, char *path, char *file, off_t size);
void nut_destroy_database (nut_config_t *cfg);
nut_file_t *nut_get_database (nut_config_t *, char *, unsigned);
nut_file_t *nut_find_database (nut_config_t *, nut_file_t *, char *);

/* check request routine */
int nut_check_upload (svz_socket_t *sock);
int nut_check_given (svz_socket_t *sock);

/* read and write callbacks */
int nut_file_read (svz_socket_t *sock);
int nut_file_write (svz_socket_t *sock);

/* disconnection routine */
int nut_disconnect_upload (svz_socket_t *sock);

/* recursion wrapper */
#define nut_read_database(cfg, dir) nut_read_database_r (cfg, dir, 0)

#endif /* not __NUT_TRANSFER_H__ */
