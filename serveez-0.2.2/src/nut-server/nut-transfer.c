/*
 * nut-transfer.c - gnutella file transfer implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
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
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <sys/stat.h>
#if HAVE_FLOSS_H
# include <floss.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <sys/types.h>
#include <errno.h>

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>

# if HAVE_DIRENT_H
#  include <dirent.h>
#  define NAMLEN(dirent) strlen((dirent)->d_name)
# else
#  define dirent direct
#  define NAMLEN(dirent) (dirent)->d_namlen
#  if HAVE_SYS_NDIR_H
#   include <sys/ndir.h>
#  endif
#  if HAVE_SYS_DIR_H
#   include <sys/dir.h>
#  endif
#  if HAVE_NDIR_H
#   include <ndir.h>
#  endif
# endif
#endif /* not __MINGW32__ */

#ifdef __MINGW32__
# include <windows.h>
# include <io.h>
# include "woe-statpred.h"
#endif

#if HAVE_SYS_DIRENT_H && !defined (HAVE_DIRENT_H)
# include <sys/dirent.h>
#endif

#ifndef __MINGW32__
# define FILENAME de->d_name
#else
# define FILENAME de.cFileName
# define closedir(dir) FindClose (dir)
#endif

#include "o-binary.h"
#include "networking-headers.h"
#include "libserveez.h"
#include "gnutella.h"
#include "nut-core.h"
#include "nut-request.h"
#include "nut-transfer.h"

/*
 * Check if a given search pattern matches a filename.  Return non-zero
 * on success and zero otherwise.
 */
static int
nut_string_regex (char *text, char *regex)
{
  char *p, *token, *str, *reg;

  /* first check if text tokens are in text */
  if (!strchr (regex, '*') && !strchr (regex, '?'))
    {
      str = svz_strdup (text);
      reg = svz_strdup (regex);
      svz_tolower (str);
      svz_tolower (reg);

      /* all tokens must be in the text */
      for (token = strtok (reg, " "); token; token = strtok (NULL, " "))
        {
          if (!strstr (str, token))
            break;
        }
      svz_free (str);
      svz_free (reg);
      if (!token)
        return -1;
      return 0;
    }

  /* parse until end of both strings */
  else
    while (*regex && *text)
      {
        /* find end of strings or '?' or '*' */
        while (*regex != '*' && *regex != '?' && *regex && *text)
          {
            /* return no Match if so */
            if (tolower (*text) != tolower (*regex))
              return 0;
            text++;
            regex++;
          }
        /* one free character */
        if (*regex == '?')
          {
            if (!(*text))
              return 0;
            text++;
            regex++;
          }
        /* free characters */
        else if (*regex == '*')
          {
            regex++;
            /* skip useless '?'s after '*'s */
            while (*regex == '?')
              regex++;
            /* skip all characters until next character in pattern found */
            while (*text && tolower (*regex) != tolower (*text))
              text++;
            /* next character in pattern found */
            if (*text)
              {
                /* find the last occurrence of this character in the text */
                p = text + strlen (text);
                while (tolower (*p) != tolower (*text))
                  p--;
                /* continue parsing at this character */
                text = p;
              }
          }
      }

  /* is the text longer than the regex?  */
  if (!*text && !*regex)
    return -1;
  return 0;
}

/*
 * Within this callback the actual file transfer is done.
 */
static int
nut_save_transfer (svz_socket_t *sock)
{
  int fill = sock->recv_buffer_fill;
  nut_transfer_t *transfer = sock->data;
  int num_written;

  /* do we have something to write in the receive buffer?  */
  if (fill > 0)
    {
      /* write as much data as possible */
      num_written = write (sock->file_desc, sock->recv_buffer, fill);

      /* seems like an error occurred */
      if (num_written < 0)
        {
          svz_log_sys_error ("nut: write");
          return -1;
        }

      /* crop written data from receive buffer */
      svz_sock_reduce_recv (sock, num_written);

      /* did we get all data */
      if ((transfer->size -= num_written) <= 0)
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "nut: file successfully received\n");
#endif
          /* yes, shutdown the connection */
          return -1;
        }
    }
  return 0;
}

/*
 * This is the sock->check_request callback for gnutella file transfers.
 * Whenever there is data within the receive queue it will be called.
 */
static int
nut_check_transfer (svz_socket_t *sock)
{
  int fill = sock->recv_buffer_fill;
  int len = strlen (NUT_GET_OK);
  char *p = sock->recv_buffer, *length;
  nut_transfer_t *transfer = sock->data;

  /* check if got at least the first part of the HTTP header */
  if (fill >= len && !memcmp (sock->recv_buffer, NUT_GET_OK, len))
    {
      /* find the end of the HTTP header (twice a CR/LF) */
      while (p < sock->recv_buffer + (fill - 3) &&
             memcmp (p, NUT_SEPERATOR, 4))
        p++;

      /* did we get all the header information?  */
      if (p < sock->recv_buffer + (fill - 3) && !memcmp (p, NUT_SEPERATOR, 4))
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "nut: download header received\n");
#endif

          len = p - sock->recv_buffer + 1;
          length = nut_parse_property (sock->recv_buffer, len, NUT_LENGTH);
          if (length == NULL)
            {
#if ENABLE_DEBUG
              svz_log (SVZ_LOG_DEBUG, "nut: no content length given\n");
#endif
              return -1;
            }

          /*
           * check if the announced file length in the search reply
           * corresponds to the content length of this HTTP header
           */
          sock->userflags |= NUT_FLAG_HDR;
          transfer->size = svz_atoi (length);
          svz_free (length);
          if (transfer->original_size != transfer->size)
            {
              svz_log (SVZ_LOG_WARNING,
                       "nut: transfer sizes differ (%u!=%u)\n",
                       transfer->original_size, transfer->size);
            }

          /* assign the appropriate gnutella transfer callbacks */
          sock->check_request = nut_save_transfer;
          sock->write_socket = NULL;
          sock->idle_func = NULL;

          /* crop header from receive buffer */
          len = (p - sock->recv_buffer) + 4;
          svz_sock_reduce_recv (sock, len);
        }
    }

  return 0;
}

/*
 * Frees the given transfer structure.
 */
void
nut_free_transfer (nut_transfer_t *transfer)
{
  svz_free (transfer->file);
  svz_free (transfer);
}

/*
 * This callback is executed whenever a gnutella file transfer aborted
 * or successfully exited.
 */
static int
nut_disconnect_transfer (svz_socket_t *sock)
{
  nut_config_t *cfg = sock->cfg;
  nut_transfer_t *transfer = sock->data;

  /* decrement amount of concurrent downloads */
  cfg->dnloads--;

  /* finally close the received file */
  if (close (sock->file_desc) == -1)
    svz_log_sys_error ("nut: close");

  /* free the transfer data */
  if (transfer)
    {
      /* if the transfer was really aborted we remove the downloaded file */
      if (transfer->size > 0 || !(sock->userflags & NUT_FLAG_HDR))
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "nut: downloading `%s' aborted\n",
                   transfer->file);
#endif
          if (unlink (transfer->file) == -1)
            svz_log_sys_error ("nut: unlink");
        }

      /*
       * send a push request if the connection process itself has been
       * aborted (refused or no route)
       */
      if (sock->userflags & NUT_FLAG_DNLOAD &&
          !(sock->userflags & NUT_FLAG_HDR))
        {
          nut_send_push (sock->cfg, sock->data);
        }

      nut_free_transfer (transfer);
      sock->data = NULL;
    }

  return 0;
}

/*
 * This routine tries to connect to a foreign gnutella host in order to
 * get a certain file.
 */
int
nut_init_transfer (svz_socket_t *sock, nut_reply_t *reply,
                   nut_record_t *record, char *savefile)
{
  nut_config_t *cfg = sock->cfg;
  svz_address_t *addr;
  svz_socket_t *xsock;
  char *ext, *file, *pattern;
  struct stat buf;
  int fd;
  nut_transfer_t *transfer;
  size_t n = 0;
  int pos;

  /* has the requested file the right file extension?  */
  if (cfg->extensions)
    {
      /* go through all file extensions */
      svz_array_foreach (cfg->extensions, ext, n)
        {
          size_t elen = strlen (ext);

          if (strlen (savefile) > elen)
            {
              pos = strlen (savefile) - elen;
              if (pos < 0 || !strncasecmp (&savefile[pos], ext, 1 + elen))
                break;
            }
        }
      /* did the above code "break"?  */
      if (n >= svz_array_size (cfg->extensions))
        {
          svz_log (SVZ_LOG_WARNING, "nut: not a valid extension: %s\n",
                   savefile);
          return -1;
        }
    }

  /* first check if the requested file is not already created */
  file = svz_malloc (strlen (cfg->save_path) + strlen (savefile) + 2);
  sprintf (file, "%s/%s", cfg->save_path, savefile);
  if (stat (file, &buf) != -1)
    {
      svz_log (SVZ_LOG_NOTICE, "nut: %s already exists\n", savefile);
      svz_free (file);
      return -1;
    }

  /* second check if the file matches the original search patterns */
  svz_array_foreach (cfg->search, pattern, n)
    {
      if (nut_string_regex (savefile, pattern))
        break;
    }
  if (n >= svz_array_size (cfg->search))
    {
      svz_log (SVZ_LOG_NOTICE, "nut: no search pattern for %s\n", savefile);
      svz_free (file);
      return -1;
    }

  /* try creating local file */
  if ((fd = open (file, O_RDWR | O_CREAT | O_BINARY, 0644)) == -1)
    {
      svz_log_sys_error ("nut: open");
      svz_free (file);
      return -1;
    }

  /* try to connect to the host */
  addr = svz_address_make (AF_INET, &reply->ip);
  xsock = svz_tcp_connect (addr, reply->port);
  svz_free (addr);
  if (xsock != NULL)
    {
      svz_log (SVZ_LOG_NOTICE, "nut: connecting %s:%u\n",
               svz_inet_ntoa (reply->ip), ntohs (reply->port));
      cfg->dnloads++;
      xsock->cfg = cfg;
      xsock->flags |= SVZ_SOFLG_NOFLOOD;
      svz_sock_setparent (xsock, svz_sock_getparent (sock));
      xsock->disconnected_socket = nut_disconnect_transfer;
      xsock->check_request = nut_check_transfer;
      xsock->userflags = NUT_FLAG_DNLOAD;
      xsock->file_desc = fd;
      xsock->idle_func = nut_connect_timeout;
      xsock->idle_counter = NUT_CONNECT_TIMEOUT;

      /* initialize transfer data */
      transfer = svz_calloc (sizeof (nut_transfer_t));
      transfer->original_size = record->size;
      transfer->file = svz_strdup (file);
      transfer->start = time (NULL);
      xsock->data = transfer;

      /* save all data for sending a push request some time later */
      memcpy (transfer->guid, reply->id, NUT_GUID_SIZE);
      transfer->index = record->index;
      transfer->id = sock->id;
      transfer->version = sock->version;

      /* send HTTP request to the listening gnutella host */
      svz_sock_printf (xsock, NUT_GET "%d/%s " NUT_HTTP "1.0\r\n",
                       record->index, savefile);
      svz_sock_printf (xsock, NUT_AGENT);
      svz_sock_printf (xsock, NUT_RANGE ": bytes=0-\r\n");
      svz_sock_printf (xsock, "\r\n");
      svz_free (file);
      return 0;
    }

  close (fd);
  svz_free (file);
  return 0;
}

/*
 * This is the check_request callback for given files.
 */
int
nut_check_given (svz_socket_t *sock)
{
  nut_config_t *cfg = sock->cfg;
  int fill = sock->recv_buffer_fill;
  char *p = sock->recv_buffer;
  nut_transfer_t *transfer;
  char *pushkey, *file;
  struct stat buf;
  int len, fd;

  /* check if we got the whole "GIV " line */
  while (p < sock->recv_buffer + (fill - 1) && memcmp (p, "\n\n", 2))
    p++;

  if (p < sock->recv_buffer + (fill - 1) && !memcmp (p, "\n\n", 2))
    {
      len = p + 2 - sock->recv_buffer;

      /* find start of file name */
      pushkey = p = sock->recv_buffer + strlen (NUT_GIVE);
      while (p < sock->recv_buffer + fill && *p != '/')
        p++;
      if (p >= sock->recv_buffer + fill || *p != '/')
        {
          svz_log (SVZ_LOG_ERROR, "nut: invalid GIV line\n");
          return -1;
        }

      /* get original push request */
      *p = '\0';
      transfer = (nut_transfer_t *) svz_hash_get (cfg->push, pushkey);
      if (transfer == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "nut: no such push request sent\n");
          return -1;
        }

      /* delete key and data from push request hash */
      svz_hash_delete (cfg->push, pushkey);
      svz_sock_reduce_recv (sock, len);

      /* assign all necessary callbacks for downloading the file */
      sock->data = transfer;
      cfg->dnloads++;
      sock->flags |= SVZ_SOFLG_NOFLOOD;
      sock->disconnected_socket = nut_disconnect_transfer;
      sock->check_request = nut_check_transfer;
      sock->userflags |= NUT_FLAG_DNLOAD;
      sock->idle_func = NULL;
      transfer->start = time (NULL);

      /* test the file to download once again */
      file = transfer->file;
      if (stat (file, &buf) != -1)
        {
          svz_log (SVZ_LOG_NOTICE, "nut: %s already exists\n", file);
          return -1;
        }

      /* try creating local file */
      if ((fd = open (file, O_RDWR | O_CREAT | O_BINARY, 0644)) == -1)
        {
          svz_log_sys_error ("nut: open");
          return -1;
        }

      /* assign file descriptor and find original file name */
      sock->file_desc = fd;
      file = file + strlen (file);
      while (*file != '/' && *file != '\\' && file > transfer->file)
        file--;
      if (*file == '/' || *file == '\\')
        file++;

      /* send HTTP request to the listening gnutella host */
      svz_sock_printf (sock, NUT_GET "%d/%s " NUT_HTTP "1.0\r\n",
                       transfer->index, file);
      svz_sock_printf (sock, NUT_AGENT);
      svz_sock_printf (sock, NUT_RANGE ": bytes=0-\r\n");
      svz_sock_printf (sock, "\r\n");
    }
  return 0;
}

/*
 * The routine is called when a connection to another host timed out.
 * When trying to get a remote file from a host behind a masquerading
 * gateway or firewall you are able to request this host to connect to
 * ourselves and thus "push" the download connection.  There is NO way
 * if both of the hosts are behind such a gateway.
 */
int
nut_send_push (nut_config_t *cfg, nut_transfer_t *transfer)
{
  svz_socket_t *sock;
  nut_header_t hdr;
  nut_push_t push;
  nut_packet_t *pkt;
  nut_transfer_t *trans;
  char *pushkey;
  struct sockaddr_in *addr = NULL;
  svz_portcfg_t *port;

  /* find original socket connection */
  if ((sock = svz_sock_find (transfer->id, transfer->version)) != NULL)
    {
      /* create new gnutella header */
      nut_calc_guid (hdr.id);
      hdr.function = NUT_PUSH_REQ;
      hdr.ttl = (uint8_t) cfg->ttl;
      hdr.hop = 0;
      hdr.length = SIZEOF_NUT_PUSH;

      /* create push request */
      memcpy (push.id, transfer->guid, NUT_GUID_SIZE);
      push.index = transfer->index;
      if ((port = svz_sock_portcfg (sock)) != NULL)
        addr = svz_portcfg_addr (port);
      push.ip = nut_v4addr_from (cfg, addr, sock);
      push.port = (cfg->port ? cfg->port : addr ?
                   addr->sin_port : sock->local_port);

      /* create push request key and check if it was already sent */
      pushkey = svz_malloc (16 + NUT_GUID_SIZE * 2);
      sprintf (pushkey, "%d:%s", push.index, nut_text_guid (push.id));
      if ((trans = svz_hash_get (cfg->push, pushkey)) != NULL)
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "nut: push request already sent\n");
#endif
          svz_free (pushkey);
          return -1;
        }

      /* try sending header and push request */
      if (svz_sock_write (sock, (char *) nut_put_header (&hdr),
                          SIZEOF_NUT_HEADER) == -1 ||
          svz_sock_write (sock, (char *) nut_put_push (&push),
                          SIZEOF_NUT_PUSH) == -1)
        {
          svz_sock_schedule_for_shutdown (sock);
          svz_free (pushkey);
          return -1;
        }

      /* put push request into hash for later reply detection */
      trans = svz_malloc (sizeof (nut_transfer_t));
      memcpy (trans, transfer, sizeof (nut_transfer_t));
      trans->file = svz_strdup (transfer->file);
      svz_hash_put (cfg->push, pushkey, trans);
      svz_free (pushkey);

#if ENABLE_DEBUG
      {
        char buf[64];

        svz_log (SVZ_LOG_DEBUG, "nut: sent push request to %s\n",
                 SVZ_PP_ADDR_PORT (buf, sock->remote_addr,
                                   sock->remote_port));
      }
#endif

      /* put into sent packet hash */
      pkt = svz_malloc (sizeof (nut_packet_t));
      pkt->sock = sock;
      pkt->sent = time (NULL);
      svz_hash_put (cfg->packet, (char *) hdr.id, pkt);
    }
  return 0;
}

/*
 * Destroy database.
 */
void
nut_destroy_database (nut_config_t *cfg)
{
  nut_file_t *entry;

  while ((entry = cfg->database) != NULL)
    {
      cfg->database = entry->next;
      svz_free (entry->file);
      svz_free (entry->path);
      svz_free (entry);
    }
  cfg->db_files = 0;
  cfg->db_size = 0;
}

/*
 * Add a further file to our database.
 */
void
nut_add_database (nut_config_t *cfg, char *path, char *file, off_t size)
{
  nut_file_t *entry;

  entry = svz_malloc (sizeof (nut_file_t));
  entry->file = svz_strdup (file);
  entry->path = svz_strdup (path);
  entry->size = size;
  entry->index = cfg->db_files;
  entry->next = cfg->database;
  cfg->database = entry;
  cfg->db_files++;
  cfg->db_size += size;
}

/*
 * Find a given search pattern within the database.  Start to find it
 * at the given ENTRY.  If it is NULL we start at the very beginning.
 */
nut_file_t *
nut_find_database (nut_config_t *cfg, nut_file_t *entry, char *search)
{
  if (entry == NULL)
    entry = cfg->database;
  else
    entry = entry->next;

  while (entry)
    {
      if (nut_string_regex (entry->file, search))
        return entry;
      entry = entry->next;
    }
  return NULL;
}

/*
 * This routine gets a gnutella database entry from a given FILE and
 * its appropriate INDEX.  If no matching file has been found then return
 * NULL.  If FILE is NULL we just search for the the given INDEX.
 */
nut_file_t *
nut_get_database (nut_config_t *cfg, char *file, unsigned index)
{
  nut_file_t *entry = NULL;

  for (entry = cfg->database; entry; entry = entry->next)
    {
      if (entry->index == index)
        if (file == NULL || !strcmp (entry->file, file))
          return entry;
    }

  return entry;
}

/*
 * This routine will re-read the share directory.  The routine itself is
 * recursive.  Thus be careful!  It cannot check for recursion loops, yet.
 */
void
nut_read_database_r (nut_config_t *cfg, char *dirname, int depth)
{
  char *path;
  static struct stat buf;
  static char filename[NUT_PATH_SIZE];
#ifndef __MINGW32__
  DIR *dir;
  static struct dirent *de = NULL;
#else
  HANDLE dir;
  static WIN32_FIND_DATA de;
#endif

  /* first call */
  if (!depth)
    {
      nut_destroy_database (cfg);
    }

  /* check recursion condition */
  if (depth < NUT_PATH_DEPTH)
    {
      depth++;

      /* open the directory */
#ifdef __MINGW32__
      if (snprintf (filename, NUT_PATH_SIZE - 1, "%s/*", dirname) == -1)
        return;

      if (! svz_invalid_handle_p (dir = FindFirstFile (filename, &de)))
#else
      if ((dir = opendir (dirname)) != NULL)
#endif
        {
          /* iterate directory */
#ifndef __MINGW32__
          while (NULL != (de = readdir (dir)))
#else
          do
#endif
            {
              if (snprintf (filename, NUT_PATH_SIZE - 1,
                            "%s/%s", dirname, FILENAME) == -1)
                continue;

              /* stat the given file */
              if (stat (filename, &buf) != -1)
                {
                  /* add valid files to database */
                  if (S_ISREG (buf.st_mode) && buf.st_size > 0)
                    {
                      nut_add_database (cfg, dirname, FILENAME, buf.st_size);
                    }
                  /* recurse into directories */
                  else if (S_ISDIR (buf.st_mode) && FILENAME[0] != '.')
                    {
                      path = svz_strdup (filename);
                      nut_read_database_r (cfg, path, depth);
                      svz_free (path);
                    }
                }
            }
#ifdef __MINGW32__
          while (FindNextFile (dir, &de));
#endif
          closedir (dir);
        }
    }
}

/*
 * This routine checks for a valid http header for gnutella upload
 * requests.
 */
int
nut_check_upload (svz_socket_t *sock)
{
  char *p = sock->recv_buffer;
  int len, fill = strlen (NUT_GET);
  unsigned index = 0;
  char *end, *file, *f, *hdr;
  nut_file_t *entry;

  /* enough receive buffer fill?  */
  if (sock->recv_buffer_fill < fill)
    return 0;

  /* initial gnutella request detection */
  if (memcmp (p, NUT_GET, fill))
    return -1;

  /* parse whole upload header and find the end of the HTTP header */
  fill = sock->recv_buffer_fill;
  while (p < sock->recv_buffer + (fill - 3) && memcmp (p, NUT_SEPERATOR, 4))
    p++;

  if (p < sock->recv_buffer + (fill - 3) && !memcmp (p, NUT_SEPERATOR, 4))
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "nut: upload header received\n");
#endif
      /* parse first (GET) line */
      len = p - sock->recv_buffer + 1;
      p = sock->recv_buffer + strlen (NUT_GET);
      end = sock->recv_buffer + len;
      while (p < end && *p >= '0' && *p <= '9')
        {
          index *= 10;
          index += *p - '0';
          p++;
        }
      /* parsed file index */
      if (p >= end || *p != '/')
        return -1;
      f = ++p;
      while (p < end && *p != '\r' && *p != '\n')
        p++;

      /* got actual header property field */
      hdr = p + 2;
      len -= hdr - sock->recv_buffer;

      while (p > f && memcmp (p, " " NUT_HTTP, strlen (NUT_HTTP) + 1))
        p--;
      if (p <= f)
        return -1;
      /* parsed file itself */
      fill = p - f;
      file = svz_malloc (fill + 1);
      memcpy (file, f, fill);
      file[fill] = '\0';

      /* here we parse all the header properties */

      /* find file in database */
      if ((entry = nut_get_database (sock->cfg, file, index)) == NULL)
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "nut: no such file: %s, %u\n", file, index);
#endif
          svz_free (file);
          return -1;
        }
      len = end - sock->recv_buffer + 3;
      svz_sock_reduce_recv (sock, len);
      svz_free (file);

      /* disable connection timeout */
      sock->idle_func = NULL;
      sock->userflags |= NUT_FLAG_HDR;

      if (nut_init_upload (sock, entry) == -1)
        return -1;
    }

  return 0;
}

/*
 * If the gnutella header has been successfully parsed this routine
 * initializes the file upload.
 */
int
nut_init_upload (svz_socket_t *sock, nut_file_t *entry)
{
  char *file;
  nut_config_t *cfg = sock->cfg;
  struct stat buf;
  int fd;
  nut_transfer_t *transfer;

  /* create filename */
  file = svz_malloc (strlen (entry->path) + strlen (entry->file) + 2);
  sprintf (file, "%s/%s", entry->path, entry->file);

  /* check file */
  if (stat (file, &buf) == -1 || !S_ISREG (buf.st_mode) || buf.st_size <= 0)
    {
      svz_log (SVZ_LOG_ERROR, "nut: invalid file: %s %s\n", file);
      svz_free (file);
      return -1;
    }

  /* open the file for reading */
  if ((fd = open (file, O_RDONLY | O_BINARY)) == -1)
    {
      svz_log_sys_error ("nut: open");
      svz_free (file);
      return -1;
    }

  svz_sock_printf (sock, NUT_GET_OK  NUT_AGENT);
  svz_sock_printf (sock, NUT_CONTENT ": application/binary\r\n");
  svz_sock_printf (sock, NUT_LENGTH ": %d\r\n", buf.st_size);
  svz_sock_printf (sock, "\r\n");

  sock->file_desc = fd;
  sock->read_socket = nut_file_read;
  sock->write_socket = nut_file_write;
  sock->disconnected_socket = nut_disconnect_upload;
  sock->flags |= SVZ_SOFLG_FILE;
  cfg->uploads++;

  transfer = svz_calloc (sizeof (nut_transfer_t));
  transfer->size = buf.st_size;
  transfer->original_size = buf.st_size;
  transfer->file = svz_strdup (file);
  transfer->start = time (NULL);
  sock->data = transfer;

  svz_free (file);
  return 0;
}

/*
 * Disconnection callback for gnutella uploads.
 */
int
nut_disconnect_upload (svz_socket_t *sock)
{
  nut_config_t *cfg = sock->cfg;
  nut_transfer_t *transfer = sock->data;

  /* decrement amount of concurrent uploads */
  cfg->uploads--;

  /* finally close the received file */
  if (close (sock->file_desc) == -1)
    svz_log_sys_error ("nut: close");

  /* free the transfer data */
  if (transfer)
    {
      nut_free_transfer (transfer);
      sock->data = NULL;
    }

  return 0;
}

/*
 * Default gnutella file reader.  It is the sock->read_socket callback for
 * file uploads.
 */
int
nut_file_read (svz_socket_t *sock)
{
  int num_read;
  int do_read;
  nut_transfer_t *transfer = sock->data;

  do_read = sock->send_buffer_size - sock->send_buffer_fill;

  /*
   * This means the send buffer is currently full, we have to
   * wait until some data has been send via the socket.
   */
  if (do_read <= 0)
    {
      return 0;
    }

  /*
   * Try to read as much data as possible from the file.
   */
  num_read = read (sock->file_desc,
                   sock->send_buffer + sock->send_buffer_fill, do_read);

  /* Read error occurred.  */
  if (num_read < 0)
    {
      svz_log_sys_error ("nut: read");
      return -1;
    }

  /* Bogus file.  File size from ‘stat’ was not true.  */
  if (num_read == 0 && transfer->size != 0)
    {
      return -1;
    }

  /* Data has been read or EOF reached, set the appropriate flags.  */
  sock->send_buffer_fill += num_read;
  transfer->size -= num_read;

  /* Read all file data?  */
  if (transfer->size <= 0)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "nut: file successfully read\n");
#endif
      /*
       * no further ‘read’s from the file descriptor, signaling
       * the writers there will not be additional data from now on
       */
      sock->read_socket = svz_tcp_read_socket;
      sock->flags &= ~SVZ_SOFLG_FILE;
    }

  return 0;
}

#define THROTTLE_RELAX  1

/*
 * This function is the upload callback for the gnutella server.  It
 * throttles its network output to a configured value.
 */
int
nut_file_write (svz_socket_t *sock)
{
  int num_written, do_write;
  nut_transfer_t *transfer = sock->data;
  nut_config_t *cfg = sock->cfg;
  time_t t = time (NULL);

  /* throttle the network output */
  num_written = transfer->original_size - transfer->size;
  if (num_written / (t - transfer->start + 1) > cfg->speed * 1024 / 8)
    {
      sock->unavailable = t + THROTTLE_RELAX;
      return 0;
    }

  /*
   * Write as many bytes as possible, remember how many
   * were actually sent.
   */
  do_write = (sock->send_buffer_fill > SVZ_SOCK_MAX_WRITE)
    ? SVZ_SOCK_MAX_WRITE : sock->send_buffer_fill;

  num_written = send (sock->sock_desc, sock->send_buffer, do_write, 0);

  /* some data has been written */
  if (num_written > 0)
    {
      sock->last_send = t;

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
      svz_log_net_error ("nut: send");
      if (svz_wait_if_unavailable (sock, 1))
        num_written = 0;
    }

  if (sock->send_buffer_fill == 0 && transfer->size <= 0)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "nut: file successfully sent\n");
#endif
      return -1;
    }

  /*
   * Return a non-zero value if an error occurred.
   */
  return (num_written < 0) ? -1 : 0;
}
