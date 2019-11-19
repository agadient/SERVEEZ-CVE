/*
 * http-cgi.c - http cgi implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2003, 2004, 2007 Stefan Jahn <stefan@lkcc.org>
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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#if HAVE_FLOSS_H
# include <floss.h>
#endif
#include <signal.h>
#include "changedir.h"
#include "networking-headers.h"
#if HAVE_STRINGS_H
# include <strings.h>
#endif

#ifdef __MINGW32__
# include <io.h>
# include <shellapi.h>
#endif

#ifndef __MINGW32__
# if HAVE_WAIT_H
#  include <wait.h>
# endif
# if HAVE_SYS_WAIT_H
#  include <sys/wait.h>
# endif
#endif

#include "libserveez.h"
#include "http-proto.h"
#include "http-core.h"
#include "http-cgi.h"
#include "unused.h"

/*
 * Extended disconnect_socket callback for CGIs.  Handling CGI related
 * topics and afterwards we process the normal http disconnection
 * functionality.
 */
int
http_cgi_disconnect (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;

  /* flush CGI output if necessary */
  if (sock->flags & SVZ_SOFLG_PIPE && sock->send_buffer_fill > 0)
    if (sock->write_socket)
      sock->write_socket (sock);

  /* close both of the CGI pipes if necessary */
  if (! svz_invalid_handle_p (sock->pipe_desc[SVZ_READ]))
    {
      if (svz_closehandle (sock->pipe_desc[SVZ_READ]) == -1)
        svz_log_sys_error ("close");
      svz_invalidate_handle (&sock->pipe_desc[SVZ_READ]);
      sock->flags &= ~SVZ_SOFLG_RECV_PIPE;
    }
  if (! svz_invalid_handle_p (sock->pipe_desc[SVZ_WRITE]))
    {
      if (svz_closehandle (sock->pipe_desc[SVZ_WRITE]) == -1)
        svz_log_sys_error ("close");
      svz_invalidate_handle (&sock->pipe_desc[SVZ_WRITE]);
      sock->flags &= ~SVZ_SOFLG_SEND_PIPE;
    }

#ifdef __MINGW32__
  /*
   * Close the process handle if necessary, but only in the Windows-Port !
   */
  if (! svz_invalid_handle_p (http->pid))
    {
      if (!TerminateProcess (http->pid, 0))
        svz_log_sys_error ("TerminateProcess");
      if (svz_closehandle (http->pid) == -1)
        svz_log_sys_error ("CloseHandle");
      svz_invalidate_handle (&http->pid);
    }
#else /* not __MINGW32__ */
  /*
   * Try killing the cgi script.
   */
  if (! svz_invalid_handle_p (http->pid))
    {
      if (kill (http->pid, SIGKILL) == -1)
        svz_log_sys_error ("kill");
#if HAVE_WAITPID
      /* Test if the cgi is still running and cleanup.  */
      else if (waitpid (http->pid, NULL, 0) == -1)
        svz_log_sys_error ("waitpid");
#endif /* not HAVE_WAITPID */
      svz_invalidate_handle (&http->pid);
    }
#endif /* not __MINGW32__ */

  return http_disconnect (sock);
}

/*
 * This is the default idle function for http connections.  It checks
 * whether any died child was a cgi script.
 */
int
http_cgi_died (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;

  if (sock->flags & SVZ_SOFLG_PIPE)
    {
#ifndef __MINGW32__
      /* Check if a died child is this cgi.  */
      if (svz_most_recent_dead_child_p (http->pid))
        svz_log (SVZ_LOG_NOTICE, "cgi script pid %d died\n", (int) http->pid);
#if HAVE_WAITPID
      /* Test if the cgi is still running.  */
      if (waitpid (http->pid, NULL, WNOHANG) == http->pid)
        {
          svz_log (SVZ_LOG_NOTICE, "cgi script pid %d died\n", (int) http->pid);
          svz_invalidate_handle (&http->pid);
        }
#endif /* HAVE_WAITPID */

#else /* __MINGW32__ */

      /*
       * Check if there died a process handle in Win32, this has to be
       * done regularly here because there is no SIGCHLD in Win32 !
       */
      if (! svz_invalid_handle_p (http->pid))
        svz_mingw_child_dead_p ("", &http->pid);

#endif /* __MINGW32__ */
    }

  sock->idle_counter = 1;
  return 0;
}

/*
 * The http cgi reader gets data from the stdout of a cgi
 * program and stores the data into the send buffer of
 * the socket structure.  We set the HTTP_FLAG_DONE flag
 * to indicate there was no more data.
 */
int
http_cgi_read (svz_socket_t *sock)
{
  int do_read;
  int num_read;
  http_socket_t *http = sock->data;

  /* read as much space is left in the buffer */
  do_read = sock->send_buffer_size - sock->send_buffer_fill;
  if (do_read <= 0)
    {
      return 0;
    }

#ifdef __MINGW32__
  /* check how many bytes could be read from the cgi pipe */
  if (!PeekNamedPipe (sock->pipe_desc[SVZ_READ], NULL, 0,
                      NULL, (DWORD *) &num_read, NULL))
    {
      svz_log_sys_error ("cgi: PeekNamedPipe");
      return -1;
    }

  /* adjust number of bytes to read */
  if (do_read > num_read)
    do_read = num_read;

  /* really read from pipe */
  if (!ReadFile (sock->pipe_desc[SVZ_READ],
                 sock->send_buffer + sock->send_buffer_fill,
                 do_read, (DWORD *) &num_read, NULL))
    {
      svz_log_sys_error ("cgi: ReadFile");
      num_read = -1;
    }
#else /* not __MINGW32__ */
  if ((num_read = read (sock->pipe_desc[SVZ_READ],
                        sock->send_buffer + sock->send_buffer_fill,
                        do_read)) == -1)
    {
      svz_log_sys_error ("cgi: read");
      if (errno == EAGAIN)
        return 0;
      num_read = -1;
    }
#endif /* not __MINGW32__ */

  /* data has been read */
  else if (num_read > 0)
    {
      http->length += num_read;
      sock->send_buffer_fill += num_read;
      return 0;
    }

#ifdef __MINGW32__
  /*
   * because pipes cannot be ‘select’ed it can happen that there is no
   * data within the receiving pipe, but the cgi has not yet terminated
   */
  if (num_read == 0 && ! svz_invalid_handle_p (http->pid))
    {
      return 0;
    }
#endif /* __MINGW32__ */

  /* no data has been received */
  sock->userflags |= HTTP_FLAG_DONE;
  if (sock->send_buffer_fill == 0)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "cgi: data successfully received and resent\n");
#endif
      sock->userflags &= ~HTTP_FLAG_CGI;
      sock->flags &= ~SVZ_SOFLG_RECV_PIPE;
      return -1;
    }

  return 0;
}

/*
 * HTTP_CGI_WRITE pipes all read data from the http socket connection
 * into the cgi stdin.  This is necessary for the so called post method.
 * It directly reads from the RECV_BUFFER of the socket structure.
 */
int
http_cgi_write (svz_socket_t *sock)
{
  int do_write;
  int num_written;
  http_socket_t *http = sock->data;

  /*
   * Write as many bytes as possible, remember how many
   * were actually sent.  Do not write more than the content
   * length of the post data.
   */
  do_write = sock->recv_buffer_fill;
  if (do_write > http->contentlength)
    do_write = http->contentlength;

#ifdef __MINGW32__
  if (!WriteFile (sock->pipe_desc[SVZ_WRITE], sock->recv_buffer,
                  do_write, (DWORD *) &num_written, NULL))
    {
      svz_log_sys_error ("cgi: WriteFile");
      num_written = -1;
    }
#else /* !__MINGW32__ */
  if ((num_written = write (sock->pipe_desc[SVZ_WRITE],
                            sock->recv_buffer, do_write)) == -1)
    {
      svz_log_sys_error ("cgi: write");
    }
#endif /* !__MINGW32__ */

  /* data has been successfully sent */
  if (num_written > 0)
    {
      sock->last_send = time (NULL);

      /*
       * Shuffle the data in the output buffer around, so that
       * new data can get stuffed into it.
       */
      if (sock->recv_buffer_fill > num_written)
        {
          memmove (sock->recv_buffer,
                   sock->recv_buffer + num_written,
                   sock->recv_buffer_fill - num_written);
        }
      sock->recv_buffer_fill -= num_written;
      http->contentlength -= num_written;
    }

  /*
   * If we have written all data to the CGI stdin, we can now start
   * reading from the CGI's stdout and write again to the http
   * connection.
   */
  if (http->contentlength <= 0)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "cgi: post data sent to cgi\n");
#endif
      sock->userflags &= ~HTTP_FLAG_POST;
      sock->flags &= ~SVZ_SOFLG_SEND_PIPE;
      sock->userflags |= HTTP_FLAG_CGI;
      sock->flags |= SVZ_SOFLG_RECV_PIPE;
      sock->read_socket = http_cgi_read;
      sock->write_socket = http_default_write;
    }

  /*
   * Return a non-zero value if an error occurred.
   */
  return (num_written < 0) ? -1 : 0;
}

/* A CGI URI has the form: DIR "/" FILENAME [PATH-INFO] [NAME-VALUE-PAIRS],
   where DIR ends with '/', PATH-INFO is optional and begins with '/',
   and NAME-VALUE-PAIRS is optional and begins with '?'.  */
struct details
{
  char *script;
  char *filename;
  char *path_info;
  char *nv_pairs;
};

static void
clear_details (struct details *det)
{
  svz_free (det->script);
  svz_free (det->filename);
  /* Don't ‘free’ the other members; they all point internally.  */
  memset (det, 0 , sizeof (struct details));
}

/*
 * Create the environment block for a CGI script.  Depending on the
 * system the environment is a field of null terminated char pointers
 * (for Unices) followed by a null pointer or one char pointer where
 * the variables a separated by zeros and the block is terminated
 * by a further zero.  It returns the amount of defined variables.
 */
static int
cgi_create_envp (svz_socket_t *sock,  /* socket for this request */
                 svz_envblock_t *env, /* env block */
                 struct details *det, /* filename, path-info, nv-pairs */
                 int type)            /* the cgi type */
{
  char buf[64];
  http_socket_t *http;
  http_config_t *cfg = sock->cfg;

  /* request type identifiers */
  static char request_type[2][5] = { "POST", "GET" };

  /*
   * This data field is needed for the conversion of http request
   * properties into environment variables.
   */
  static struct
  {
    char *property; /* property identifier */
    char *env;      /* variable identifier */
  }
  env_var[] =
  {
    { "Content-length",  "CONTENT_LENGTH"       },
    { "Content-type",    "CONTENT_TYPE"         },
    { "Accept",          "HTTP_ACCEPT"          },
    { "Referer",         "HTTP_REFERER"         },
    { "User-Agent",      "HTTP_USER_AGENT"      },
    { "Host",            "HTTP_HOST"            },
    { "Connection",      "HTTP_CONNECTION"      },
    { "Accept-Encoding", "HTTP_ACCEPT_ENCODING" },
    { "Accept-Language", "HTTP_ACCEPT_LANGUAGE" },
    { "Accept-Charset",  "HTTP_ACCEPT_CHARSET"  },
    { NULL, NULL }
  };

  unsigned n;
  int c;

  /* setup default environment */
  svz_envblock_default (env);

  /* get http socket structure */
  http = sock->data;

  /* convert some http request properties into environment variables */
  if (http->property)
    for (c = 0; env_var[c].property; c++)
      {
        char *prop = env_var[c].property;
        size_t plen = strlen (prop);

        for (n = 0; http->property[n]; n += 2)
          if (!strncasecmp (http->property[n], prop, plen))
            {
              svz_envblock_add (env, "%s=%s",
                                env_var[c].env, http->property[n + 1]);
              break;
            }
      }

  /*
   * set up some more environment variables which might be
   * necessary for the cgi script
   */
  svz_envblock_add (env, "SERVER_NAME=%s", cfg->host ? cfg->host :
                    SVZ_PP_ADDR (buf, sock->local_addr));
  svz_envblock_add (env, "SERVER_PORT=%u", ntohs (sock->local_port));
  svz_envblock_add (env, "REMOTE_ADDR=%s", http->host ? http->host :
                    SVZ_PP_ADDR (buf, sock->remote_addr));
  svz_envblock_add (env, "REMOTE_PORT=%u", ntohs (sock->remote_port));
  svz_envblock_add (env, "SCRIPT_NAME=%s", det->script);
  svz_envblock_add (env, "GATEWAY_INTERFACE=%s", CGI_VERSION);
  svz_envblock_add (env, "SERVER_PROTOCOL=%s", HTTP_VERSION);
  svz_envblock_add (env, "SERVER_SOFTWARE=%s", SERVER_STRING);
  svz_envblock_add (env, "REQUEST_METHOD=%s", request_type[type]);

  /*
   * put PATH_INFO and QUERY_STRING into the env block
   */
#define JAM(env_var_name,member_name)                   \
  svz_envblock_add                                      \
    (env, "%s=%s",                                      \
     #env_var_name,                                     \
     det-> member_name ? det-> member_name : "")
  JAM (PATH_INFO, path_info);
  JAM (QUERY_STRING, nv_pairs);
#undef JAM

  return env->size;
}

/*
 * Check the http option (the URL) for a cgi request.  This routine
 * parses the text of the request and fills in the ‘struct details’ if
 * possible.  This function makes sure that the cgi script file exists
 * and is executable.  Return 0 on success, otherwise -1.
 */
static int
check_cgi (svz_socket_t *sock, char *request, struct details *det)
{
#ifndef __MINGW32__
  struct stat buf;
#endif
  int fd;
  char *p, *fn;
  int len;
  http_config_t *cfg = sock->cfg;

  memset (det, 0, sizeof (struct details));

  /* check if the request is a real cgi request */
  if (strstr (request, cfg->cgiurl) != request)
    {
      return -1;
    }

  /*
   * skip the CGI url and concate the script file itself, then
   * check for trailing path-info and nv-pairs
   */

  fn = request + strlen (cfg->cgiurl);
  p = fn + 1;                           /* don't discard '/' (slash) */
  while (*p
         && '?' != *p
         && '/' != *p)
    p++;

  /* script */
  det->script = svz_malloc (p - request + 1);
  memcpy (det->script, request, p - request);
  det->script[p - request] = '\0';

  /* filename */
  len = strlen (cfg->cgidir);
  det->filename = svz_malloc (len + p - fn + 1);
  memcpy (det->filename, cfg->cgidir, len);
  memcpy (det->filename + len, fn, p - fn);
  det->filename[len + p - fn] = '\0';

  /* path-info */
  if ('/' == *p)
    {
      det->path_info = p;
      while (*p && '?' != *p)
        p++;
    }
  /* nv-pairs */
  if ('?' == *p)
    {
      /* This modifies ‘request’, rendering it non-‘const’-able;
         totally unfunctional, dude.  How can you sleep?  */
      *p = '\0';
      det->nv_pairs = 1 + p;
    }

  /* test if the file really exists and close it again */
  if ((fd = open (det->filename, O_RDONLY)) == -1)
    {
      svz_log_sys_error ("cgi: open (%s)", det->filename);
      clear_details (det);
      return -1;
    }

#ifndef __MINGW32__
  /* test the file being an executable */
  if (fstat (fd, &buf) == -1)
    {
      svz_log_sys_error ("cgi: fstat");
      close (fd);
      clear_details (det);
      return -1;
    }

  if (!(buf.st_mode & S_IFREG) ||
      !(buf.st_mode & S_IXUSR) || !(buf.st_mode & S_IRUSR))
    {
      svz_log (SVZ_LOG_ERROR, "cgi: no executable: %s\n", det->filename);
      close (fd);
      clear_details (det);
      return -1;
    }
#endif
  if (close (fd) == -1)
    svz_log_sys_error ("cgi: close");

  return 0;
}

/*
 * Prepare the invocation of a cgi script which means to change to
 * the referred directory and the creation of a valid environment
 * block.  Return a NULL pointer on errors or a pointer to the full
 * cgi file (including the path).  This MUST be freed afterwards.
 */
static char *
pre_exec (svz_socket_t *sock,   /* socket structure */
          svz_envblock_t *envp, /* environment block to be filled */
          struct details *det,  /* filename, path-info, nv-pairs */
          int type)             /* POST or GET?  */
{
  char *cgidir;
  char *cgifile;
  http_config_t *cfg = sock->cfg;

  /* change into the CGI directory temporarily */
  if (chdir (cfg->cgidir) == -1)
    {
      svz_log_sys_error ("cgi: chdir");
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "cgi: cannot change dir: %s\n", cfg->cgidir);
#endif
      return NULL;
    }

  /* get the current directory  */
  cgidir = svz_getcwd ();

  /* put the directory and file together */
  cgifile = svz_malloc (strlen (cgidir) + strlen (det->filename) + 1);
  sprintf (cgifile, "%s%s", cgidir, det->filename + strlen (cfg->cgidir));
  svz_free (cgidir);

  /* create the environment block for the CGI script */
  cgi_create_envp (sock, envp, det, type);

  return cgifile;
}

/*
 * Write an initial HTTP response header to the socket SOCK
 * right after the the actual CGI script has been invoked.
 */
int
http_cgi_accepted (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;

  http->response = 202;
  return svz_sock_printf (sock, HTTP_OK
                          "Date: %s\r\n"
                          "Server: %s\r\n"
                          "Connection: close\r\n",
                          http_asc_date (time (NULL)),
                          SERVER_STRING);
}

/*
 * This routine generates some standard cgi associations.
 */
#define DEFAULT_CGIAPP "default"
void
http_gen_cgi_apps (http_config_t *cfg)
{
  char *p;

  /* create the cgi association hash table if necessary */
  if (cfg->cgiapps == NULL)
    cfg->cgiapps = svz_hash_create (4, svz_free);

  /* the associations need to be in the hash to be executed at all */
  if ((p = svz_hash_put (cfg->cgiapps, "exe", svz_strdup (DEFAULT_CGIAPP)))
      != NULL)
    svz_free (p);
  if ((p = svz_hash_put (cfg->cgiapps, "com", svz_strdup (DEFAULT_CGIAPP)))
      != NULL)
    svz_free (p);
  if ((p = svz_hash_put (cfg->cgiapps, "bat", svz_strdup (DEFAULT_CGIAPP)))
      != NULL)
    svz_free (p);
}

/* FIXME: Make ‘static inline’ func w/ attribute ‘SVZ_EXITING’.  */
#define cool()  exit (EXIT_SUCCESS)

/*
 * Invoke a cgi script.  In Unices we ‘fork’ us and in Win32 we
 * ‘CreateProcess’.
 */
static int
cgi_exec (svz_socket_t *sock,  /* the socket structure */
          svz_t_handle in,     /* here the cgi reads from or NULL if GET */
          svz_t_handle out,    /* here the cgi writes to */
          struct details *det, /* filename, path-info, nv-pairs */
          int type)            /* request type (POST or GET) */
{
  svz_t_handle pid; /* the pid from ‘fork’ or the process handle in Win32 */
  char *cgifile;    /* path including the name of the cgi script */
  http_socket_t *http;
  svz_envblock_t *envp;

#ifdef __MINGW32__
  http_config_t *cfg = sock->cfg;
  STARTUPINFO StartupInfo;         /* store here the inherited handles */
  PROCESS_INFORMATION ProcessInfo; /* where we get the process handle from */
  char *savedir;                   /* save the original directory */
  char *suffix, *p;
  char *cgiapp;
#else
  char *argv[2];
  struct stat buf;
  int retries;
  int oflags;
#endif

  /* Assign local CGI disconnection routine.  */
  sock->disconnected_socket = http_cgi_disconnect;

#ifdef __MINGW32__
  /*
   * Clean the StartupInfo, use the stdio handles, and store the
   * pipe handles there if necessary (depends on type).
   */
  memset (&StartupInfo, 0, sizeof (StartupInfo));
  StartupInfo.cb = sizeof (StartupInfo);
  StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  StartupInfo.hStdOutput = out;
  /* StartupInfo.hStdError = out; */
  if (type == POST_METHOD)
    StartupInfo.hStdInput = in;

  /* reserve buffer space for the environment block */
  envp = svz_envblock_create ();

  /* save the current directory */
  savedir = svz_getcwd ();

  if ((cgifile = pre_exec (sock, envp, det, type)) == NULL)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      chdir (savedir);
      svz_envblock_destroy (envp);
      svz_free (savedir);
      return -1;
    }

  /* find a cgi interpreter if possible */
  p = cgifile + strlen (cgifile) - 1;
  while (p != cgifile && *p != '.')
    p--;
  suffix = p + 1;

  if ((p = svz_hash_get (cfg->cgiapps, svz_tolower (suffix))) != NULL)
    {
      if (strcmp (p, DEFAULT_CGIAPP))
        {
          cgiapp = svz_malloc (strlen (cgifile) + strlen (p) + 2);
          sprintf (cgiapp, "%s %s", p, cgifile);
          svz_free (cgifile);
          cgifile = cgiapp;
        }
    }
  /* not a valid file extension */
  else
    {
      /* find an appropriate system association */
      cgiapp = svz_malloc (MAX_PATH);
      if (FindExecutable (cgifile, NULL, cgiapp) <= (HINSTANCE) 32)
        svz_log_sys_error ("FindExecutable");
#if ENABLE_DEBUG
      /* if this is enabled you could learn about the system */
      else
        svz_log (SVZ_LOG_DEBUG, "FindExecutable: %s\n", cgiapp);
#endif
      svz_free (cgiapp);

      /* print some error message */
      svz_sock_printf (sock, HTTP_ACCESS_DENIED "\r\n");
      http_error_response (sock, 403);
      sock->userflags |= HTTP_FLAG_DONE;
      chdir (savedir);
      svz_free (cgifile);
      svz_envblock_destroy (envp);
      svz_free (savedir);
      return -1;
    }

  /* send http header response */
  if (http_cgi_accepted (sock) == -1)
    {
      sock->userflags |= HTTP_FLAG_DONE;
      chdir (savedir);
      svz_free (cgifile);
      svz_envblock_destroy (envp);
      svz_free (savedir);
      return -1;
    }

  /* create the process here */
  if (!CreateProcess (NULL,                    /* ApplicationName */
                      cgifile,                 /* CommandLine */
                      NULL,                    /* ProcessAttributes */
                      NULL,                    /* ThreadAttributes */
                      TRUE,                    /* InheritHandles */
                      DETACHED_PROCESS,        /* CreationFlags */
                      svz_envblock_get (envp), /* Environment */
                      NULL,                    /* CurrentDirectory */
                      &StartupInfo, &ProcessInfo))
    {
      svz_log_sys_error ("cgi: CreateProcess");
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "cgi: cannot execute: %s\n", cgifile);
#endif
      svz_sock_printf (sock, "\r\n");
      sock->userflags |= HTTP_FLAG_DONE;
      chdir (savedir);
      svz_free (cgifile);
      svz_envblock_destroy (envp);
      svz_free (savedir);
      return -1;
    }

  /* reenter the actual directory and free reserved space */
  chdir (savedir);
  svz_free (cgifile);
  svz_envblock_destroy (envp);
  svz_free (savedir);
  pid = ProcessInfo.hProcess;

#ifdef ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "http: cgi %s got pid 0x%08X\n",
           file + 1, ProcessInfo.dwProcessId);
#endif

#else /* not __MINGW32__ */

  retries = 3;
 retry:

  /* fork us here */
  if ((pid = fork ()) == 0)
    {
      /* ------ child process here ------ */

      /* create environment block */
      envp = svz_envblock_create ();
      if ((cgifile = pre_exec (sock, envp, det, type)) == NULL)
        cool ();

      /* make the output blocking */
      if ((oflags = fcntl (out, F_GETFL)) == -1)
        {
          svz_log_sys_error ("cgi: fcntl");
          cool ();
        }
      if (fcntl (out, F_SETFL, oflags & ~O_NONBLOCK) == -1)
        {
          svz_log_sys_error ("cgi: fcntl");
          cool ();
        }

      /* duplicate the receiving pipe descriptor to stdout */
      if (dup2 (out, 1) != 1)
        {
          svz_log_sys_error ("cgi: dup2");
          cool ();
        }
#ifndef ENABLE_DEBUG
      /* duplicate stderr to the cgi output */
      if (dup2 (out, 2) != 2)
        {
          svz_log_sys_error ("cgi: dup2");
          cool ();
        }
#endif /* !ENABLE_DEBUG */

      /* handle post method */
      if (type == POST_METHOD)
        {
          /* make the input blocking */
          if ((oflags = fcntl (in, F_GETFL)) == -1)
            {
              svz_log_sys_error ("cgi: fcntl");
              cool ();
            }
          if (fcntl (in, F_SETFL, oflags & ~O_NONBLOCK) == -1)
            {
              svz_log_sys_error ("cgi: fcntl");
              cool ();
            }

          /* duplicate the sending pipe descriptor to stdin */
          if (dup2 (in, 0) != 0)
            {
              svz_log_sys_error ("cgi: dup2");
              cool ();
            }

          /* close the old file descriptors */
          if (close (in) < 0)
            svz_log_sys_error ("cgi: close");
        }
      /* close remaining stdin in get method */
      else
        {
          close (0);
        }

      /* close the old file descriptors */
      if (close (out) < 0)
        svz_log_sys_error ("cgi: close");

      /* get the cgi scripts permissions */
      if (stat (cgifile, &buf) == -1)
        {
          svz_log_sys_error ("cgi: stat");
          cool ();
        }

      /* set the appropriate user permissions */
      if (setgid (buf.st_gid) == -1)
        {
          svz_log_sys_error ("cgi: setgid");
          cool ();
        }
      if (setuid (buf.st_uid) == -1)
        {
          svz_log_sys_error ("cgi: setuid");
          cool ();
        }

      /* create the argv[] and envp[] pointers */
      argv[0] = cgifile;
      argv[1] = NULL;

      /*
       * Execute the CGI script itself here.  This will overwrite the
       * current process.
       */
      if (execve (cgifile, argv, svz_envblock_get (envp)) == -1)
        {
          svz_log_sys_error ("cgi: execve");
          cool ();
        }
    }
  else if (pid == -1)
    {
      if (errno == EAGAIN && --retries)
        {
          /* sleep (1); */
          goto retry;
        }

      /* ------ error forking new process ------ */
      svz_log_sys_error ("cgi: fork");
      svz_sock_printf (sock, HTTP_BAD_REQUEST "\r\n");
      http_error_response (sock, 400);
      sock->userflags |= HTTP_FLAG_DONE;
      return -1;
    }

  /* ------ still current (parent) process here ------ */

#ifdef ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "http: cgi %s got pid %d\n", det->filename, pid);
#endif

  /* send http header response */
  if (http_cgi_accepted (sock) == -1)
    {
      sock->userflags |= HTTP_FLAG_DONE;
      return -1;
    }

#endif /* not __MINGW32__ */

  /* save the process id */
  http = sock->data;
  http->pid = pid;

  /* close the inherited http data handles */
  if (svz_closehandle (out) == -1)
    {
      svz_log_sys_error ("cgi: close");
    }
  if (type == POST_METHOD)
    {
      /* close the reading end of the pipe for the post data */
      if (svz_closehandle (in) == -1)
        {
          svz_log_sys_error ("cgi: close");
        }
    }

  return 0;
}

#define LOSE()  rv = -1; goto out

/*
 * The http GET cgi request response.
 */
int
http_cgi_get_response (svz_socket_t *sock, char *request,
                       UNUSED int flags)
{
  svz_t_handle dummy;
  svz_t_handle cgi2s[2];
  struct details det;
  int rv;

  /* check if this is a cgi request at all */
  if (0 > check_cgi (sock, request, &det))
    return -2;

  rv = 0;
  if (! det.filename)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      LOSE ();
    }

  /* create a pipe for the cgi script process */
  if (svz_pipe_create_pair (cgi2s) == -1)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      LOSE ();
    }

  /* execute the cgi script in FILE */
  sock->userflags |= HTTP_FLAG_CGI;
  sock->flags |= SVZ_SOFLG_RECV_PIPE;
  sock->read_socket = http_cgi_read;
  sock->pipe_desc[SVZ_READ] = cgi2s[SVZ_READ];
  svz_fd_cloexec ((int) cgi2s[SVZ_READ]);

  svz_invalidate_handle (&dummy);
  if (cgi_exec (sock, dummy, cgi2s[SVZ_WRITE], &det, GET_METHOD))
    {
      /* some error occurred here */
      sock->read_socket = svz_tcp_read_socket;
      LOSE ();
    }

 out:
  clear_details (&det);
  return rv;
}

/*
 * The http POST request response.
 */
int
http_post_response (svz_socket_t *sock, char *request,
                    UNUSED int flags)
{
  struct details det;
  char *length;
  svz_t_handle s2cgi[2];
  svz_t_handle cgi2s[2];
  http_socket_t *http;
  int rv = 0;

  /* get http socket structure */
  http = sock->data;

  /* is this a valid POST request?  */
  if (0 > check_cgi (sock, request, &det) || !det.filename)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      LOSE ();
    }

  /* create a pair of pipes for the cgi script process */
  if (svz_pipe_create_pair (cgi2s) == -1)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      LOSE ();
    }
  if (svz_pipe_create_pair (s2cgi) == -1)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      LOSE ();
    }

  /* get the content length from the header information */
  if ((length = http_find_property (http, "Content-length")) == NULL)
    {
      svz_sock_printf (sock, HTTP_BAD_REQUEST "\r\n");
      http_error_response (sock, 411);
      sock->userflags |= HTTP_FLAG_DONE;
      LOSE ();
    }
  http->contentlength = svz_atoi (length);

  /* prepare everything for the cgi pipe handling */
  sock->pipe_desc[SVZ_WRITE] = s2cgi[SVZ_WRITE];
  sock->pipe_desc[SVZ_READ] = cgi2s[SVZ_READ];
  svz_fd_cloexec ((int) s2cgi[SVZ_WRITE]);
  svz_fd_cloexec ((int) cgi2s[SVZ_READ]);

  /* execute the cgi script in FILE */
  if (cgi_exec (sock, s2cgi[SVZ_READ], cgi2s[SVZ_WRITE], &det, POST_METHOD))
    {
      /* some error occurred here */
      sock->read_socket = svz_tcp_read_socket;
      sock->write_socket = http_default_write;
      LOSE ();
    }

  sock->write_socket = http_cgi_write;
  sock->flags |= SVZ_SOFLG_SEND_PIPE;
  sock->userflags |= HTTP_FLAG_POST;

 out:
  clear_details (&det);
  return rv;
}
