/*
 * server-loop.c - server loop implementation
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

#ifdef _AIX
# undef _NO_PROTO
# ifndef _USE_IRS
#  define _USE_IRS 1
# endif
# define _XOPEN_SOURCE_EXTENDED 1
#endif /* _AIX */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>

#if HAVE_FLOSS_H
# include <floss.h>
#endif
#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#include <sys/types.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_POLL_H
# include <sys/poll.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif

#include "networking-headers.h"
#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/pipe-socket.h"
#include "libserveez/server-core.h"

#define USE_POLL  (HAVE_POLL && ENABLE_POLL)

#define SOCK_FILE_FUNCTIONALITY(sock) do {                 \
  /* If socket is a file descriptor, then read it here.  */\
  if (sock->flags & SVZ_SOFLG_FILE)                        \
    if (sock->read_socket)                                 \
      if (sock->read_socket (sock))                        \
        svz_sock_schedule_for_shutdown (sock);             \
  } while (0)


#define SOCK_TRIGGER_FUNCTIONALITY(sock) do {    \
  /* Issue the trigger funcionality.  */         \
  if (sock->trigger_cond)                        \
    if (sock->trigger_cond (sock))               \
      if (sock->trigger_func)                    \
        if (sock->trigger_func (sock))           \
          svz_sock_schedule_for_shutdown (sock); \
  } while (0)


#define SOCK_READABLE(sock)                                \
  (!((sock)->flags & SVZ_SOFLG_NOOVERFLOW) ||              \
   ((sock)->recv_buffer_fill < (sock)->recv_buffer_size && \
    (sock)->recv_buffer_size > 0))

/*
 * Get and clear the pending socket error of a given socket.  Print
 * the result to the log file.
 */
static int
error_info (svz_socket_t *sock)
{
  int error;
  socklen_t optlen = sizeof (int);

  if (getsockopt (sock->sock_desc, SOL_SOCKET, SO_ERROR,
                  (void *) &error, &optlen) < 0)
    {
      svz_log_net_error ("getsockopt");
      return -1;
    }
  if (error)
    {
#ifdef __MINGW32__
      WSASetLastError (error);
#else
      errno = error;
#endif
      svz_log_net_error ("socket error");
      return -1;
    }
  return 0;
}

#if !defined __MINGW32__ && !USE_POLL

/*
 * Check the server and client sockets for incoming connections
 * and data, and process outgoing data.
 */
static int
check_sockets_select (void)
{
  int nfds;                     /* count of file descriptors to check */
  fd_set read_fds;              /* bitmasks for file descriptors to check */
  fd_set write_fds;             /* ditto */
  fd_set except_fds;            /* ditto */
  struct timeval wait;          /* used for timeout in ‘select’ */
  svz_socket_t *sock;

  /*
   * Prepare the file handle sets for the ‘select’ call below.
   */
  FD_ZERO (&read_fds);
  FD_ZERO (&except_fds);
  FD_ZERO (&write_fds);
  nfds = 0;

  /*
   * Here we set the bitmaps for all clients we handle.
   */
  svz_sock_foreach (sock)
    {
      /* Put only those SOCKs into fd set not yet killed and skip files.  */
      if (sock->flags & SVZ_SOFLG_KILLED)
        continue;

      /* If socket is a file descriptor, then read it here.  */
      SOCK_FILE_FUNCTIONALITY (sock);

      /* Issue the trigger funcionality.  */
      SOCK_TRIGGER_FUNCTIONALITY (sock);

      /* Handle pipes.  */
      if (sock->flags & SVZ_SOFLG_PIPE)
        {
          /* Do not handle listening pipes here.  */
          if (sock->flags & SVZ_SOFLG_LISTENING)
            continue;

          /*
           * Put a pipe's descriptor into WRITE and EXCEPT set.
           */
          if (sock->flags & SVZ_SOFLG_SEND_PIPE)
            {
              FD_SET (sock->pipe_desc[SVZ_WRITE], &except_fds);
              if (sock->pipe_desc[SVZ_WRITE] > nfds)
                nfds = sock->pipe_desc[SVZ_WRITE];
              if (sock->send_buffer_fill > 0)
                FD_SET (sock->pipe_desc[SVZ_WRITE], &write_fds);
            }

          /*
           * Put a pipe's descriptor into READ set for getting data.
           */
          if (sock->flags & SVZ_SOFLG_RECV_PIPE)
            {
              FD_SET (sock->pipe_desc[SVZ_READ], &except_fds);
              if (SOCK_READABLE (sock))
                FD_SET (sock->pipe_desc[SVZ_READ], &read_fds);
              if (sock->pipe_desc[SVZ_READ] > nfds)
                nfds = sock->pipe_desc[SVZ_READ];
            }
        }

      /* Handle sockets.  */
      if (sock->flags & SVZ_SOFLG_SOCK)
        {
          /* Is the socket descriptor currently unavailable?  */
          if (sock->unavailable)
            {
              if (time (NULL) >= sock->unavailable)
                sock->unavailable = 0;
            }

          /* Put every client's socket into EXCEPT.  */
          FD_SET (sock->sock_desc, &except_fds);
          if (sock->sock_desc > (svz_t_socket) nfds)
            nfds = sock->sock_desc;

          /* Put socket into READ if necessary.  */
          if (!(sock->flags & SVZ_SOFLG_CONNECTING))
            if (SOCK_READABLE (sock))
              FD_SET (sock->sock_desc, &read_fds);

          /* Put a socket into WRITE if necessary and possible.  */
          if (!sock->unavailable && (sock->send_buffer_fill > 0 ||
                                     sock->flags & SVZ_SOFLG_CONNECTING))
            {
              FD_SET (sock->sock_desc, &write_fds);
            }
        }
    }

  nfds++;

  /*
   * Adjust timeout value, so we won't wait longer than we want.
   */
  wait.tv_sec = svz_notify - time (NULL);
  if (wait.tv_sec < 0)
    wait.tv_sec = 0;
  wait.tv_usec = 0;

  if ((nfds = select (nfds, &read_fds, &write_fds, &except_fds, &wait)) <= 0)
    {
      if (nfds < 0)
        {
          svz_log_net_error ("select");
          if (errno == EBADF)
            svz_sock_check_bogus ();
          return -1;
        }
      else
        {
          /*
           * ‘select’ timed out, so we can do some administrative stuff.
           */
          svz_periodic_tasks ();
        }
    }

  /*
   * Go through all enqueued SOCKs and check if these have been
   * ‘select’ed or could be handle in any other way.
   */
  svz_sock_foreach (sock)
    {
      if (sock->flags & SVZ_SOFLG_KILLED)
        continue;

      /* Handle pipes.  */
      if (sock->flags & SVZ_SOFLG_PIPE)
        {
          /* Make listening pipe servers listen.  */
          if (sock->flags & SVZ_SOFLG_LISTENING)
            {
              if (!(sock->flags & SVZ_SOFLG_INITED))
                if (sock->read_socket)
                  if (sock->read_socket (sock))
                    svz_sock_schedule_for_shutdown (sock);
              continue;
            }

          /* Handle receiving pipes.  */
          if (sock->flags & SVZ_SOFLG_RECV_PIPE)
            {
              if (FD_ISSET (sock->pipe_desc[SVZ_READ], &except_fds))
                {
                  svz_log (SVZ_LOG_ERROR, "exception on receiving pipe %d \n",
                           sock->pipe_desc[SVZ_READ]);
                  svz_sock_schedule_for_shutdown (sock);
                }
              if (FD_ISSET (sock->pipe_desc[SVZ_READ], &read_fds))
                {
                  if (sock->read_socket)
                    if (sock->read_socket (sock))
                      svz_sock_schedule_for_shutdown (sock);
                }
            }

          /* Handle sending pipes.  */
          if (sock->flags & SVZ_SOFLG_SEND_PIPE)
            {
              if (FD_ISSET (sock->pipe_desc[SVZ_WRITE], &except_fds))
                {
                  svz_log (SVZ_LOG_ERROR, "exception on sending pipe %d \n",
                           sock->pipe_desc[SVZ_WRITE]);
                  svz_sock_schedule_for_shutdown (sock);
                }
              if (FD_ISSET (sock->pipe_desc[SVZ_WRITE], &write_fds))
                {
                  if (sock->write_socket)
                    if (sock->write_socket (sock))
                      svz_sock_schedule_for_shutdown (sock);
                }
            }
        }

      /* Handle usual sockets.  Socket in the exception set?  */
      if (sock->flags & SVZ_SOFLG_SOCK)
        {
          if (FD_ISSET (sock->sock_desc, &except_fds))
            {
              if (sock->flags & SVZ_SOFLG_CONNECTING)
                {
                  svz_log (SVZ_LOG_ERROR, "exception connecting socket %d\n",
                           sock->sock_desc);
                  error_info (sock);
                  svz_sock_schedule_for_shutdown (sock);
                  continue;
                }
              else
                {
                  /* Handle out-of-band data correctly.  */
                  if (sock->read_socket_oob)
                    if (sock->read_socket_oob (sock))
                      {
                        svz_sock_schedule_for_shutdown (sock);
                        continue;
                      }
                }
            }

          /* Is socket readable?  */
          if (FD_ISSET (sock->sock_desc, &read_fds))
            {
              if (sock->read_socket)
                if (sock->read_socket (sock))
                  {
                    svz_sock_schedule_for_shutdown (sock);
                    continue;
                  }
            }

          /* Is socket writable?  */
          if (FD_ISSET (sock->sock_desc, &write_fds))
            {
              /* Socket connecting?  */
              if (sock->flags & SVZ_SOFLG_CONNECTING)
                {
                  if (sock->connected_socket)
                    if (sock->connected_socket (sock))
                      {
                        svz_sock_schedule_for_shutdown (sock);
                        continue;
                      }
                }
              /* No.  Just writable.  */
              else
                {
                  if (sock->write_socket)
                    if (sock->write_socket (sock))
                      {
                        svz_sock_schedule_for_shutdown (sock);
                        continue;
                      }
                }
            }
        }
    }

  /*
   * We had no chance to time out so we have to explicitly call the
   * timeout handler.
   */
  if (time (NULL) > svz_notify)
    {
      svz_periodic_tasks ();
    }

  return 0;
}

#endif  /* !defined __MINGW32__ && !USE_POLL*/

#if USE_POLL

/* re-allocate static buffers if necessary */
#define FD_EXPAND()                                                 \
  if (nfds >= max_nfds) {                                           \
    max_nfds++;                                                     \
    ufds = svz_prealloc (ufds, sizeof (struct pollfd) * max_nfds);  \
    memset (&ufds[max_nfds-1], 0, sizeof (struct pollfd));          \
    sfds = svz_prealloc (sfds, sizeof (svz_socket_t *) * max_nfds); \
    memset (&sfds[max_nfds-1], 0, sizeof (svz_socket_t *));         \
  }                                                                 \

/* check for incoming data */
#define FD_POLL_IN(fd, sock)                 \
  {                                          \
    FD_EXPAND();                             \
    ufds[nfds].fd = fd;                      \
    ufds[nfds].events |= (POLLIN | POLLPRI); \
    sfds[nfds] = sock;                       \
  }                                          \

/* process outgoing data */
#define FD_POLL_OUT(fd, sock)     \
  {                               \
    FD_EXPAND();                  \
    ufds[nfds].fd = fd;           \
    ufds[nfds].events |= POLLOUT; \
    sfds[nfds] = sock;            \
  }                               \

/* clear polling buffers */
#define FD_POLL_CLR(ufds, sfds)                           \
  if (ufds) {                                             \
    memset (ufds, 0, sizeof (struct pollfd) * max_nfds);  \
    memset (sfds, 0, sizeof (svz_socket_t *) * max_nfds); \
  }                                                       \

/*
 * Same routine as the above ‘check_sockets_select’ routine.  Here we are
 * using ‘poll’ instead of ‘select’.  This might solve the builtin file
 * descriptor limit of the (g)libc ‘select’.  This routine will NOT be
 * available under Win32.
 */
static int
check_sockets_poll (void)
{
  static unsigned int max_nfds = 0;   /* maximum number of file descriptors */
  unsigned int nfds, fd;              /* number of fds */
  static struct pollfd *ufds = NULL;  /* poll fd array */
  static svz_socket_t **sfds = NULL;  /* referring socket structures */
  int timeout;                        /* timeout in milliseconds */
  int polled;                         /* amount of polled fds */
  svz_socket_t *sock;                 /* socket structure */

  /* clear polling structures */
  nfds = 0;
  FD_POLL_CLR (ufds, sfds);

  /* go through all sockets */
  svz_sock_foreach (sock)
    {
      /* skip already killed sockets */
      if (sock->flags & SVZ_SOFLG_KILLED)
        continue;

      /* process files */
      SOCK_FILE_FUNCTIONALITY (sock);

      /* issue the trigger funcionality */
      SOCK_TRIGGER_FUNCTIONALITY (sock);

      /* process pipes */
      if (sock->flags & SVZ_SOFLG_PIPE)
        {
          /* handle listening pipe */
          if (sock->flags & SVZ_SOFLG_LISTENING)
            {
              if (!(sock->flags & SVZ_SOFLG_INITED))
                if (sock->read_socket)
                  if (sock->read_socket (sock))
                    svz_sock_schedule_for_shutdown (sock);
              continue;
            }

          /* send pipe?  */
          if (sock->flags & SVZ_SOFLG_SEND_PIPE)
            {
              if (sock->send_buffer_fill > 0)
                {
                  fd = sock->pipe_desc[SVZ_WRITE];
                  FD_POLL_OUT (fd, sock);
                  nfds++;
                }
            }

          /* receive pipe?  */
          if (sock->flags & SVZ_SOFLG_RECV_PIPE)
            {
              if (SOCK_READABLE (sock))
                {
                  fd = sock->pipe_desc[SVZ_READ];
                  FD_POLL_IN (fd, sock);
                  nfds++;
                }
            }
        }

      /* normal bi-directional socket connection */
      if (sock->flags & SVZ_SOFLG_SOCK)
        {
          int polled = 0;

          /* process lingering connection counter */
          if (sock->unavailable)
            {
              if (time (NULL) >= sock->unavailable)
                sock->unavailable = 0;
            }

          /* poll this socket for reading and writing */
          fd = sock->sock_desc;
          if (!(sock->flags & SVZ_SOFLG_CONNECTING))
            {
              if (SOCK_READABLE (sock))
                {
                  FD_POLL_IN (fd, sock);
                  polled = 1;
                }
            }
          if (!sock->unavailable && (sock->send_buffer_fill > 0 ||
                                     sock->flags & SVZ_SOFLG_CONNECTING))
            {
              FD_POLL_OUT (fd, sock);
              polled = 1;
            }
          nfds += polled;
        }
    }

  /* calculate timeout value */
  timeout = (svz_notify - time (NULL)) * 1000;
  if (timeout < 0)
    timeout = 0;

  /* now ‘poll’ everything */
  if ((polled = poll (ufds, nfds, timeout)) <= 0)
    {
      if (polled < 0)
        {
          svz_log_net_error ("poll");
          if (errno == EBADF)
            svz_sock_check_bogus ();
          return -1;
        }
      else
        {
          svz_periodic_tasks ();
        }
    }

  /* go through all ‘poll’ed structures */
  for (fd = 0; fd < nfds && polled != 0; fd++)
    {
      /* count down the number of polled descriptors */
      if (ufds[fd].revents != 0)
        polled--;

      sock = sfds[fd];
      /* do not process killed connections */
      if (sock->flags & SVZ_SOFLG_KILLED)
        continue;

      /* urgent data (out-of-band) on the file descriptor?
         IMPORTANT note: POLLPRI + recv(...,MSG_OOB) *before* anything else!
         Otherwise you'll miss the out-of-band data byte.  */
      if (ufds[fd].revents & POLLPRI)
        if (sock->read_socket_oob)
          if (sock->read_socket_oob (sock))
            {
              svz_sock_schedule_for_shutdown (sock);
              continue;
            }

      /* file descriptor ready for reading?  */
      if (ufds[fd].revents & POLLIN)
        {
          if (sock->read_socket)
            if (sock->read_socket (sock))
              {
                svz_sock_schedule_for_shutdown (sock);
                continue;
              }
        }

      /* file descriptor ready for writing */
      if (ufds[fd].revents & POLLOUT)
        {
          /* socket connected?  */
          if (sock->flags & SVZ_SOFLG_CONNECTING)
            {
              if (sock->connected_socket)
                if (sock->connected_socket (sock))
                  {
                    svz_sock_schedule_for_shutdown (sock);
                    continue;
                  }
            }
          /* ready for writing */
          else
            {
              if (sock->write_socket)
                if (sock->write_socket (sock))
                  {
                    svz_sock_schedule_for_shutdown (sock);
                    continue;
                  }
            }
        }

      /* file descriptor caused some error */
      if (ufds[fd].revents & (POLLERR | POLLHUP | POLLNVAL))
        {
          if (sock->flags & SVZ_SOFLG_SOCK)
            {
              if (sock->flags & SVZ_SOFLG_CONNECTING)
                {
                  svz_log (SVZ_LOG_ERROR, "exception connecting socket %d\n",
                           sock->sock_desc);
                }
              else
                {
                  svz_log (SVZ_LOG_ERROR, "exception on socket %d\n",
                           sock->sock_desc);
                }
              error_info (sock);
              svz_sock_schedule_for_shutdown (sock);
            }
          if (sock->flags & SVZ_SOFLG_RECV_PIPE)
            {
              svz_log (SVZ_LOG_ERROR, "exception on receiving pipe %d \n",
                       sock->pipe_desc[SVZ_READ]);
              svz_sock_schedule_for_shutdown (sock);
            }
          if (sock->flags & SVZ_SOFLG_SEND_PIPE)
            {
              svz_log (SVZ_LOG_ERROR, "exception on sending pipe %d \n",
                       sock->pipe_desc[SVZ_WRITE]);
              svz_sock_schedule_for_shutdown (sock);
            }
        }
    }

  /* handle regular tasks ...  */
  if (time (NULL) > svz_notify)
    {
      svz_periodic_tasks ();
    }

  return 0;
}

#endif  /* USE_POLL */

#ifdef __MINGW32__

/*
 * This is the specialized routine for this Win32 port.
 */
static int
check_sockets_mingw (void)
{
  int nfds;                     /* count of file descriptors to check */
  fd_set read_fds;              /* bitmasks for file descriptors to check */
  fd_set write_fds;             /* ditto */
  fd_set except_fds;            /* ditto */
  struct timeval wait;          /* used for timeout in ‘select’ */
  svz_socket_t *sock;

  /*
   * Prepare the file handle sets for the ‘select’ call below.
   */
  FD_ZERO (&read_fds);
  FD_ZERO (&except_fds);
  FD_ZERO (&write_fds);
  nfds = 0;

  /*
   * Here we set the bitmaps for all clients we handle.
   */
  svz_sock_foreach (sock)
    {
      /* Put only those SOCKs into fd set not yet killed and skip files.  */
      if (sock->flags & SVZ_SOFLG_KILLED)
        continue;


      /* If socket is a file descriptor, then read it here.  */
      SOCK_FILE_FUNCTIONALITY (sock);

      /* Issue the trigger funcionality.  */
      SOCK_TRIGGER_FUNCTIONALITY (sock);

      /* Handle pipes.  */
      if (sock->flags & SVZ_SOFLG_PIPE)
        {
          /* Do not handle listening pipes here.  */
          if (sock->flags & SVZ_SOFLG_LISTENING)
            continue;

          /*
           * Handle receiving pipes.  Is non-blocking, but cannot
           * be ‘select’ed.
           */
          if (sock->flags & SVZ_SOFLG_RECV_PIPE)
            {
              if (SOCK_READABLE (sock))
                if (sock->read_socket)
                  if (sock->read_socket (sock))
                    svz_sock_schedule_for_shutdown (sock);
            }
        }

      if (sock->flags & SVZ_SOFLG_SOCK)
        {
          /* Is the socket descriptor currently unavailable?  */
          if (sock->unavailable)
            {
              if (time (NULL) >= sock->unavailable)
                sock->unavailable = 0;
            }

          /* Put every client's socket into EXCEPT.  */
          FD_SET (sock->sock_desc, &except_fds);
          if (sock->sock_desc > (svz_t_socket) nfds)
            nfds = sock->sock_desc;

          /* Put a client's socket into READ if necessary.  */
          if (!(sock->flags & SVZ_SOFLG_CONNECTING))
            if (SOCK_READABLE (sock))
              FD_SET (sock->sock_desc, &read_fds);

          /* Put a socket into WRITE if necessary and possible.  */
          if (!sock->unavailable && (sock->send_buffer_fill > 0 ||
                                     sock->flags & SVZ_SOFLG_CONNECTING))
            {
              FD_SET (sock->sock_desc, &write_fds);
            }
        }
    }

  nfds++;

  /*
   * Adjust timeout value, so we won't wait longer than we want.
   */
  wait.tv_sec = svz_notify - time (NULL);
  if (wait.tv_sec < 0)
    wait.tv_sec = 0;
  wait.tv_usec = 0;

  /* Just sleep a bit if there is no file descriptor to be ‘select’ed.  */
  if (nfds < 2)
    Sleep (1);

  else if ((nfds = select (nfds, &read_fds, &write_fds, &except_fds, &wait))
           <= 0)
    {
      if (nfds < 0)
        {
          svz_log_net_error ("select");
          /* FIXME: What value do we choose here?  */
          if (svz_errno != 0)
            svz_sock_check_bogus ();
          return -1;
        }
      else
        {
          /*
           * ‘select’ timed out, so we can do some administrative stuff.
           */
          svz_periodic_tasks ();
        }
    }

  /*
   * Go through all enqueued SOCKs and check if these have been
   * ‘select’ed or could be handle in any other way.
   */
  svz_sock_foreach (sock)
    {
      if (sock->flags & SVZ_SOFLG_KILLED)
        continue;

      /* Handle pipes.  Different in Win32 and Unices.  */
      if (sock->flags & SVZ_SOFLG_PIPE)
        {
          /* Make listening pipe servers listen.  */
          if (sock->flags & SVZ_SOFLG_LISTENING)
            {
              if (!(sock->flags & SVZ_SOFLG_INITED))
                if (sock->read_socket)
                  if (sock->read_socket (sock))
                    svz_sock_schedule_for_shutdown (sock);
              continue;
            }

          /* Handle sending pipes.  Is blocking!  */
          if (sock->flags & SVZ_SOFLG_SEND_PIPE)
            {
              if (sock->send_buffer_fill > 0)
                if (sock->write_socket)
                  if (sock->write_socket (sock))
                    svz_sock_schedule_for_shutdown (sock);
            }
        }

      /* Handle usual sockets. Socket in the exception set?  */
      if (sock->flags & SVZ_SOFLG_SOCK)
        {
          if (FD_ISSET (sock->sock_desc, &except_fds))
            {
              if (sock->flags & SVZ_SOFLG_CONNECTING)
                {
                  svz_log (SVZ_LOG_ERROR, "exception connecting socket %d\n",
                           sock->sock_desc);
                  error_info (sock);
                  svz_sock_schedule_for_shutdown (sock);
                  continue;
                }
              else
                {
                  /* Handle out-of-band data correctly.  */
                  if (sock->read_socket_oob)
                    if (sock->read_socket_oob (sock))
                      {
                        svz_sock_schedule_for_shutdown (sock);
                        continue;
                      }
                }
            }

          /* Is socket readable?  */
          if (FD_ISSET (sock->sock_desc, &read_fds))
            {
              if (sock->read_socket)
                if (sock->read_socket (sock))
                  {
                    svz_sock_schedule_for_shutdown (sock);
                    continue;
                  }
            }

          /* Is socket writable?  */
          if (FD_ISSET (sock->sock_desc, &write_fds))
            {
              /* Finally connected * */
              if (sock->flags & SVZ_SOFLG_CONNECTING)
                {
                  if (sock->connected_socket)
                    if (sock->connected_socket (sock))
                      {
                        svz_sock_schedule_for_shutdown (sock);
                        continue;
                      }
                }
              /* Just writable.  */
              else
                {
                  if (sock->write_socket)
                    if (sock->write_socket (sock))
                      {
                        svz_sock_schedule_for_shutdown (sock);
                        continue;
                      }
                }
            }
        }
    }

  /*
   * We had no chance to time out so we have to explicitly call the
   * timeout handler.
   */
  if (time (NULL) > svz_notify)
    {
      svz_periodic_tasks ();
    }

  return 0;
}

#endif /* __MINGW32__ */

/*
 * Check the server and client sockets for incoming connections
 * and data, and process outgoing data.
 */
int
svz_check_sockets (void)
{
#if USE_POLL
  return check_sockets_poll ();
#elif defined (__MINGW32__)
  return check_sockets_mingw ();
#else
  return check_sockets_select ();
#endif
}
