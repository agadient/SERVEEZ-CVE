/*
 * pipe-socket.c - pipes in socket structures
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
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#if HAVE_FLOSS_H
# include <floss.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <fcntl.h>
#include <sys/stat.h>
#include <time.h>

#if HAVE_PWD_H
# include <pwd.h>
#endif

#if HAVE_GRP_H
# include <grp.h>
#endif

#include "networking-headers.h"
#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/core.h"
#include "libserveez/server-core.h"
#include "libserveez/pipe-socket.h"

#ifdef __MINGW32__
/* Some static data for the ‘CancelIo’ call.  We need to load the symbol
   dynamically because it is not available on all versions of Windows.  */
typedef BOOL (__stdcall * CancelIoProc) (HANDLE);
static CancelIoProc CancelIoFunc = NULL;
static HANDLE Kernel32Handle = NULL;
#endif /* __MINGW32__ */

/*
 * Startup the pipe interface of the core API of serveez.  Returns zero on
 * success.
 */
static int
startup (void)
{
#ifdef __MINGW32__
  if ((Kernel32Handle = LoadLibrary ("KERNEL32.DLL")) == NULL)
    {
      svz_log_sys_error ("pipe: LoadLibrary");
      return -1;
    }

  /* obtain ‘CancelIo’ function */
  CancelIoFunc = (CancelIoProc) GetProcAddress (Kernel32Handle, "CancelIo");
  if (CancelIoFunc == NULL)
    {
      svz_log_sys_error ("pipe: GetProcAddress");
      FreeLibrary (Kernel32Handle);
      Kernel32Handle = NULL;
      return -1;
    }
#endif /* __MINGW32__ */
  return 0;
}

/*
 * Cleanup the pipe interface of the core API of serveez.  Returns zero on
 * success.
 */
static int
cleanup (void)
{
#ifdef __MINGW32__
  if (Kernel32Handle != NULL)
    {
      FreeLibrary (Kernel32Handle);
      Kernel32Handle = NULL;
      CancelIoFunc = NULL;
    }
#endif /* __MINGW32__ */
  return 0;
}

/*
 * Check the consistency of the "user" - "user id" pair in the given pipe
 * structure @var{pipe}.  Return zero if it is ok.
 */
int
svz_pipe_check_user (svz_pipe_t *pipe)
{
#if HAVE_PWD_H
  struct passwd *p = NULL;

  if (pipe->user)
    {
      if ((p = getpwnam (pipe->user)) == NULL)
        {
          svz_log (SVZ_LOG_WARNING, "%s: no such user `%s'\n",
                   pipe->name, pipe->user);
          return 0;
        }
      pipe->uid = p->pw_uid;
      pipe->pgid = p->pw_gid;
    }
  else if (pipe->uid != (uid_t) -1)
    {
      if ((p = getpwuid (pipe->uid)) == NULL)
        {
          svz_log (SVZ_LOG_WARNING, "%s: no such user id `%d'\n",
                   pipe->name, pipe->uid);
          return 0;
        }
      pipe->user = svz_strdup (p->pw_name);
      pipe->pgid = p->pw_gid;
    }
#endif /* not HAVE_PWD_H */
  return 0;
}

/*
 * Check the consistency of the "group" - "group id" pair in the structure
 * @var{pipe}.  Return zero if it is valid.
 */
int
svz_pipe_check_group (svz_pipe_t *pipe)
{
#if HAVE_GRP_H
  struct group *g = NULL;
  int n = 0;

  if (pipe->group)
    {
      if ((g = getgrnam (pipe->group)) == NULL)
        {
          svz_log (SVZ_LOG_WARNING, "%s: no such group `%s'\n",
                   pipe->name, pipe->group);
          return 0;
        }
      pipe->gid = g->gr_gid;
    }
  else if (pipe->gid != (gid_t) -1)
    {
      if ((g = getgrgid (pipe->gid)) == NULL)
        {
          svz_log (SVZ_LOG_WARNING, "%s: no such group id `%d'\n",
                   pipe->name, pipe->gid);
          return 0;
        }
      pipe->group = svz_strdup (g->gr_name);
    }

  /* Check if the user is in the selected group and croak about it
     if not.  This check is only done if all necessary info is given.  */
  if (g && g->gr_mem && pipe->user)
    {
      while (g->gr_mem[n])
        {
          if (!strcmp (g->gr_mem[n], pipe->user))
            {
              n = -1;
              break;
            }
          n++;
        }
      if (n != -1 && pipe->gid != pipe->pgid)
        {
          svz_log (SVZ_LOG_WARNING, "%s: user `%s' is not in group `%s'\n",
                   pipe->name, pipe->user, pipe->group);
          return 0;
        }
    }
#endif /* HAVE_GRP_H */
  return 0;
}

/**
 * Invalidate the handle pointed at by @var{href}.
 */
void
svz_invalidate_handle (svz_t_handle *href)
{
#ifdef __MINGW32__
  *href = INVALID_HANDLE_VALUE;
#else
  *href = (svz_t_handle) -1;
#endif
}

/**
 * Return 1 if @var{handle} is invalid, otherwise 0.
 */
int
svz_invalid_handle_p (svz_t_handle handle)
{
#ifdef __MINGW32__
  return INVALID_HANDLE_VALUE == handle;
#else
  return ((svz_t_handle) -1) == handle;
#endif
}

/*
 * This function is for checking if a given socket structure contains
 * a valid pipe socket (checking both pipes).  Return non-zero on errors.
 */
int
svz_pipe_valid (svz_socket_t *sock)
{
  if (sock->flags & SVZ_SOFLG_LISTENING)
    return 0;

  if (!(sock->flags & SVZ_SOFLG_CONNECTED))
    return -1;

  if (sock->flags & SVZ_SOFLG_RECV_PIPE)
    if (svz_invalid_handle_p (sock->pipe_desc[SVZ_READ]))
      return -1;

  if (sock->flags & SVZ_SOFLG_SEND_PIPE)
    if (svz_invalid_handle_p (sock->pipe_desc[SVZ_WRITE]))
      return -1;

  return 0;
}

/**
 * Close @var{handle}.
 * Return 0 if successful, -1 otherwise.
 */
int
svz_closehandle (svz_t_handle handle)
{
#ifdef __MINGW32__
  return CloseHandle (handle) ? 0 : -1;
#else
  return close (handle);
#endif
}

/*
 * This function is the default disconnection routine for pipe socket
 * structures.  Return non-zero on errors.
 */
int
svz_pipe_disconnect (svz_socket_t *sock)
{
  svz_socket_t *rsock;

  if (sock->flags & SVZ_SOFLG_CONNECTED)
    {
      /* has this socket created by a listener?  */
      if ((rsock = svz_sock_getreferrer (sock)) != NULL)
        {
#ifdef __MINGW32__
          /* cancel any pending I/O if necessary and possible */
          if (CancelIoFunc)
            {
              if (sock->flags & (SVZ_SOFLG_READING | SVZ_SOFLG_CONNECTING))
                if (!CancelIoFunc (sock->pipe_desc[SVZ_READ]))
                  svz_log_sys_error ("CancelIo");
              if (sock->flags & (SVZ_SOFLG_WRITING | SVZ_SOFLG_CONNECTING))
                if (!CancelIoFunc (sock->pipe_desc[SVZ_WRITE]))
                  svz_log_sys_error ("CancelIo");
            }

          /* just disconnect client pipes */
          if (!DisconnectNamedPipe (sock->pipe_desc[SVZ_READ]))
            svz_log_sys_error ("DisconnectNamedPipe");
          if (!DisconnectNamedPipe (sock->pipe_desc[SVZ_WRITE]))
            svz_log_sys_error ("DisconnectNamedPipe");

          /* reinitialize the overlapped structure of the listener */
          if (svz_mingw_at_least_nt4_p ())
            {
              memset (sock->overlap[SVZ_READ], 0, sizeof (OVERLAPPED));
              memset (sock->overlap[SVZ_WRITE], 0, sizeof (OVERLAPPED));
              sock->overlap[SVZ_READ] = NULL;
              sock->overlap[SVZ_WRITE] = NULL;
            }
#else /* not __MINGW32__ */

          /* close sending pipe only */
          if (! svz_invalid_handle_p (sock->pipe_desc[SVZ_WRITE]))
            if (svz_closehandle (sock->pipe_desc[SVZ_WRITE]) < 0)
              svz_log_sys_error ("close");

          /* FIXME: reset receiving pipe???  */

#endif /* not __MINGW32__ */

          /* restart listening pipe server socket */
          rsock->flags &= ~SVZ_SOFLG_INITED;
          svz_sock_setreferrer (rsock, NULL);
        }

      /* no, it is a connected pipe */
      else
        {
          /* close both pipes */
          if (! svz_invalid_handle_p (sock->pipe_desc[SVZ_READ]))
            if (svz_closehandle (sock->pipe_desc[SVZ_READ]) < 0)
              svz_log_sys_error ("pipe: close");
          if (! svz_invalid_handle_p (sock->pipe_desc[SVZ_WRITE]))
            if (svz_closehandle (sock->pipe_desc[SVZ_WRITE]) < 0)
              svz_log_sys_error ("pipe: close");
        }

#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "pipe (%d-%d) disconnected\n",
               sock->pipe_desc[SVZ_READ], sock->pipe_desc[SVZ_WRITE]);
#endif

      svz_invalidate_handle (&sock->pipe_desc[SVZ_READ]);
      svz_invalidate_handle (&sock->pipe_desc[SVZ_WRITE]);
    }

  /* prevent a pipe server's child to reinit the pipe server */
  if (sock->flags & SVZ_SOFLG_LISTENING)
    {
      if ((rsock = svz_sock_getreferrer (sock)) != NULL)
        {
#ifdef __MINGW32__
          rsock->overlap[SVZ_READ] = NULL;
          rsock->overlap[SVZ_WRITE] = NULL;
#endif /* __MINGW32__ */
          svz_sock_setreferrer (rsock, NULL);
        }

#ifndef __MINGW32__

      /* close listening pipe */
      if (! svz_invalid_handle_p (sock->pipe_desc[SVZ_READ]))
        if (svz_closehandle (sock->pipe_desc[SVZ_READ]) < 0)
          svz_log_sys_error ("close");

      /* delete named pipes on file system */
      if (unlink (sock->recv_pipe) == -1)
        svz_log_sys_error ("unlink");
      if (unlink (sock->send_pipe) == -1)
        svz_log_sys_error ("unlink");

#else /* __MINGW32__ */

      /* disconnect and close named pipes */
      if (! svz_invalid_handle_p (sock->pipe_desc[SVZ_READ]))
        {
          if (!DisconnectNamedPipe (sock->pipe_desc[SVZ_READ]))
            svz_log_sys_error ("DisconnectNamedPipe");
          if (svz_closehandle (sock->pipe_desc[SVZ_READ]))
            svz_log_sys_error ("CloseHandle");
        }
      if (! svz_invalid_handle_p (sock->pipe_desc[SVZ_WRITE]))
        {
          if (!DisconnectNamedPipe (sock->pipe_desc[SVZ_WRITE]))
            svz_log_sys_error ("DisconnectNamedPipe");
          if (!CloseHandle (sock->pipe_desc[SVZ_WRITE]))
            svz_log_sys_error ("CloseHandle");
        }

#endif /* __MINGW32__ */

#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "pipe listener (%s) destroyed\n", sock->recv_pipe);
#endif

      svz_invalidate_handle (&sock->pipe_desc[SVZ_READ]);
      svz_invalidate_handle (&sock->pipe_desc[SVZ_WRITE]);
    }

  return 0;
}

#ifdef __MINGW32__
#define PIPE_MAX_WRITE 0xFFFF /* maximum amount of write transfer bytes */
#define PIPE_MAX_READ  0xFFFF /* maximum amount of read transfer bytes */

/* Print text representation of given overlapped I/O structure.  */
static void
display_overlap (LPOVERLAPPED overlap)
{
  if (overlap)
    {
      printf ("Internal: %ld (0x%08lX), InternalHigh: %ld (0x%08lX)\n"
              "Offset: %ld (0x%08lX), OffsetHigh: %ld (0x%08lX)\n"
              "Event: %p\n",
              overlap->Internal, overlap->Internal, overlap->InternalHigh,
              overlap->InternalHigh, overlap->Offset, overlap->Offset,
              overlap->OffsetHigh, overlap->OffsetHigh, overlap->hEvent);
    }
}
#endif /* __MINGW32__ */

/*
 * The @code{svz_pipe_read_socket} function reads as much data as
 * available on a readable pipe descriptor or handle on Win32.  Return
 * a non-zero value on errors.
 */
int
svz_pipe_read_socket (svz_socket_t *sock)
{
  int num_read, do_read;

  /* Read as much space is left in the receive buffer and return
   * zero if there is no more space.  */
  do_read = sock->recv_buffer_size - sock->recv_buffer_fill;
  if (do_read <= 0)
    {
      svz_log (SVZ_LOG_ERROR, "receive buffer overflow on pipe %d\n",
               sock->pipe_desc[SVZ_READ]);
      if (sock->kicked_socket)
        sock->kicked_socket (sock, 0);
      return -1;
    }

#ifdef __MINGW32__
  /* Named pipes in Win32 cannot transfer more than 64KB at once.  */
  if (do_read > PIPE_MAX_READ)
    do_read = PIPE_MAX_READ;

  /* Use the ‘PeekNamedPipe’ call if there is no overlapped I/O in
     order to make the following ‘ReadFile’ non-blocking.  */
  if (sock->overlap[SVZ_READ] == NULL)
    {
      /* Check how many bytes could have been read from the pipe without
         really reading them.  */
      if (!PeekNamedPipe (sock->pipe_desc[SVZ_READ], NULL, 0,
                          NULL, (DWORD *) &num_read, NULL))
        {
          svz_log_sys_error ("pipe: PeekNamedPipe");
          return -1;
        }

      /* Leave this function if there is no data within the pipe.  */
      if (num_read <= 0)
        return 0;

      /* Adjust number of bytes to read.  */
      if (do_read > num_read)
        do_read = num_read;
    }

  /* Try to get the result of the last ‘ReadFile’.  */
  if (sock->flags & SVZ_SOFLG_READING)
    {
      if (!GetOverlappedResult (sock->pipe_desc[SVZ_READ], sock->overlap[SVZ_READ],
                                (DWORD *) &num_read, FALSE))
        {
          if (GetLastError () != ERROR_IO_INCOMPLETE)
            {
              svz_log_sys_error ("pipe: GetOverlappedResult");
              return -1;
            }
          return 0;
        }

      /* Schedule the pipe for the ‘ReadFile’ call again.  */
      else
        {
          sock->recv_pending = 0;
          sock->flags &= ~SVZ_SOFLG_READING;
        }
    }
  /* Really read from the pipe.  */
  else if (!ReadFile (sock->pipe_desc[SVZ_READ],
                      sock->recv_buffer + sock->recv_buffer_fill,
                      do_read, (DWORD *) &num_read, sock->overlap[SVZ_READ]))
    {
      if (GetLastError () != ERROR_IO_PENDING)
        {
          svz_log_sys_error ("pipe: ReadFile");
          return -1;
        }

      /* Schedule the pipe for the ‘GetOverlappedResult’ call.  */
      sock->recv_pending = do_read;
      sock->flags |= SVZ_SOFLG_READING;
      return 0;
    }
#else /* not __MINGW32__ */
  if ((num_read = read (sock->pipe_desc[SVZ_READ],
                        sock->recv_buffer + sock->recv_buffer_fill,
                        do_read)) == -1)
    {
      svz_log_sys_error ("pipe: read");
      if (errno == EAGAIN)
        return 0;
      return -1;
    }
#endif /* not __MINGW32__ */

  /* Some data has been read from the pipe.  */
  if (num_read > 0)
    {
      sock->last_recv = time (NULL);

#if ENABLE_FLOOD_PROTECTION
      if (svz_sock_flood_protect (sock, num_read))
        {
          svz_log (SVZ_LOG_ERROR, "kicked pipe %d (flood)\n",
                   sock->pipe_desc[SVZ_READ]);
          return -1;
        }
#endif /* ENABLE_FLOOD_PROTECTION */

      sock->recv_buffer_fill += num_read;

      if (sock->check_request)
        if (sock->check_request (sock))
          return -1;
    }

#ifndef __MINGW32__
  /* The pipe was selected but there is no data.  */
  else
    {
      svz_log (SVZ_LOG_ERROR, "pipe: read: no data on pipe %d\n",
               sock->pipe_desc[SVZ_READ]);
      return -1;
    }
#endif /* !__MINGW32__ */

  return 0;
}

/*
 * This @code{svz_pipe_write_socket} function writes as much data as
 * possible into a writable pipe descriptor.  It returns a non-zero value
 * on errors.
 */
int
svz_pipe_write_socket (svz_socket_t *sock)
{
  int num_written, do_write;

  /* Write as many bytes as possible, remember how many were actually
     sent.  */
  do_write = sock->send_buffer_fill;

#ifdef __MINGW32__
  /* Named pipes in Win32 cannot transfer more than 64KB at once.  */
  if (do_write > PIPE_MAX_WRITE)
    do_write = PIPE_MAX_WRITE;

  /* Data bytes have been stored in system's cache.  Now we are checking
     if pending write operation has been completed.  */
  if (sock->flags & SVZ_SOFLG_WRITING)
    {
      if (!GetOverlappedResult (sock->pipe_desc[SVZ_WRITE], sock->overlap[SVZ_WRITE],
                                (DWORD *) &num_written, FALSE))
        {
          if (GetLastError () != ERROR_IO_INCOMPLETE)
            {
              svz_log_sys_error ("pipe: GetOverlappedResult");
              return -1;
            }
          return 0;
        }

      /* Reschedule the pipe descriptor for yet another ‘WriteFile’.  */
      else
        {
          sock->send_pending -= num_written;
          sock->flags &= ~SVZ_SOFLG_WRITING;
          if (sock->send_pending != 0)
            {
              svz_log (SVZ_LOG_ERROR, "pipe: %d pending send bytes left\n",
                       sock->send_pending);
            }
        }
    }
  /* Really write to the pipe.  */
  else if (!WriteFile (sock->pipe_desc[SVZ_WRITE], sock->send_buffer,
                       do_write, (DWORD *) &num_written, sock->overlap[SVZ_WRITE]))
    {
      if (GetLastError () != ERROR_IO_PENDING)
        {
          svz_log_sys_error ("pipe: WriteFile");
          return -1;
        }
      sock->send_pending += do_write;
      sock->flags |= SVZ_SOFLG_WRITING;
      return 0;
    }
#else /* not __MINGW32__ */
  if ((num_written = write (sock->pipe_desc[SVZ_WRITE],
                            sock->send_buffer, do_write)) == -1)
    {
      svz_log_sys_error ("pipe: write");
      if (svz_wait_if_unavailable (sock, 1))
        num_written = 0;
    }
#endif /* not __MINGW32__ */

  /* Some data has been successfully written to the pipe.  */
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

  return (num_written < 0) ? -1 : 0;
}

/**
 * Create a socket structure containing both the pipe descriptors
 * @var{recv_fd} and @var{send_fd}.  Return @code{NULL} on errors.
 */
svz_socket_t *
svz_pipe_create (svz_t_handle recv_fd, svz_t_handle send_fd)
{
  svz_socket_t *sock;

#ifndef __MINGW32__
  /* Try to set to non-blocking I/O.  */
  if (svz_fd_nonblock ((int) recv_fd) != 0)
    return NULL;
  if (svz_fd_nonblock ((int) send_fd) != 0)
    return NULL;
#endif /* __MINGW32__ */

  /* Do not inherit these pipes */
  if (svz_fd_cloexec ((int) recv_fd) != 0)
    return NULL;
  if (svz_fd_cloexec ((int) send_fd) != 0)
    return NULL;

  if ((sock = svz_sock_alloc ()) != NULL)
    {
      svz_sock_unique_id (sock);
      sock->pipe_desc[SVZ_READ] = recv_fd;
      sock->pipe_desc[SVZ_WRITE] = send_fd;
      sock->flags |= (SVZ_SOFLG_PIPE | SVZ_SOFLG_CONNECTED);
    }

  return sock;
}

/**
 * Create a (non blocking) pair of pipes.  This differs in Win32 and
 * Unices.  Return a non-zero value on errors.
 */
int
svz_pipe_create_pair (svz_t_handle pipe_desc[2])
{
#ifdef __MINGW32__

  SECURITY_ATTRIBUTES sa = { sizeof (SECURITY_ATTRIBUTES),
                             NULL,    /* NULL security descriptor */
                             TRUE };  /* Inherit handles */

  if (!CreatePipe (&pipe_desc[SVZ_READ], &pipe_desc[SVZ_WRITE], &sa, 0))
    {
      svz_log_sys_error ("CreatePipe");
      return -1;
    }

#else /* not __MINGW32__ */

  if (pipe (pipe_desc) == -1)
    {
      svz_log_sys_error ("pipe");
      return -1;
    }

  /*
   * FIXME: Maybe cgi pipes MUST be blocking for *very* fast
   *        outputs because thay cannot handle the EAGAIN error.
   */

  /* Make both ends of the pipe non-blocking.  */
  if (svz_fd_nonblock (pipe_desc[SVZ_READ]) != 0)
    return -1;

  if (svz_fd_nonblock (pipe_desc[SVZ_WRITE]) != 0)
    return -1;

#endif /* not __MINGW32__ */

  return 0;
}

#ifndef __MINGW32__

#if HAVE_SETEUID
# define SETUID(id) seteuid (id)
# define SETUID_FUNC "seteuid"
#else
# define SETUID(id) setuid (id)
# define SETUID_FUNC "setuid"
#endif

#if HAVE_SETEGID
# define SETGID(id) setegid (id)
# define SETGID_FUNC "setegid"
#else
# define SETGID(id) setgid (id)
# define SETGID_FUNC "setgid"
#endif

#if HAVE_GETEUID
# define GETUID() geteuid ()
#else
# define GETUID() getuid ()
#endif

#if HAVE_GETEGID
# define GETGID() getegid ()
#else
# define GETGID() getgid ()
#endif

/*
 * The following function saves the user and group permissions of the current
 * process.  It stores the values for the umask, user id and group id in
 * @var{mask}, @var{uid} and @var{gid}.
 */
static void
save_state (mode_t *mask, uid_t *uid, gid_t *gid)
{
  *mask = umask (0);
  *uid = GETUID ();
  *gid = GETGID ();
}

/*
 * This function sets the umask @var{mask}, the user id @var{uid} and the
 * group id @var{gid}.  Returns zero on success, non-zero otherwise.
 */
static int
set_state (mode_t mask, uid_t uid, gid_t gid)
{
  umask (mask);
  if (SETUID (uid) < 0)
    {
      svz_log_sys_error ("pipe: %s (%d)", SETUID_FUNC, uid);
      return -1;
    }
  if (SETGID (gid) < 0)
    {
      svz_log_sys_error ("pipe: %s (%d)", SETGID_FUNC, gid);
      return -1;
    }
  return 0;
}

/*
 * Modify the current permissions state as specified by @var{pipe}.  Returns
 * zero on success and non-zero on errors.
 */
static int
try_state (svz_pipe_t *pipe)
{
  /* umask value */
  if (pipe->perm != (mode_t) -1)
    umask (~pipe->perm);

  /* group id (need to change group first !) */
  if (pipe->gid != (gid_t) -1)
    if (SETGID (pipe->gid) < 0)
      {
        svz_log_sys_error ("pipe: %s (%d)", SETGID_FUNC, pipe->gid);
        return -1;
      }

  /* user id */
  if (pipe->uid != (uid_t) -1)
    if (SETUID (pipe->uid) < 0)
      {
        svz_log_sys_error ("pipe: %s (%d)", SETUID_FUNC, pipe->uid);
        return -1;
      }

  return 0;
}
#endif /* not __MINGW32__ */

/*
 * Set the file names of the socket structure @var{sock} to @var{recv} for
 * the receiving end and @var{send} for the sending end of a pipe socket.
 */
static void
set_files (svz_socket_t *sock, char *recv, char *send)
{
  if (sock->recv_pipe)
    svz_free (sock->recv_pipe);
  if (sock->send_pipe)
    svz_free (sock->send_pipe);
#ifndef __MINGW32__
  sock->recv_pipe = svz_strdup (recv);
  sock->send_pipe = svz_strdup (send);
#else /* __MINGW32__ */
  if (strstr (recv, "\\pipe\\") == NULL)
    {
      sock->recv_pipe = svz_malloc (strlen (recv) + 10);
      sprintf (sock->recv_pipe, "\\\\.\\pipe\\%s", recv);
    }
  else
    sock->recv_pipe = svz_strdup (recv);
  if (strstr (send, "\\pipe\\") == NULL)
    {
      sock->send_pipe = svz_malloc (strlen (send) + 10);
      sprintf (sock->send_pipe, "\\\\.\\pipe\\%s", send);
    }
  else
    sock->send_pipe = svz_strdup (send);
#endif /* __MINGW32__ */
}

/**
 * Create a pipe connection socket structure to the pair of named
 * pipes @var{recv} and @var{send}.  Return @code{NULL} on errors.
 */
svz_socket_t *
svz_pipe_connect (svz_pipe_t *recv, svz_pipe_t *send)
{
  svz_socket_t *sock;
  svz_t_handle recv_pipe, send_pipe;
#ifndef __MINGW32__
  mode_t mask; uid_t uid; gid_t gid;
  struct stat buf;
#endif

  /* create socket structure */
  if ((sock = svz_sock_alloc ()) == NULL)
    return NULL;

  /* create pipe file text representation */
  set_files (sock, recv->name, send->name);

#ifndef __MINGW32__
  /* is receive pipe such a?  */
  if (stat (sock->recv_pipe, &buf) == -1 || !S_ISFIFO (buf.st_mode))
    {
      svz_log (SVZ_LOG_ERROR, "pipe: no such pipe: %s\n", sock->recv_pipe);
      svz_sock_free (sock);
      return NULL;
    }

  /* is send pipe such a?  */
  if (stat (sock->send_pipe, &buf) == -1 || !S_ISFIFO (buf.st_mode))
    {
      svz_log (SVZ_LOG_ERROR, "pipe: no such pipe: %s\n", sock->send_pipe);
      svz_sock_free (sock);
      return NULL;
    }

  /* save the current process's permission state and set the new state
     for the receive pipe if necessary and possible */
  save_state (&mask, &uid, &gid);
  if (try_state (recv) < 0)
    {
      set_state (mask, uid, gid);
      svz_sock_free (sock);
      return NULL;
    }

  /* try opening receiving pipe for reading */
  if ((recv_pipe = open (sock->recv_pipe, O_RDONLY | O_NONBLOCK)) == -1)
    {
      svz_log_sys_error ("pipe: open");
      svz_sock_free (sock);
      set_state (mask, uid, gid);
      return NULL;
    }

  /* restore the current process's permission state */
  set_state (mask, uid, gid);

  /* save the current process's permission state and set the new state
     for the send pipe if necessary and possible */
  save_state (&mask, &uid, &gid);
  if (try_state (send) < 0)
    {
      close (recv_pipe);
      set_state (mask, uid, gid);
      svz_sock_free (sock);
      return NULL;
    }

  /* try opening sending pipe for writing */
  if ((send_pipe = open (sock->send_pipe, O_WRONLY | O_NONBLOCK)) == -1)
    {
      svz_log_sys_error ("pipe: open");
      close (recv_pipe);
      svz_sock_free (sock);
      set_state (mask, uid, gid);
      return NULL;
    }

  /* restore the current process's permission state */
  set_state (mask, uid, gid);

  /* set send pipe to non-blocking mode and do not inherit the created
     pipe descriptors */
  if (svz_fd_nonblock (send_pipe) != 0 ||
      svz_fd_cloexec (send_pipe) != 0 || svz_fd_cloexec (recv_pipe) != 0)
    {
      close (recv_pipe);
      close (send_pipe);
      svz_sock_free (sock);
      return NULL;
    }

#else /* __MINGW32__ */

  /* try opening receiving pipe */
  if (svz_invalid_handle_p
      (recv_pipe = CreateFile (sock->recv_pipe, GENERIC_READ, 0,
                               NULL, OPEN_EXISTING, FILE_FLAG_OVERLAPPED,
                               NULL)))
    {
      svz_log_sys_error ("pipe: CreateFile");
      svz_sock_free (sock);
      return NULL;
    }

  /* try opening sending pipe */
  if (svz_invalid_handle_p
      (send_pipe = CreateFile (sock->send_pipe, GENERIC_WRITE, 0,
                               NULL, OPEN_EXISTING, FILE_FLAG_OVERLAPPED,
                               NULL)))
    {
      svz_log_sys_error ("pipe: CreateFile");
      DisconnectNamedPipe (recv_pipe);
      CloseHandle (recv_pipe);
      svz_sock_free (sock);
      return NULL;
    }

  /* initialize the overlap structure on WinNT systems */
  if (svz_mingw_at_least_nt4_p ())
    {
      sock->overlap[SVZ_READ] = svz_malloc (sizeof (OVERLAPPED));
      memset (sock->overlap[SVZ_READ], 0, sizeof (OVERLAPPED));
      sock->overlap[SVZ_WRITE] = svz_malloc (sizeof (OVERLAPPED));
      memset (sock->overlap[SVZ_WRITE], 0, sizeof (OVERLAPPED));
    }

#endif /* __MINGW32__ */

  /* modify socket structure and assign some callbacks */
  svz_sock_unique_id (sock);
  sock->pipe_desc[SVZ_READ] = recv_pipe;
  sock->pipe_desc[SVZ_WRITE] = send_pipe;
  sock->flags |= (SVZ_SOFLG_PIPE | SVZ_SOFLG_CONNECTED);
  svz_sock_enqueue (sock);

  sock->read_socket = svz_pipe_read_socket;
  sock->write_socket = svz_pipe_write_socket;

  svz_sock_connections++;
  return sock;
}

#if HAVE_MKFIFO
# define MKFIFO(path, mode) mkfifo (path, mode)
# define MKFIFO_FUNC "mkfifo"
#else
# define MKFIFO(path, mode) mknod (path, mode | S_IFIFO, 0)
# define MKFIFO_FUNC "mknod"
#endif

/*
 * Prepare the server socket structure @var{sock} for listening
 * on the receiving pipe of @var{recv}.  Open the reading end of such a
 * connection.  Return either zero or non-zero on errors.
 */
int
svz_pipe_listener (svz_socket_t *sock, svz_pipe_t *recv, svz_pipe_t *send)
{
#if defined (HAVE_MKFIFO) || defined (HAVE_MKNOD)
  struct stat buf;
  mode_t mask; uid_t uid; gid_t gid;
#endif

#if defined (HAVE_MKFIFO) || defined (HAVE_MKNOD) || defined (__MINGW32__)
#ifdef __MINGW32__
  svz_t_handle send_pipe;
#endif
  svz_t_handle recv_pipe;

  /* Setup the text representation of the fifo names.  */
  set_files (sock, recv->name, send->name);

  /* Pipe requested via port configuration?  */
  if (!sock->recv_pipe || !sock->send_pipe)
    return -1;
#endif

#if defined (HAVE_MKFIFO) || defined (HAVE_MKNOD)
  /* Test if both of the named pipes have been created yet.  If not then
     create them locally.  */
  if (stat (sock->recv_pipe, &buf) == -1)
    {
      /* Save old permissions and set new ones.  */
      save_state (&mask, &uid, &gid);
      if (try_state (recv) < 0)
        {
          set_state (mask, uid, gid);
          return -1;
        }
      /* Create fifo locally.  */
      if (MKFIFO (sock->recv_pipe, 0666) != 0)
        {
          svz_log_sys_error ("pipe: %s", MKFIFO_FUNC);
          set_state (mask, uid, gid);
          return -1;
        }
      /* Check if that was successful.  */
      if (stat (sock->recv_pipe, &buf) == -1 || !S_ISFIFO (buf.st_mode))
        {
          svz_log (SVZ_LOG_ERROR,
                   "pipe: stat: " MKFIFO_FUNC "() did not create a fifo\n");
          set_state (mask, uid, gid);
          return -1;
        }
      /* Restore old permissions.  */
      set_state (mask, uid, gid);
    }

  if (stat (sock->send_pipe, &buf) == -1)
    {
      save_state (&mask, &uid, &gid);
      if (try_state (send) < 0)
        {
          set_state (mask, uid, gid);
          return -1;
        }
      if (MKFIFO (sock->send_pipe, 0666) != 0)
        {
          svz_log_sys_error ("pipe: %s", MKFIFO_FUNC);
          set_state (mask, uid, gid);
          return -1;
        }
      if (stat (sock->send_pipe, &buf) == -1 || !S_ISFIFO (buf.st_mode))
        {
          svz_log (SVZ_LOG_ERROR,
                   "pipe: stat: " MKFIFO_FUNC "() did not create a fifo\n");
          set_state (mask, uid, gid);
          return -1;
        }
      set_state (mask, uid, gid);
    }

  /* Try opening the server's read pipe.  Should always be possible.  */
  if ((recv_pipe = open (sock->recv_pipe, O_NONBLOCK | O_RDONLY)) == -1)
    {
      svz_log_sys_error ("pipe: open");
      return -1;
    }
  /* Check if the file descriptor is a pipe.  */
  if (fstat (recv_pipe, &buf) == -1 || !S_ISFIFO (buf.st_mode))
    {
      svz_log (SVZ_LOG_ERROR,
               "pipe: fstat: " MKFIFO_FUNC "() did not create a fifo\n");
      close (recv_pipe);
      return -1;
    }

  /* Do not inherit this pipe.  */
  svz_fd_cloexec (recv_pipe);

  sock->pipe_desc[SVZ_READ] = recv_pipe;
  sock->flags |= SVZ_SOFLG_RECV_PIPE;

#elif defined (__MINGW32__) /* not (HAVE_MKFIFO || HAVE_MKNOD) */

  /*
   * Create both of the named pipes and put the handles into
   * the server socket structure.
   */
  recv_pipe = CreateNamedPipe (
    sock->recv_pipe,                            /* path */
    PIPE_ACCESS_INBOUND | FILE_FLAG_OVERLAPPED, /* receive + overlapped */
    PIPE_READMODE_BYTE | PIPE_WAIT,             /* binary + blocking */
    1,                                          /* one instance only */
    0, 0,                                       /* default buffer sizes */
    100,                                        /* timeout in ms */
    NULL);                                      /* no security */

  if (svz_invalid_handle_p (recv_pipe) || !recv_pipe)
    {
      svz_log_sys_error ("pipe: CreateNamedPipe");
      return -1;
    }
  sock->pipe_desc[SVZ_READ] = recv_pipe;
  sock->flags |= SVZ_SOFLG_RECV_PIPE;

  send_pipe = CreateNamedPipe (
    sock->send_pipe,                             /* path */
    PIPE_ACCESS_OUTBOUND | FILE_FLAG_OVERLAPPED, /* send + overlapped */
    PIPE_TYPE_BYTE | PIPE_WAIT,                  /* binary + blocking */
    1,                                           /* one instance only */
    0, 0,                                        /* default buffer sizes */
    100,                                         /* timeout in ms */
    NULL);                                       /* no security */

  if (svz_invalid_handle_p (send_pipe) || !send_pipe)
    {
      svz_log_sys_error ("pipe: CreateNamedPipe");
      return -1;
    }
  sock->pipe_desc[SVZ_WRITE] = send_pipe;
  sock->flags |= SVZ_SOFLG_SEND_PIPE;

  /*
   * Initialize the overlapped structures for this server socket.  Each
   * client connected gets it passed.
   */
  if (svz_mingw_at_least_nt4_p ())
    {
      sock->overlap[SVZ_READ] = svz_malloc (sizeof (OVERLAPPED));
      memset (sock->overlap[SVZ_READ], 0, sizeof (OVERLAPPED));
      sock->overlap[SVZ_WRITE] = svz_malloc (sizeof (OVERLAPPED));
      memset (sock->overlap[SVZ_WRITE], 0, sizeof (OVERLAPPED));
    }

#else /* not __MINGW32__ */

  return -1;

#endif /* neither HAVE_MKFIFO nor __MINGW32__ */

#if defined (HAVE_MKFIFO) || defined (HAVE_MKNOD) || defined (__MINGW32__)

  return 0;

#endif /* HAVE_MKFIFO or __MINGW32__ */
}


void
svz__pipe_updn (int direction)
{
  (direction
   ? startup
   : cleanup)
    ();
}
