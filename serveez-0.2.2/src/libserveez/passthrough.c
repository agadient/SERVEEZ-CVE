/*
 * passthrough.c - pass through connections to processes
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include <stdio.h>
#include "timidity.h"
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/stat.h>
#include <time.h>

#if HAVE_FLOSS_H
# include <floss.h>
#endif
#if HAVE_PWD_H
# include <pwd.h>
#endif
#if HAVE_GRP_H
# include <grp.h>
#endif
#if HAVE_WAIT_H
# include <wait.h>
#endif
#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#include "changedir.h"
#include "networking-headers.h"
#include "woe-wait.h"
#ifdef __MINGW32__
# include <io.h>
# include <shellapi.h>
#else
# include <sys/types.h>
# include <sys/socket.h>
#endif

#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/core.h"
#include "libserveez/server-core.h"
#include "libserveez/pipe-socket.h"
#include "libserveez/passthrough.h"
#include "misc-macros.h"

/* Passthrough methods.  */
#define SVZ_PROCESS_FORK         1
#define SVZ_PROCESS_SHUFFLE_SOCK 2
#define SVZ_PROCESS_SHUFFLE_PIPE 3

struct svz_process_t
{
  svz_socket_t *sock;   /* Socket structure to pass through.  */
  char *bin;            /* Fully qualified program name.  */
  char *dir;            /* Working directory.  */
  char **argv;          /* Program arguments including argv[0].  */
  svz_envblock_t *envp; /* Environment block.  */
  char *user;           /* User and group.  */
  char *app;            /* Additional @var{bin} interpreter application.  */
  svz_t_handle in, out; /* New stdin and stdout of child process.  */
  int flag;             /* Passthrough method flag.  */
};

typedef struct svz_process_t svz_process_t;

/* Extern declaration of the process environment pointer.  */
#if !defined(__MINGW32__) && !defined(__CYGWIN__)
extern char **environ;
#endif

/*
 * This variable is meant to hold the @code{environ} variable of the
 * application using the Serveez core API.  It must be setup via the macro
 * @code{svz_envblock_setup}.
 */
static char **environment = NULL;

/**
 * Set up internal tables for environment block wrangling.
 *
 * This function must be called once after @code{svz_boot}
 * so that subsequent functions
 * (like @code{svz_envblock_default}) can work correctly.
 */
void
svz_envblock_setup (void)
{
  environment = environ;
}

/*
 * Check if the given @var{file} is an executable (script or binary
 * program).  If it is script the routine returns an application able to
 * execute the script in  @var{app}.  Returns zero on success, non-zero
 * otherwise.
 */
static int
check_executable (char *file, char **app)
{
  struct stat buf;

  /* Check the existence of the file.  */
  if (stat (file, &buf) < 0)
    {
      svz_log_sys_error ("passthrough: stat");
      return -1;
    }

#ifndef __MINGW32__
  if (!(buf.st_mode & S_IFREG) || !(buf.st_mode & S_IXUSR) ||
      !(buf.st_mode & S_IRUSR))
#else
  if (!(buf.st_mode & S_IFREG))
#endif
    {
      svz_log (SVZ_LOG_ERROR, "passthrough: no executable: %s\n", file);
      return -1;
    }

  if (app != NULL)
    *app = NULL;

#ifdef __MINGW32__
  if (app)
    {
      char *suffix = strrchr (file, '.');
      suffix = suffix ? suffix + 1 : file;

#define SUFFIX_IS_CI(k)  (! strncasecmp (k, suffix, sizeof (k)))

      /* Does the file have a known suffix?  */
      if (SUFFIX_IS_CI ("com")
          || SUFFIX_IS_CI ("exe")
          || SUFFIX_IS_CI ("bat"))
        return 0;

#undef SUFFIX_IS_CI

      /* No.  Try finding an application able to execute the script.  */
      *app = svz_malloc (MAX_PATH);
      if (FindExecutable (file, NULL, *app) <= (HINSTANCE) 32)
        {
          svz_log_sys_error ("passthrough: FindExecutable");
          svz_free (*app);
          *app = NULL;
          return -1;
        }
    }
#endif /* __MINGW32__ */

  return 0;
}

/*
 * Disconnection routine for the passthrough socket structure @var{sock}
 * connected with a process's stdin/stdout.  Schedules the referring socket
 * connection for shutdown if necessary and possible.
 */
static int
disconnect_passthrough (svz_socket_t *sock)
{
  svz_socket_t *xsock;

  if ((xsock = svz_sock_getreferrer (sock)) != NULL)
    {
      svz_sock_setreferrer (sock, NULL);
      svz_sock_setreferrer (xsock, NULL);
      if (sock->flags & (SVZ_PROTO_TCP | SVZ_PROTO_PIPE))
        {
#if ENABLE_DEBUG
          svz_log (SVZ_LOG_DEBUG, "passthrough: shutting down referring id %d\n",
                   xsock->id);
#endif
          svz_sock_schedule_for_shutdown (xsock);
        }
    }

  /* Clean up receive and send buffer.  */
  sock->recv_buffer = sock->send_buffer = NULL;
  sock->recv_buffer_fill = sock->recv_buffer_size = 0;
  sock->send_buffer_fill = sock->send_buffer_size = 0;
  return 0;
}

/*
 * If the argument @var{set} is zero this routine makes the receive buffer
 * of the socket structure's referrer @var{sock} the send buffer of @var{sock}
 * itself, otherwise the other way around.  Return non-zero if the routine
 * failed to determine a referrer.
 */
static int
send_switch_buffers (svz_socket_t *sock, int set)
{
  svz_socket_t *xsock;

  if ((xsock = svz_sock_getreferrer (sock)) == NULL)
    return -1;

  if (set)
    {
      sock->send_buffer = xsock->recv_buffer;
      sock->send_buffer_fill = xsock->recv_buffer_fill;
      sock->send_buffer_size = xsock->recv_buffer_size;
    }
  else
    {
      xsock->recv_buffer = sock->send_buffer;
      xsock->recv_buffer_fill = sock->send_buffer_fill;
      xsock->recv_buffer_size = sock->send_buffer_size;
    }
  return 0;
}

/*
 * This is the shuffle socket pair writer which is directly connected with
 * the reading end of the child process.  It writes as much data as possible
 * from its send buffer which is the receive buffer of the original
 * passthrough connection.
 */
static int
send_socket (svz_socket_t *sock)
{
  int num_written, do_write;

  /* update send buffer depending on receive buffer of referrer */
  if (send_switch_buffers (sock, 1))
    return -1;

  /* return here if there is nothing to do */
  if ((do_write = sock->send_buffer_fill) <= 0)
    return 0;

  if ((num_written = send (sock->sock_desc,
                           sock->send_buffer, do_write, 0)) == -1)
    {
      svz_log_sys_error ("passthrough: send");
      if (svz_errno == EAGAIN)
        num_written = 0;
    }
  else if (num_written > 0)
    {
      sock->last_send = time (NULL);
      svz_sock_reduce_send (sock, num_written);
      send_switch_buffers (sock, 0);
    }

  return (num_written >= 0) ? 0 : -1;
}

/*
 * Depending on the flag @var{set} this routine either makes the send buffer
 * of the referring socket structure of @var{sock} the receive buffer of
 * @var{sock} itself or the other way around.  Returns non-zero if it failed
 * to find a referring socket.
 */
static int
recv_switch_buffers (svz_socket_t *sock, int set)
{
  svz_socket_t *xsock;

  if ((xsock = svz_sock_getreferrer (sock)) == NULL)
    return -1;

  if (set)
    {
      sock->recv_buffer = xsock->send_buffer;
      sock->recv_buffer_fill = xsock->send_buffer_fill;
      sock->recv_buffer_size = xsock->send_buffer_size;
    }
  else
    {
      xsock->send_buffer = sock->recv_buffer;
      xsock->send_buffer_fill = sock->recv_buffer_fill;
      xsock->send_buffer_size = sock->recv_buffer_size;
    }
  return 0;
}

/*
 * This is the shuffle socket pair reader which is directly connected
 * with the writing end of the child process.  It reads as much data as
 * possible into its receive buffer which is the send buffer of the
 * original passthrough connection.
 */
static int
recv_socket (svz_socket_t *sock)
{
  int num_read, do_read;

  /* update receive buffer depending on send buffer of referrer */
  if (recv_switch_buffers (sock, 1))
    return -1;

  /* return here if there is nothing to do */
  if ((do_read = sock->recv_buffer_size - sock->recv_buffer_fill) <= 0)
    return 0;

  if ((num_read = recv (sock->sock_desc,
                        sock->recv_buffer + sock->recv_buffer_fill,
                        do_read, 0)) == -1)
    {
      svz_log_sys_error ("passthrough: recv");
      if (svz_errno == EAGAIN)
        return 0;
    }
  else if (num_read > 0)
    {
      sock->last_recv = time (NULL);
      sock->recv_buffer_fill += num_read;
      recv_switch_buffers (sock, 0);
    }
  else
    svz_log (SVZ_LOG_ERROR, "passthrough: recv: no data on socket %d\n",
             sock->sock_desc);

  return (num_read > 0) ? 0 : -1;
}

/*
 * This is the shuffle pipe writer (reading end of a process's stdin).  It
 * writes as much data as possible from the send buffer which is the receive
 * buffer of the referring socket structure.  Returns non-zero on errors.
 */
static int
send_pipe (svz_socket_t *sock)
{
  int num_written, do_write;

  /* update send buffer depending on receive buffer of referrer */
  if (send_switch_buffers (sock, 1))
    return -1;

  /* return here if there is nothing to do */
  if ((do_write = sock->send_buffer_fill) <= 0)
    return 0;

#ifndef __MINGW32__
  if ((num_written = write ((int) sock->pipe_desc[SVZ_WRITE],
                            sock->send_buffer, do_write)) == -1)
    {
      svz_log_sys_error ("passthrough: write");
      if (svz_errno == EAGAIN)
        num_written = 0;
    }
#else /* __MINGW32__ */
   if (!WriteFile (sock->pipe_desc[SVZ_WRITE], sock->send_buffer,
                   do_write, (DWORD *) &num_written, NULL))
    {
      svz_log_sys_error ("passthrough: WriteFile");
      num_written = -1;
    }
#endif /* __MINGW32__ */

  else if (num_written > 0)
    {
      sock->last_send = time (NULL);
      svz_sock_reduce_send (sock, num_written);
      send_switch_buffers (sock, 0);
    }

  return (num_written >= 0) ? 0 : -1;
}

/*
 * This is the shuffle pipe reader (writing end of a process's stdout).  It
 * reads as much data as possible into its receive buffer which is the send
 * buffer of the connection this passthrough pipe socket structure stems
 * from.
 */
static int
recv_pipe (svz_socket_t *sock)
{
  int num_read, do_read;

  /* update receive buffer depending on send buffer of referrer */
  if (recv_switch_buffers (sock, 1))
    return -1;

  /* return here if there is nothing to do */
  if ((do_read = sock->recv_buffer_size - sock->recv_buffer_fill) <= 0)
    return 0;

#ifndef __MINGW32__
  if ((num_read = read ((int) sock->pipe_desc[SVZ_READ],
                        sock->recv_buffer + sock->recv_buffer_fill,
                        do_read)) == -1)
    {
      svz_log_sys_error ("passthrough: read");
      if (svz_errno == EAGAIN)
        return 0;
    }
#else /* __MINGW32__ */
  if (!PeekNamedPipe (sock->pipe_desc[SVZ_READ], NULL, 0,
                      NULL, (DWORD *) &num_read, NULL))
    {
      svz_log_sys_error ("passthrough: PeekNamedPipe");
      return -1;
    }
  if (do_read > num_read)
    do_read = num_read;
  if (!ReadFile (sock->pipe_desc[SVZ_READ],
                 sock->recv_buffer + sock->recv_buffer_fill,
                 do_read, (DWORD *) &num_read, NULL))
    {
      svz_log_sys_error ("passthrough: ReadFile");
      num_read = -1;
    }
#endif /* __MINGW32__ */

  else if (num_read > 0)
    {
      sock->last_recv = time (NULL);
      sock->recv_buffer_fill += num_read;
      recv_switch_buffers (sock, 0);
    }

  return (num_read > 0) ? 0 : -1;
}

/*
 * Disconnection routine for the socket connection @var{sock} which is
 * connected with a process's stdin/stdout via the referring passthrough
 * socket structure which gets also scheduled for shutdown if possible.
 */
static int
shut_down_referrer (svz_socket_t *sock)
{
  svz_socket_t *xsock;

  if ((xsock = svz_sock_getreferrer (sock)) != NULL)
    {
      svz_sock_setreferrer (sock, NULL);
      svz_sock_setreferrer (xsock, NULL);
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "passthrough: shutting down referring id %d\n",
               xsock->id);
#endif
      svz_sock_schedule_for_shutdown (xsock);
    }
  return 0;
}

/*
 * Check request routine for the original passthrough connection @var{sock}.
 * Sets the send buffer fill counter of the referring socket structure which
 * is the passthrough connection in order to schedule it for sending.
 */
static int
check_request (svz_socket_t *sock)
{
  svz_socket_t *xsock;

  if ((xsock = svz_sock_getreferrer (sock)) == NULL)
    return -1;
  xsock->send_buffer_fill = sock->recv_buffer_fill;
  return 0;
}

#ifdef __MINGW32__
/**
 * Check child pointed at by @var{pid} by waiting a bit.
 * If it is dead, close and invalidate its handle, and return 1.
 * Otherwise, return 0.
 * @var{prefix} is for error messages; it should be either the
 * empty string, or a string ending in colon and space.
 */
int
svz_mingw_child_dead_p (char *prefix, svz_t_handle *pid)
{
  DWORD result;

  if (!prefix)
    abort ();
  result = WOE_WAIT_1 (*pid);
  if (result == WAIT_FAILED)
    WOE_WAIT_LOG_ERROR (prefix);
  else if (result != WAIT_TIMEOUT)
    {
      if (svz_closehandle (*pid) == -1)
        svz_log_sys_error ("%sCloseHandle", prefix);
      svz_child_died = *pid;
      svz_invalidate_handle (pid);
      return 1;
    }
  return 0;
}
#endif  /* __MINGW32__ */

/**
 * Return 1 if a child process @var{pid} died recently, updating
 * other internal state by side effect.  Otherwise, return 0.
 */
int
svz_most_recent_dead_child_p (svz_t_handle pid)
{
  if (svz_child_died && pid == svz_child_died)
    {
      svz_child_died = 0;
      return 1;
    }
  return 0;
}

/*
 * Idle function for the passthrough shuffle connection @var{sock}.  The
 * routine checks whether the spawned child process is still valid.  If not
 * it schedules the connection for shutdown.  The routine schedules itself
 * once a second.
 */
static int
mind_children (svz_socket_t *sock)
{
#ifndef __MINGW32__

#if HAVE_WAITPID
  /* Test if the passthrough child is still running.  */
  if (waitpid (sock->pid, NULL, WNOHANG) == -1 && errno == ECHILD)
    {
      svz_log (SVZ_LOG_NOTICE, "passthrough: shuffle pid %d died\n",
               (int) sock->pid);
      svz_invalidate_handle (&sock->pid);
      return -1;
    }
#endif /* HAVE_WAITPID */

#else /* __MINGW32__ */

  if (svz_mingw_child_dead_p ("passthrough: ", &sock->pid))
    return -1;

#endif /* __MINGW32__ */

  sock->idle_counter = 1;
  return 0;
}

/* A forward declaration for the benefit of ‘shuffle’.  */
static int create_child (svz_process_t *proc);

/*
 * Creates two pairs of pipes in order to passthrough the transactions of
 * the a socket structure.  The function create a new socket structure and
 * sets it up for handling the transactions automatically.  The given
 * argument @var{proc} contains the information inherited from
 * @code{svz_sock_process}.  The function returns -1 on failure and the
 * new child's process id otherwise.
 */
static int
shuffle (svz_process_t *proc)
{
  svz_t_socket pair[2];
  svz_t_handle process_to_serveez[2] = { 0 };
  svz_t_handle serveez_to_process[2] = { 0 };
  svz_socket_t *xsock;
  int pid;

  if (proc->flag == SVZ_PROCESS_SHUFFLE_SOCK)
    {
      /* create the pair of sockets */
      if (svz_socket_create_pair (proc->sock->proto, pair) < 0)
        return -1;
      /* create yet another socket structure */
      if ((xsock = svz_sock_create ((int) pair[1])) == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "passthrough: failed to create socket\n");
          return -1;
        }
    }
  else
    {
      /* create the pairs of pipes for the process */
      if (svz_pipe_create_pair (process_to_serveez) == -1)
        return -1;
      if (svz_pipe_create_pair (serveez_to_process) == -1)
        return -1;
      /* create yet another socket structure */
      if ((xsock = svz_pipe_create (process_to_serveez[SVZ_READ],
                                    serveez_to_process[SVZ_WRITE])) == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "passthrough: failed to create pipe\n");
          return -1;
        }
    }

  /* prepare everything for the pipe handling */
  xsock->cfg = proc->sock->cfg;
  xsock->disconnected_socket = disconnect_passthrough;
  if (proc->flag == SVZ_PROCESS_SHUFFLE_SOCK)
    {
      xsock->write_socket = send_socket;
      xsock->read_socket = recv_socket;
    }
  else
    {
      xsock->write_socket = send_pipe;
      xsock->read_socket = recv_pipe;
    }

  /* release receive and send buffers of the new socket structure */
  svz_free_and_zero (xsock->recv_buffer);
  xsock->recv_buffer_fill = xsock->recv_buffer_size = 0;
  svz_free_and_zero (xsock->send_buffer);
  xsock->send_buffer_fill = xsock->send_buffer_size = 0;

  /* let both socket structures refer to each other */
  svz_sock_setreferrer (proc->sock, xsock);
  svz_sock_setreferrer (xsock, proc->sock);

  /* setup original socket structure */
  proc->sock->disconnected_socket = shut_down_referrer;
  proc->sock->check_request = check_request;

  /* enqueue the new passthrough pipe socket */
  if (svz_sock_enqueue (xsock) < 0)
    return -1;

  if (proc->flag == SVZ_PROCESS_SHUFFLE_SOCK)
    proc->in = proc->out = (svz_t_handle) pair[0];
  else
    {
      proc->in = serveez_to_process[SVZ_READ];
      proc->out = process_to_serveez[SVZ_WRITE];
    }

  /* create a process and pass the left-over pipe ends to it */
#ifndef __MINGW32__
  if ((pid = fork ()) == 0)
    {
      create_child (proc);
      exit (EXIT_SUCCESS);
    }
  else if (pid == -1)
    {
      svz_log_sys_error ("passthrough: fork");
      return -1;
    }
#else /* __MINGW32__ */
  pid = create_child (proc);
  if (proc->envp)
    svz_envblock_destroy (proc->envp);
#endif /*  __MINGW32__ */

  /* close the passed descriptors */
  svz_closehandle (proc->in);
  if (proc->flag == SVZ_PROCESS_SHUFFLE_PIPE)
    svz_closehandle (proc->out);

  /* setup child checking callback */
  xsock->pid = (svz_t_handle) pid;
  xsock->idle_func = mind_children;
  xsock->idle_counter = 1;
#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "process `%s' got pid %d\n", proc->bin, pid);
#endif
  return pid;
}

/*
 * Fork the current process and execute a new child program.  The given
 * argument @var{proc} contains the information inherited from
 * @code{svz_sock_process}.  The routine passes the socket or pipe
 * descriptors of the original passthrough socket structure to stdin and
 * stdout of the child.  The caller is responsible for shutting down the
 * original socket structure.  Returns -1 on errors and the child's
 * process id on success.
 */
static int
via_fork (svz_process_t *proc)
{
  int pid;

#ifdef __MINGW32__
  pid = create_child (proc);
  if (proc->envp)
    svz_envblock_destroy (proc->envp);
#else /* __MINGW32__ */
  if ((pid = fork ()) == 0)
    {
      create_child (proc);
      exit (EXIT_SUCCESS);
    }
  else if (pid == -1)
    {
      svz_log_sys_error ("passthrough: fork");
      return -1;
    }
#endif /* __MINGW32__ */

  /* The parent process.  */
#if ENABLE_DEBUG
  svz_log (SVZ_LOG_DEBUG, "process `%s' got pid %d\n", proc->bin, pid);
#endif
  return pid;
}

/**
 * Start a new program @var{bin}, a fully qualified executable filename,
 * passing the socket or pipe descriptor(s) in the socket structure
 * @var{sock} to its stdin and stdout.
 *
 * If @var{dir} is non-@code{NULL}, it specifies the working directory of
 * the new process.
 *
 * The program arguments and the environment of the new process are taken
 * from @var{argv} and @var{envp}.  Normally @code{argv[0]} should be set to
 * the program's name.  If @code{NULL}, it defaults to @var{bin}.
 *
 * The @var{forkp} argument is a flag that controls the passthrough method.
 * If non-zero, pipe descriptors or the socket descriptor are passed to the
 * child process directly through @code{fork} and @code{exec}.  Otherwise,
 * socket transactions are passed via a pair or pipes or sockets (depending
 * on whether or not the system provides @code{socketpair}).
 *
 * You can pass the user and group identifications in the format
 * @samp{user[.group]} (group is optional), as @code{SVZ_PROCESS_NONE} or
 * @code{SVZ_PROCESS_OWNER} in the @var{user} argument.  This specifies the
 * permissions of the new child process.  If @code{SVZ_PROCESS_OWNER} is
 * passed the permissions are set to the executable file @var{bin} owner;
 * @code{SVZ_PROCESS_NONE} does not change user or group.
 *
 * Return the new process id on success, -1 on failure.
 */
int
svz_sock_process (svz_socket_t *sock, char *bin, char *dir,
                  char **argv, svz_envblock_t *envp, int forkp, char *user)
{
  svz_process_t proc;
  int ret = -1;

  /* Check arguments.  */
  if (sock == NULL || bin == NULL || argv == NULL)
    {
      svz_log (SVZ_LOG_ERROR, "passthrough: invalid argument\n");
      return -1;
    }

  /* Setup descriptors depending on the type of socket structure.  */
  if (sock->flags & SVZ_SOFLG_PIPE)
    {
      proc.in = sock->pipe_desc[SVZ_READ];
      proc.out = sock->pipe_desc[SVZ_WRITE];
    }
  else
    {
      proc.in = proc.out = (svz_t_handle) sock->sock_desc;
    }

  /* Check executable.  */
  if (check_executable (bin, &proc.app) < 0)
    return -1;

  /* Fill in rest of process structure.  */
  proc.sock = sock;
  proc.bin = bin;
  proc.dir = dir;
  proc.argv = argv;
  proc.envp = envp;
  proc.user = user;
  proc.flag = forkp
    ? SVZ_PROCESS_FORK
    :
#if HAVE_SOCKETPAIR
    SVZ_PROCESS_SHUFFLE_SOCK
#else
    SVZ_PROCESS_SHUFFLE_PIPE
#endif
    ;

  /* Depending on the given flag use different methods to passthrough
     the connection.  */
  switch (proc.flag)
    {
    case SVZ_PROCESS_FORK:
      ret = via_fork (&proc);
      break;
    case SVZ_PROCESS_SHUFFLE_SOCK:
    case SVZ_PROCESS_SHUFFLE_PIPE:
      ret = shuffle (&proc);
      break;
    }

  return ret;
}

#ifdef __MINGW32__
/*
 * This function duplicates a given @var{handle} in the sense of @code{dup}.
 * The returned handle references the same underlying object.  If
 * an invalid handle is returned something went wrong.  The @var{proto}
 * argument specifies if it is a socket or pipe handle.
 */
static svz_t_handle
duplicate_handle (svz_t_handle handle, int proto)
{
  svz_t_handle duphandle;
  svz_t_socket dupsock;
  WSAPROTOCOL_INFO info;

  /* Duplicate a pipe handle.  */
  if (proto & SVZ_PROTO_PIPE)
    {
      if (!DuplicateHandle (GetCurrentProcess (), handle,
                            GetCurrentProcess (), &duphandle,
                            DUPLICATE_SAME_ACCESS, TRUE, 0))
        {
          svz_log_sys_error ("passthrough: DuplicateHandle");
          svz_invalidate_handle (&duphandle);
        }
      return duphandle;
    }

  /* Duplicate a socket.  */
  if (WSADuplicateSocket ((svz_t_socket) handle, GetCurrentProcessId (),
                          &info) == SOCKET_ERROR)
    {
      svz_log_net_error ("passthrough: WSADuplicateSocket");
      svz_invalidate_handle (&duphandle);
      return duphandle;
    }
  if ((dupsock = WSASocket (FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO,
                            FROM_PROTOCOL_INFO,
                            &info, 0, 0)) == INVALID_SOCKET)
    {
      svz_log_net_error ("passthrough: WSASocket");
      svz_invalidate_handle (&duphandle);
      return duphandle;
    }
  return (svz_t_handle) dupsock;
}
#endif /* __MINGW32__ */

/*
 * Splits the given character string @var{str} in the format
 * @samp{user[.group]} into a user name and a group name and stores
 * pointers to it in @var{user} and @var{group}.  If the group has been
 * omitted in the format string then @var{group} is @code{NULL}
 * afterwards.  The function returns zero on success, or non-zero if the
 * given arguments have been invalid.
 */
static int
split_usergroup (char *str, char **user, char **group)
{
  static char copy[128], *p;

  if (user == NULL || group == NULL)
    return -1;
  *user = *group = NULL;
  if (str == NULL || strlen (str) >= sizeof (copy) - 1)
    return -1;
  strcpy (copy, str);
  if ((p = strchr (copy, '.')) != NULL)
    {
      *group = p + 1;
      *p = '\0';
    }
  *user = copy;
  return 0;
}

/*
 * Try setting the user and group for the current process specified by the
 * given executable file @var{file} and the @var{user} argument.  If @var{user}
 * equals @code{SVZ_PROCESS_NONE} no user or group is set.  When you pass
 * @code{SVZ_PROCESS_OWNER} in the @var{user} argument the @var{file}'s owner
 * will be set.  Otherwise @var{user} specifies the user and group
 * identification in the format @samp{user[.group]}.  If you omit the group
 * information the routine uses the primary group of the user.  Returns zero
 * on success, non-zero otherwise.
 */
static int
check_access (char *file, char *user)
{
  struct stat buf;

  /* get the executable permissions */
  if (stat (file, &buf) == -1)
    {
      svz_log_sys_error ("passthrough: stat");
      return -1;
    }

#ifndef __MINGW32__
  /* set the appropriate user and group permissions for file owner */
  if (user == SVZ_PROCESS_OWNER)
    {
      if (setgid (buf.st_gid) == -1)
        {
          svz_log_sys_error ("passthrough: setgid");
          return -1;
        }
      if (setuid (buf.st_uid) == -1)
        {
          svz_log_sys_error ("passthrough: setuid");
          return -1;
        }
    }
  /* set given user and group */
  else if (user != SVZ_PROCESS_NONE)
    {
      char *_user = NULL, *_group = NULL;
      struct passwd *u = NULL;
      struct group *g = NULL;

      split_usergroup (user, &_user, &_group);

      /* Group name specified?  */
      if (_group != NULL)
        {
          if ((g = getgrnam (_group)) == NULL)
            {
              svz_log (SVZ_LOG_ERROR, "passthrough: no such group `%s'\n", _group);
              return -1;
            }
          /* Set the group.  */
          if (setgid (g->gr_gid) == -1)
            {
              svz_log_sys_error ("passthrough: setgid");
              return -1;
            }
        }

      /* Check user name.  */
      if ((u = getpwnam (_user)) == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "passthrough: no such user `%s'\n", _user);
          return -1;
        }
      /* No group name specified.  Use the user's one.  */
      if (_group == NULL)
        {
          if (setgid (u->pw_gid) == -1)
            {
              svz_log_sys_error ("passthrough: setgid");
              return -1;
            }
        }
      /* Set the user.  */
      if (setuid (u->pw_uid) == -1)
        {
          svz_log_sys_error ("setuid");
          return -1;
        }
    }
#endif /* not __MINGW32__ */

  return 0;
}

/*
 * Spawns a new child process.  The given @var{proc} argument contains all
 * information necessary to set a working directory, assign a new user
 * defined stdin and stdout of the new process, to set up a process
 * environment, pass a command line to the new process and to specify a
 * user and group identification the child process should have.  The routine
 * returns -1 on failure, otherwise the new child program's process id.
 */
static int
create_child (svz_process_t *proc)
{
#ifndef __MINGW32__
  /* Change directory, make descriptors blocking, setup environment,
     set permissions, duplicate descriptors and finally execute the
     program.  */

  if (proc->dir && chdir (proc->dir) < 0)
    {
      svz_log_sys_error ("passthrough: chdir (%s)", proc->dir);
      return -1;
    }

  if (svz_fd_block (proc->out) < 0 || svz_fd_block (proc->in) < 0)
    return -1;

  if (dup2 (proc->out, 1) != 1 || dup2 (proc->in, 0) != 0)
    {
      svz_log_sys_error ("passthrough: unable to redirect");
      return -1;
    }

  if (check_access (proc->bin, proc->user) < 0)
    return -1;

  /* Check the environment and create a default one if necessary.  */
  if (proc->envp == NULL)
    {
      proc->envp = svz_envblock_create ();
      svz_envblock_default (proc->envp);
    }

  if (proc->argv[0] == NULL)
    proc->argv[0] = proc->bin;

  /* Disconnect this process from our TTY.  */
  close (fileno (stderr));

  /* Execute the file itself here overwriting the current process.  */
  if (execve (proc->bin, proc->argv, svz_envblock_get (proc->envp)) == -1)
    {
      svz_log_sys_error ("passthrough: execve");
      return -1;
    }

  /* Not reached.  */
  return getpid ();

#else /* __MINGW32__ */

  STARTUPINFO startup_info;
  PROCESS_INFORMATION process_info;
  char *savedir, *application;
  int pid, n;

  /* Clean the Startup-Info, use the stdio handles, and store the pipe
     handles there if necessary.  */
  memset (&startup_info, 0, sizeof (startup_info));
  startup_info.cb = sizeof (startup_info);
  startup_info.dwFlags = STARTF_USESTDHANDLES;

  /* For ‘fork’ and ‘exec’ emulation we need to duplicate the handles
     and pass them to the child program.  */
  if (proc->flag == SVZ_PROCESS_FORK)
    {
      svz_t_handle fd;

      if (proc->in != proc->out)
        {
          /* Create an inheritable receive pipe and replace it.  */
          fd = duplicate_handle (proc->in, proc->sock->proto);
          if (svz_invalid_handle_p (fd))
            return -1;
          svz_closehandle (proc->sock->pipe_desc[SVZ_READ]);
          proc->in = proc->sock->pipe_desc[SVZ_READ] = fd;

          /* Create an inheritable send pipe and replace it.  */
          fd = duplicate_handle (proc->out, proc->sock->proto);
          if (svz_invalid_handle_p (fd))
            return -1;
          svz_closehandle (proc->sock->pipe_desc[SVZ_WRITE]);
          proc->out = proc->sock->pipe_desc[SVZ_WRITE] = fd;
        }
      else
        {
          /* Create an inheritable socket and replace it.  */
          fd = duplicate_handle (proc->in, proc->sock->proto);
          if (svz_invalid_handle_p (fd))
            return -1;
          proc->in = proc->out = fd;
          svz_closesocket (proc->sock->sock_desc);
          proc->sock->sock_desc = (svz_t_socket) fd;
        }
    }

  /* Now assign the standard input and standard output handles.
     FIXME: This does not work correctly for socket handles.  */
  startup_info.hStdInput = proc->in;
  startup_info.hStdOutput = proc->out;
  startup_info.hStdError = GetStdHandle (STD_ERROR_HANDLE);

  /* Save current directory and change into application's.  */
  savedir = svz_getcwd ();
  if (proc->dir && chdir (proc->dir) < 0)
    {
      svz_log_sys_error ("passthrough: chdir (%s)", proc->dir);
      svz_free (savedir);
      return -1;
    }

  /* Check the access to the file.  */
  if (check_access (proc->bin, proc->user) < 0)
    {
      chdir (savedir);
      svz_free (savedir);
      return -1;
    }

  /* Create sane environment and pass the receive and send handle to
     the child process via two environment variables.  */
  if (proc->envp == NULL)
    {
      proc->envp = svz_envblock_create ();
      svz_envblock_default (proc->envp);
    }
  svz_envblock_add (proc->envp, "%s_HANDLE=%ld", "RECV", (long) proc->in);
  svz_envblock_add (proc->envp, "%s_HANDLE=%ld", "SEND", (long) proc->out);

  /* Concatenate application name.  */
  {
    int len;
    char *w;

    /* First compute total allocation.  */
    len = strlen (proc->bin) + 1;       /* bin + '\0' */
    if (proc->app != NULL)
      len += strlen (proc->app) + 1;    /* app + ' ' */
    for (n = 1; proc->argv[n] != NULL; n++)
      len += 1 + strlen (proc->argv[n]); /* ' ' + arg */

    /* Create the string.  */
    w = application = svz_malloc (len);
    if (proc->app != NULL)
      {
        len = strlen (proc->app);
        memcpy (w, proc->app, len);
        w += len;
        *w++ = ' ';
      }
    len = strlen (proc->bin);
    memcpy (w, proc->bin, len);
    w += len;
    for (n = 1; prog->argv[n] != NULL; n++)
      {
        *w++ = ' ';
        len = strlen (proc->argv[n]);
        memcpy (w, proc->argv[n], len);
        w += len;
      }
    *w = '\0';
  }

  if (!CreateProcess (NULL,                          /* application name */
                      application,                   /* command line */
                      NULL,                          /* process attributes */
                      NULL,                          /* thread attributes */
                      TRUE,                          /* inherit handles */
                      DETACHED_PROCESS,              /* creation flags */
                      svz_envblock_get (proc->envp), /* environment */
                      NULL,                          /* current directory */
                      &startup_info, &process_info))
    {
      svz_log_sys_error ("passthrough: CreateProcess (%s)", application);
      chdir (savedir);
      svz_free (savedir);
      svz_free (application);
      return -1;
    }

  chdir (savedir);
  svz_free (savedir);
  svz_free (application);
  pid = (int) process_info.hProcess;
  return pid;
#endif /* __MINGW32__ */
}

/**
 * Create and return a fresh environment block, useful for passing
 * to @code{svz_envblock_default} and @code{svz_envblock_add}.  Its
 * size is initially set to zero.
 */
svz_envblock_t *
svz_envblock_create (void)
{
  return svz_calloc (sizeof (svz_envblock_t));
}

/*
 * This function releases all environment variables currently stored in the
 * given environment block @var{env}.  The block will be as clean as returned
 * by @code{svz_envblock_create} afterwards.
 */
static int
envblock_free (svz_envblock_t *env)
{
  int n;

  if (env == NULL)
    return -1;
  for (n = 0; n < env->size; n++)
    svz_free (env->entry[n]);
  env->block = NULL;
  svz_free_and_zero (env->entry);
  env->size = 0;
  return 0;
}

/**
 * Fill environment block @var{env} with the environment variables from
 * the current process, replacing its current contents (if any).
 */
int
svz_envblock_default (svz_envblock_t *env)
{
  int n;

  if (env == NULL)
    return -1;
  if (env->size)
    envblock_free (env);

  for (n = 0; environment != NULL && environment[n] != NULL; n++)
    {
      env->size++;
      env->entry = svz_realloc (env->entry,
                                sizeof (char *) * (env->size + 1));
      env->entry[env->size - 1] = svz_strdup (environment[n]);
    }

  env->entry[env->size] = NULL;
  return 0;
}

/**
 * Insert a new environment variable into environment block
 * @var{env}.  The @var{format} argument is a @code{printf}-style format
 * string describing how to format the optional arguments.  You specify
 * environment variables in the @samp{VAR=VALUE} format.
 */
int
svz_envblock_add (svz_envblock_t *env, char *format, ...)
{
  /* FIXME: Add parm ‘var’; do formatting only for value.  */
  static char buffer[VSNPRINTF_BUF_SIZE];
  int n, len;
  va_list args;

  va_start (args, format);
  vsnprintf (buffer, VSNPRINTF_BUF_SIZE, format, args);
  va_end (args);

  /* Check for duplicate entry.  */
  len = strchr (buffer, '=') - buffer;
  for (n = 0; n < env->size; n++)
    if (!memcmp (buffer, env->entry[n], len))
      {
        svz_free (env->entry[n]);
        env->entry[n] = svz_strdup (buffer);
        return env->size;
      }

  env->size++;
  env->entry = svz_realloc (env->entry, sizeof (char *) * (env->size + 1));
  env->entry[env->size - 1] = svz_strdup (buffer);
  env->entry[env->size] = NULL;
  return env->size;
}

#ifdef __MINGW32__
/*
 * Win9x and WinNT systems use sorted environments.  That is why we will sort
 * each environment block passed to @code{CreateProcess}.  The following
 * routine is the comparison routine for the @code{qsort} call.
 */
static int
envblock_sort (const void *data1, const void *data2)
{
  char *entry1 = * (char **) data1;
  char *entry2 = * (char **) data2;
  return strcmp (entry1, entry2);
}
#endif /* __MINGW32__ */

/**
 * Convert environment block @var{env} into something which can be passed to
 * @code{execve} (Unix) or @code{CreateProcess} (Windows).  Additionally,
 * under Windows, sort the environment block.
 *
 * (Unfortunately the layout of environment blocks in Unices and Windows
 * differ.  On Unices you have a NULL terminated array of character strings
 * (i.e., @code{char **}) and on Windows systems you have a simple character
 * string containing the environment variables in the format @samp{VAR=VALUE}
 * each separated by a zero byte (i.e., @code{char *}).  The end of the list
 * is indicated by a further zero byte.)
 */
void *
svz_envblock_get (svz_envblock_t *env)
{
  char *dir;
#ifdef __MINGW32__
  int len = 32;
  char *block = NULL;
  int n, size;
#endif

  /* Setup the PWD environment variable correctly.  */
  dir = svz_getcwd ();
  svz_envblock_add (env, "PWD=%s", dir);
  svz_free (dir);

#ifdef __MINGW32__
  qsort ((void *) env->entry, env->size, sizeof (char *), envblock_sort);
  for (size = 1, n = 0; n < env->size; n++)
    {
      len = strlen (env->entry[n]) + 1;
      /* Use permanent allocator here.  You may not ‘free’ environment blocks
         passed to programs.  */
      block = svz_prealloc (block, size + len);
      memcpy (&block[size - 1], env->entry[n], len);
      size += len;
    }
  block[size] = '\0';
  env->block = block;
  return block;
#else /* !__MINGW32__ */
  return env->entry;
#endif /* !__MINGW32__ */
}

/**
 * Destroy environment block @var{env} completely.  Afterwards,
 * @var{env} is invalid and should therefore not be further referenced.
 */
void
svz_envblock_destroy (svz_envblock_t *env)
{
  envblock_free (env);
  svz_free (env);
}
