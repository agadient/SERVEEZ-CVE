/*
 * core.c - socket and file descriptor core implementations
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef __MINGW32__
# include <sys/socket.h>
#endif

#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_NETINET_TCP_H
# include <netinet/tcp.h>
#endif
#if HAVE_SYS_SENDFILE_H
# include <sys/sendfile.h>
#endif
#if defined (HAVE_SYS_UIO_H) && defined (__FreeBSD__)
# include <sys/uio.h>
#endif

#include "o-binary.h"
#include "networking-headers.h"
#include "woe-wait.h"

#if HAVE_MSWSOCK_H && defined (__MINGW32__)
# include <mswsock.h>
#endif

#include "libserveez/util.h"
#include "libserveez/core.h"
#include "misc-macros.h"

/*
 * Set the given file descriptor to nonblocking I/O.  This heavily differs
 * in Win32 and Unix.  The given file descriptor @var{fd} must be a socket
 * descriptor under Win32, otherwise the function fails.  Return zero on
 * success, otherwise non-zero.
 */
int
svz_fd_nonblock (int fd)
{
#ifdef __MINGW32__
  unsigned long blockMode = 1;

  if (ioctlsocket (fd, FIONBIO, &blockMode) == SOCKET_ERROR)
    {
      svz_log_net_error ("ioctlsocket");
      return -1;
    }
#else /* not __MINGW32__ */
  int flag;

  flag = fcntl (fd, F_GETFL);
  if (fcntl (fd, F_SETFL, flag | O_NONBLOCK) < 0)
    {
      svz_log_net_error ("fcntl");
      return -1;
    }
#endif /* not __MINGW32__ */

  return 0;
}

/*
 * Set the given file descriptor to blocking I/O.  This routine is the
 * counter part to @code{svz_fd_nonblock}.
 */
int
svz_fd_block (int fd)
{
#ifdef __MINGW32__
  unsigned long blockMode = 0;

  if (ioctlsocket (fd, FIONBIO, &blockMode) == SOCKET_ERROR)
    {
      svz_log_net_error ("ioctlsocket");
      return -1;
    }
#else /* not __MINGW32__ */
  int flag;

  flag = fcntl (fd, F_GETFL);
  if (fcntl (fd, F_SETFL, flag & ~O_NONBLOCK) < 0)
    {
      svz_log_net_error ("fcntl");
      return -1;
    }
#endif /* not __MINGW32__ */

  return 0;
}

/**
 * Set the close-on-exec flag of the given file descriptor @var{fd} and
 * return zero on success.  Otherwise return non-zero.
 */
int
svz_fd_cloexec (int fd)
{
#ifndef __MINGW32__

  /*
   * ... SNIP : from the cygwin mail archives 1999/05 ...
   * The problem is in ‘socket’ call on W95 - the socket returned
   * is non-inheritable handle (unlike NT and Unixes, where
   * sockets are inheritable).  To fix the problem DuplicateHandle
   * call is used to create inheritable handle, and original
   * handle is closed.
   * ... SNAP ...
   *
   * Thus here is NO NEED to set the FD_CLOEXEC flag and no
   * chance anyway.
   */

  int flag;

  flag = fcntl (fd, F_GETFD);
  if ((fcntl (fd, F_SETFD, flag | FD_CLOEXEC)) < 0)
    {
      svz_log_net_error ("fcntl");
      return -1;
    }

#endif /* !__MINGW32__ */

  return 0;
}

/**
 * Close the socket @var{sock}.
 * Return 0 if successful, -1 otherwise.
 */
int
svz_closesocket (svz_t_socket sockfd)
{
#ifdef __MINGW32__
  return closesocket (sockfd);
#else
  return close (sockfd);
#endif
}

/*
 * This function creates an unnamed pair of connected sockets with the
 * specified protocol @var{proto}.  The descriptors used in referencing the
 * new sockets are returned in desc[0] and desc[1].  The two sockets are
 * indistinguishable.  Also make both of them non-blocking and
 * non-inheritable.  Returns -1 on failure, otherwise zero.
 */
int
svz_socket_create_pair (int proto, svz_t_socket desc[2])
{
#ifndef HAVE_SOCKETPAIR
  svz_log (SVZ_LOG_FATAL, "socketpair(2) not available\n");
  return -1;
#else

  int stype, ptype;

  /* Assign the appropriate socket type.  */
  switch (proto)
    {
    case SVZ_PROTO_TCP:
      stype = SOCK_STREAM;
      ptype = IPPROTO_IP;
      break;
    case SVZ_PROTO_UDP:
      stype = SOCK_DGRAM;
      ptype = IPPROTO_UDP;
      break;
    case SVZ_PROTO_ICMP:
      stype = SOCK_RAW;
      ptype = IPPROTO_ICMP;
      break;
    case SVZ_PROTO_RAW:
      stype = SOCK_RAW;
      ptype = IPPROTO_RAW;
      break;
    default:
      stype = SOCK_STREAM;
      ptype = IPPROTO_IP;
      break;
    }

  /* Create the pair of sockets.  */
  if (socketpair (AF_UNIX, stype, ptype, desc) < 0)
    {
      svz_log_net_error ("socketpair");
      return -1;
    }

  /* Make the sockets non-blocking and non-inheritable.  */
  if (svz_fd_nonblock (desc[0]) != 0 || svz_fd_nonblock (desc[1]) != 0 ||
      svz_fd_cloexec (desc[0]) != 0 || svz_fd_cloexec (desc[1]) != 0)
    {
      svz_closesocket (desc[0]);
      svz_closesocket (desc[1]);
      return -1;
    }

  return 0;
#endif /* HAVE_SOCKETPAIR */
}

/*
 * Create a new non-blocking socket which does not get inherited on
 * @code{exec}.  The protocol is specified by @var{proto}.  Return the
 * socket descriptor or -1 on errors.
 */
svz_t_socket
svz_socket_create (int proto)
{
  int stype;                 /* socket type (STREAM or DGRAM or RAW) */
  int ptype;                 /* protocol type (IP or UDP or ICMP) */
  svz_t_socket sockfd;

  /* Assign the appropriate socket type.  */
  switch (proto)
    {
    case SVZ_PROTO_TCP:
      stype = SOCK_STREAM;
      ptype = IPPROTO_IP;
      break;
    case SVZ_PROTO_UDP:
      stype = SOCK_DGRAM;
      ptype = IPPROTO_UDP;
      break;
    case SVZ_PROTO_ICMP:
      stype = SOCK_RAW;
      ptype = IPPROTO_ICMP;
      break;
      /* This protocol is for sending packets only.  The kernel filters
         any received packets by the socket protocol (here: IPPROTO_RAW
         which is unspecified).  */
    case SVZ_PROTO_RAW:
      stype = SOCK_RAW;
      ptype = IPPROTO_RAW;
      break;
    default:
      stype = SOCK_STREAM;
      ptype = IPPROTO_IP;
      break;
    }

  /* Create a socket for communication with a server.  */
  if ((sockfd = socket (AF_INET, stype, ptype)) == INVALID_SOCKET)
    {
      svz_log_net_error ("socket");
      return (svz_t_socket) -1;
    }

  /* Make the socket non-blocking.  */
  if (svz_fd_nonblock (sockfd) != 0)
    {
      svz_closesocket (sockfd);
      return (svz_t_socket) -1;
    }

  /* Do not inherit this socket.  */
  if (svz_fd_cloexec (sockfd) != 0)
    {
      svz_closesocket (sockfd);
      return (svz_t_socket) -1;
    }

  return sockfd;
}

/*
 * Connect the given socket descriptor @var{sockfd} to the host @var{host}
 * at the network port @var{port}.  Return non-zero on errors.
 */
int
svz_socket_connect (svz_t_socket sockfd, svz_address_t *host, in_port_t port)
{
  in_addr_t v4addr;
  struct sockaddr_in server;
  int error;

  STILL_NO_V6_DAMMIT (host);
  svz_address_to (&v4addr, host);

  /* Setup the server address.  */
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = v4addr;
  server.sin_port = port;

  /* Try to connect to the server, */
  if (connect (sockfd, (struct sockaddr *) &server, sizeof (server)) == -1)
    {
#ifdef __MINGW32__
      error = WSAGetLastError ();
      svz_errno = error;                /* Is this correct? --ttn  */
#else
      error = errno;
#endif
      if (error != SOCK_INPROGRESS
          && ! svz_socket_unavailable_error_p ())
        {
          svz_log_net_error ("connect");
          svz_closesocket (sockfd);
          return -1;
        }
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "connect: %s\n", svz_net_strerror ());
#endif
    }
  return 0;
}

/**
 * Convert @var{ip}, an address in network byte order, to its dotted
 * decimal representation, returning a pointer to a statically
 * allocated buffer.  (You should copy the result.)
 */
char *
svz_inet_ntoa (in_addr_t ip)
{
  struct in_addr addr;

  addr.s_addr = ip;
  return inet_ntoa (addr);
}

/**
 * Convert the Internet host address @var{str} from the standard
 * numbers-and-dots notation into binary data and store it in the
 * structure that @var{addr} points to.
 * Return zero if the address is valid, nonzero otherwise.
 * As a special case, if @var{str} is @samp{*} (asterisk),
 * store @code{INADDR_ANY} in @var{addr}.
 */
int
svz_inet_aton (char *str, struct sockaddr_in *addr)
{
  /* Handle "*" special: use INADDR_ANY for it */
  if (!strcmp (str, "*"))
    {
      /* FIXME: does that work?  */
      addr->sin_addr.s_addr = INADDR_ANY;
      return 0;
    }

  return svz_pton (str, &addr->sin_addr);
}

/**
 * Enable or disable the @code{TCP_CORK} socket option of the socket
 * @var{fd}.  This is useful for performance reasons when using
 * @code{sendfile} with any prepending or trailing data not inside the
 * file to transmit.  Return zero on success, otherwise non-zero.
 */
int
svz_tcp_cork (svz_t_socket fd, int set)
{
#ifdef TCP_CORK
  int flags;

  /* get current socket options */
  if ((flags = fcntl (fd, F_GETFL)) < 0)
    {
      svz_log_net_error ("fcntl");
      return -1;
    }

  /* set or unset the cork option */
  flags = set ? flags | TCP_CORK : flags & ~TCP_CORK;

  /* apply new socket option */
  if (fcntl (fd, F_SETFL, flags) < 0)
    {
      svz_log_net_error ("fcntl");
      return -1;
    }

#endif /* TCP_CORK */
  return 0;
}

/* M$-Windows compatibility definition.  */
#ifndef SOL_TCP
#define SOL_TCP IPPROTO_TCP
#endif

/**
 * Enable or disable the @code{TCP_NODELAY} setting for the socket
 * @var{fd} depending on the flag @var{set}, effectively enabling
 * or disabling the Nagle algorithm.
 * This means that packets are always sent
 * as soon as possible and no unnecessary delays are introduced.
 * If @var{old} is not @code{NULL}, save the old setting there.
 * Return zero on success, otherwise non-zero.
 */
int
svz_tcp_nodelay (svz_t_socket fd, int set, int *old)
{
#ifdef TCP_NODELAY
  int optval;
  socklen_t optlen = sizeof (optval);

  /* Get old setting if required.  */
  if (old != NULL)
    {
      if (getsockopt (fd, SOL_TCP, TCP_NODELAY,
                      (void *) &optval, &optlen) < 0)
        {
          svz_log_net_error ("getsockopt");
          return -1;
        }
      *old = optval ? 1 : 0;
    }

  /* Set the setting.  */
  optval = set ? 1 : 0;
  if (setsockopt (fd, SOL_TCP, TCP_NODELAY,
                  (void *) &optval, sizeof (optval)) < 0)
    {
      svz_log_net_error ("setsockopt");
      return -1;
    }
#else /* not TCP_NODELAY */
  if (old)
    {
      *old = 0;
    }
#endif /* not TCP_NODELAY */
  return 0;
}

/**
 * Transmit data between one file descriptor and another where
 * @var{in_fd} is the source and @var{out_fd} the destination.
 * The @var{offset} argument is a pointer to a variable holding
 * the input file pointer position from which reading starts.
 * On return, the @var{offset} variable will be set to the offset
 * of the byte following the last byte that was read.
 * @var{count} is the number of bytes to copy.
 * Return the number of bytes actually read/written or -1 on errors.
 */
int
svz_sendfile (int out_fd, int in_fd, off_t *offset, size_t count)
{
#ifndef ENABLE_SENDFILE
  svz_log (SVZ_LOG_ERROR, "sendfile(2) disabled\n");
  return -1;
#else
#if defined (HAVE_SENDFILE) || defined (__MINGW32__)
  int ret;
#if defined (__osf__)

  /* FIXME:
     On True64 ‘sendfile’ is said to crash the machine.  Moreover the
     system call is not documented.  Thus we do not know the exact meaning
     of the remaining arguments.  We definitely know that there must be
     some kind of pointer (maybe specifying head and tail).  */

  ret = sendfile (out_fd, in_fd, *offset, count, NULL, NULL, 0);
  *offset += ((ret >= 0) ? ret : 0);

#elif defined (__FreeBSD__)

  /* This was tested for FreeBSD4.3 on alpha.  */
  off_t sbytes;
  ret = sendfile (in_fd, out_fd, *offset, count, NULL, &sbytes, 0);
  *offset += sbytes;
  ret = ret ? -1 : (int) sbytes;

#elif defined (__MINGW32__)

  /* ‘TransmitFile’
     This system call provides something likely the Unix ‘sendfile’.  It
     is a M$ specific extension to Winsock.  The function comes from the
     MSWSOCK.DLL and should be prototyped in MSWSOCK.H.  It operates on
     file handles gained from ‘kernel32.CreateFile’ only.  We experienced
     quite low performance on small (less than 4096 byte) file chunks.
     Performance is better with about 32 KB per chunk.  The function is
     available on Windows NT, Windows 2000 and Windows XP only (not W95,
     W98 or ME).  */

  OVERLAPPED overlap = { 0, 0, 0, 0, NULL };
  DWORD result;

  /* Data transmission via overlapped I/O.
     The MSDN documentation tells nothing odd about passing NULL as
     overlapped structure argument, but we experienced that this does not
     work.  Thus we pass the overlapped structure with the Offset member
     set to the current file position.  */

  overlap.Offset = *offset;
  if (!TransmitFile ((svz_t_socket) out_fd, (svz_t_handle) in_fd, count, 0,
                     &overlap, NULL, 0))
    {
      /* Operation is pending.  */
      if (GetLastError () == ERROR_IO_PENDING)
        {
          /* Wait for the operation to complete (blocking).  We could either
             wait here for the socket handle itself or for the hEvent member
             of the overlapped structure which must be created previously.
             If waiting for the socket handle we need to ensure that no other
             thread is operating on the socket.  This is given since serveez
             is single threaded.  */

          if ((result = WOE_WAIT_INF ((svz_t_handle) out_fd)) != WAIT_OBJECT_0)
            {
              WOE_WAIT_LOG_ERROR_ANONYMOUSLY ();
              ret = -1;
            }
          /* Finally transmitted the data.  */
          else
            {
              *offset += count;
              ret = count;
            }
        }
      /* Some error occurred.  */
      else
        {
          svz_log_sys_error ("TransmitFile");
          ret = -1;
        }
    }
  /* Data transmission completed successfully at once.  */
  else
    {
      *offset += count;
      ret = count;
    }

#elif defined (__hpux)

  /* HP-UX 11i */
  ret = sendfile (out_fd, in_fd, *offset, (bsize_t) count, NULL, 0);
  *offset += ((ret >= 0) ? ret : 0);

#else

  /* Linux here.  Works like charm...  */
  ret = sendfile (out_fd, in_fd, offset, count);

#endif
  return ret;
#else
  svz_log (SVZ_LOG_ERROR, "sendfile(2) not available\n");
  return -1;
#endif /* HAVE_SEND_FILE */
#endif /* ENABLE_SENDFILE */
}

#ifndef __MINGW32__

/* List for file descriptors.  */
static svz_array_t *files = NULL;

/* Add a file descriptor to the list.  */
static void
add_file (int fd)
{
  if (files == NULL)
    files = svz_array_create (1, NULL);
  svz_array_add (files, SVZ_NUM2PTR (fd));
}

/* Delete a file descriptor from the list.  */
static void
remove_file (int fd)
{
  void *val;
  size_t n;

  svz_array_foreach (files, val, n)
    {
      if (val == SVZ_NUM2PTR (fd))
        {
          svz_array_del (files, n);
          break;
        }
    }
  if (svz_array_size (files) == 0)
    {
      svz_array_destroy (files);
      files = NULL;
    }
}

/*
 * Close all file descriptors collected so far by the core API of serveez.
 * This should be called if @code{fork} has been called without a
 * following @code{exec}.
 */
void
svz_file_closeall (void)
{
  void *fd;
  size_t n;

  svz_array_foreach (files, fd, n)
    close ((int) SVZ_PTR2NUM (fd));
}
#endif /* not __MINGW32__ */

/**
 * Open the filename @var{file} and convert it into a file handle.  The
 * given @var{flags} specify the access mode and the @var{mode} argument
 * the permissions if the @code{O_CREAT} flag is set.
 */
int
svz_open (const char *file, int flags, mode_t mode)
{
#ifndef __MINGW32__
  int fd;

  if ((fd = open (file, flags, mode)) < 0)
    {
      svz_log_sys_error ("open (%s)", file);
      return -1;
    }
  if (svz_fd_cloexec (fd) < 0)
    {
      close (fd);
      return -1;
    }
  add_file (fd);
  return fd;

#else /* __MINGW32__ */
  DWORD access = 0, creation = 0;
  svz_t_handle fd;

  /* drop this flag */
  flags &= ~O_BINARY;

  /* translate access */
  if (flags == O_RDONLY)
    access = GENERIC_READ;
  else if (flags & O_WRONLY)
    access = GENERIC_WRITE;
  else if (flags & O_RDWR)
    access = GENERIC_READ | GENERIC_WRITE;

  /* creation necessary?  */
  if (flags & O_CREAT)
    {
      creation |= CREATE_ALWAYS;
      if (flags & O_EXCL)
        creation |= CREATE_NEW;
    }
  else
    {
      creation |= OPEN_EXISTING;
      if (flags & O_TRUNC)
        creation |= TRUNCATE_EXISTING;
    }

  if (svz_invalid_handle_p (fd = CreateFile (file, access, 0, NULL,
                                             creation, 0, NULL)))
    {
      svz_log_sys_error ("CreateFile (%s)", file);
      return -1;
    }

  if (flags & O_APPEND)
    SetFilePointer (fd, 0, 0, FILE_END);
  return (int) fd;

#endif /* not __MINGW32__ */
}

/**
 * Close the given file handle @var{fd}.  Return -1 on errors.
 */
int
svz_close (int fd)
{
#ifndef __MINGW32__
  if (close (fd) < 0)
    {
      svz_log_sys_error ("close");
      return -1;
    }
  remove_file (fd);
#else /* __MINGW32__ */
  if (!CloseHandle ((svz_t_handle) fd))
    {
      svz_log_sys_error ("CloseHandle");
      return -1;
    }
#endif /* not __MINGW32__ */
  return 0;
}

/*
 * Conversion from FILETIME (100 nano-sec intervals from 1.1.1601) to
 * UTC time (seconds since 1.1.1970).
 */
#define DIFF_FT_LT                             \
  /* there have been 89 years with 366 days */ \
  ((((__int64) (1970 - 1601) * 365 + 89) * 24 * 3600) * 10000000L)

#define ft2lt(ft)                                                    \
  ((time_t) (((ft.dwLowDateTime | (__int64) ft.dwHighDateTime << 32) \
               - DIFF_FT_LT) / 10000000L))

/**
 * Return information about the specified file associated with the file
 * descriptor @var{fd} returned by @code{svz_open}.  Store available
 * information in the stat buffer @var{buf}.
 */
int
svz_fstat (int fd, struct stat *buf)
{
#ifndef __MINGW32__
  if (fstat (fd, buf) < 0)
    {
      svz_log_sys_error ("fstat");
      return -1;
    }
#else /* __MINGW32__ */
  BY_HANDLE_FILE_INFORMATION info;

  if (buf == NULL)
    return -1;

  if (!GetFileInformationByHandle ((svz_t_handle) fd, &info))
    {
      svz_log_sys_error ("GetFileInformationByHandle");
      return -1;
    }

  buf->st_mode = 0;
  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    buf->st_mode |= (S_IFDIR | _S_IREAD | _S_IWRITE | _S_IEXEC);
  else if (!(info.dwFileAttributes & FILE_ATTRIBUTE_OFFLINE))
    buf->st_mode |= (S_IFREG | _S_IREAD | _S_IWRITE | _S_IEXEC);

  buf->st_dev = info.dwVolumeSerialNumber;
  buf->st_ino = (short) info.nFileIndexLow;
  buf->st_nlink = (short) info.nNumberOfLinks;
  buf->st_uid = 0;
  buf->st_gid = 0;
  buf->st_rdev = 0;
  buf->st_size = (off_t) (((__int64) info.nFileSizeHigh << 32) |
                          info.nFileSizeLow);
  buf->st_atime = ft2lt (info.ftLastAccessTime);
  buf->st_mtime = ft2lt (info.ftLastWriteTime);
  buf->st_ctime = ft2lt (info.ftCreationTime);
#endif  /* not __MINGW32__ */
  return 0;
}

/**
 * Open the file whose name is the string pointed to by @var{file} and
 * associate a stream with it.
 */
FILE *
svz_fopen (const char *file, const char *mode)
{
  FILE *f;

  if ((f = fopen (file, mode)) == NULL)
    {
      svz_log_sys_error ("fopen (%s)", file);
      return NULL;
    }
#ifndef __MINGW32__
  if (svz_fd_cloexec (fileno (f)) < 0)
    {
      fclose (f);
      return NULL;
    }
  add_file (fileno (f));
#endif /* __MINGW32__ */
  return f;
}

/**
 * Dissociate the named stream @var{f} from its underlying file.
 */
int
svz_fclose (FILE *f)
{
#ifndef __MINGW32__
  remove_file (fileno (f));
#endif
  if (fclose (f) < 0)
    {
      svz_log_sys_error ("fclose");
      return -1;
    }
  return 0;
}
