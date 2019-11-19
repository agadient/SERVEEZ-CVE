/*
 * util.c - utility function implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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

#ifdef _AIX
# undef _NO_PROTO
# ifndef _USE_IRS
#  define _USE_IRS 1
# endif
# define _XOPEN_SOURCE_EXTENDED 1
#endif /* _AIX */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include <assert.h>

#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#if HAVE_SYS_RESOURCE_H && !defined (__MINGW32__)
# include <sys/resource.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_STRINGS_H
# include <strings.h>
#endif

#if HAVE_SYS_UTSNAME_H
# include <sys/utsname.h>
#endif

#include "networking-headers.h"
#include "unused.h"
#include "libserveez/alloc.h"
#include "libserveez/boot.h"
#include "libserveez/windoze.h"
#ifdef ENABLE_LOG_MUTEX
# include "libserveez/mutex.h"
#endif
#include "libserveez/util.h"

#ifdef __MINGW32__
/* definitions for Win95..WinME */
#define MaxSocketKey       HKEY_LOCAL_MACHINE
#define MaxSocketSubKey    "System\\CurrentControlSet\\Services\\VxD\\MSTCP"
#define MaxSocketSubSubKey "MaxConnections"
#endif  /* defined __MINGW32__ */

/*
 * Level of the logging interfaces verbosity:
 * 0 - only fatal error messages
 * 1 - error messages
 * 2 - warnings
 * 3 - informational messages
 * 4 - debugging output
 * Levels always imply numerically lesser levels.
 */

static char log_level[][16] = {
  "fatal",
  "error",
  "warning",
  "notice",
  "debug"
};

/*
 * This is the file all log messages are written to.  Change it with a
 * call to @code{svz_log_setfile}.  By default, all log messages are written
 * to @code{stderr}.
 */
static FILE *logfile = NULL;

/* The logging mutex is necessary only if stdio doesn't do locking.  */
#ifdef ENABLE_LOG_MUTEX

static svz_mutex_t spew_mutex = SVZ_MUTEX_INITIALIZER;
static int spew_mutex_valid;

#define LOCK_LOG_MUTEX() \
  if (spew_mutex_valid) svz_mutex_lock (&spew_mutex)
#define UNLOCK_LOG_MUTEX() \
  if (spew_mutex_valid) svz_mutex_unlock (&spew_mutex)

#else  /* !ENABLE_LOG_MUTEX */

#define LOCK_LOG_MUTEX()
#define UNLOCK_LOG_MUTEX()

#endif  /* !ENABLE_LOG_MUTEX */

#define LOGBUFSIZE  512

#ifndef HAVE_FWRITE_UNLOCKED
#define SVZ_UNUSED_IF_HAVE_FWRITE_UNLOCKED
#else
#define SVZ_UNUSED_IF_HAVE_FWRITE_UNLOCKED  UNUSED
#endif

void
svz__log_updn (SVZ_UNUSED_IF_HAVE_FWRITE_UNLOCKED int direction)
{
#ifndef HAVE_FWRITE_UNLOCKED
#endif
}

/**
 * Print a message to the log system.  @var{level} specifies the prefix.
 */
void
svz_log (int level, const char *format, ...)
{
  char buf[LOGBUFSIZE];
  size_t w = 0;
  va_list args;
  time_t tm;
  struct tm *t;

  if (level > SVZ_RUNPARM (VERBOSITY) || logfile == NULL ||
      feof (logfile) || ferror (logfile))
    return;

  tm = time (NULL);
  t = localtime (&tm);
  w = strftime (buf, LOGBUFSIZE, "[%Y/%m/%d %H:%M:%S]", t);
  w += snprintf (buf + w, LOGBUFSIZE - w, " %s: ", log_level[level]);
  va_start (args, format);
  w += vsnprintf (buf + w, LOGBUFSIZE - w, format, args);
  va_end (args);

  /* Ensure that an overlong message is properly truncated.  */
  if (LOGBUFSIZE > w)
    assert ('\0' == buf[w]);
  else
    {
      w = LOGBUFSIZE - 1;
      buf[w - 1] = '\n';
      buf[w] = '\0';
    }

  /* Write it out.  */
  LOCK_LOG_MUTEX ();
  fwrite (buf, 1, w, logfile);
  fflush (logfile);
  UNLOCK_LOG_MUTEX ();
}

/**
 * Set the file stream @var{file} to the log file all messages
 * are printed to.  Can also be @code{stdout} or @code{stderr}.
 */
void
svz_log_setfile (FILE * file)
{
  logfile = file;
}

int
svz_pton (const char *str, void *dst)
{
  int rv;

#if HAVE_INET_PTON

  rv = (1 == inet_pton (AF_INET, str, dst))
    ?  0
    : -1;

#elif HAVE_INET_ATON

  {
    rv = (1 == inet_aton (str, dst))
      ?  0
      : -1;
  }

#elif defined (__MINGW32__)

  {
    struct in_addr *a = dst;
    struct sockaddr_in addr;
    size_t len = sizeof (struct sockaddr_in);

    rv = (0 == WSAStringToAddress (str, AF_INET, NULL,
                                   (struct sockaddr *) addr, &len))
      ?  0
      : -1;
    if (0 == rv)
      *a = addr->sin_addr;
  }

#else

  {
    struct in_addr *a = dst;

    (*a).s_addr =  inet_addr (str);
    rv = 0;
  }

#endif

  return rv;
}

#define MAX_DUMP_LINE 16   /* bytes per line */

/**
 * Dump @var{buffer} with the length @var{len} to the file stream @var{out}.
 * Display description @var{action} along with origin and size info first,
 * followed by the hexadecimal text representation.
 * Stop output at either @var{max} or @var{len} (if @var{max} is zero) bytes.
 * @var{from} is a numerical identifier of the buffers creator.
 */
int
svz_hexdump (FILE *out, char *action, int from,
             char *buffer, int len, int max)
{
  int row, col, x, max_col;

  if (!max)
    max = len;
  if (max > len)
    max = len;
  max_col = max / MAX_DUMP_LINE;
  if ((max % MAX_DUMP_LINE) != 0)
    max_col++;

  fprintf (out, "%s [ FROM:0x%08X SIZE:%d ]\n", action, (unsigned) from, len);

  for (x = row = 0; row < max_col && x < max; row++)
    {
      /* print hexdump */
      fprintf (out, "%04X   ", x);
      for (col = 0; col < MAX_DUMP_LINE; col++, x++)
        {
          if (x < max)
            fprintf (out, "%02X ", (uint8_t) buffer[x]);
          else
            fprintf (out, "   ");
        }
      /* print character representation */
      x -= MAX_DUMP_LINE;
      fprintf (out, "  ");
      for (col = 0; col < MAX_DUMP_LINE && x < max; col++, x++)
        {
          fprintf (out, "%c", buffer[x] >= ' ' ? buffer[x] : '.');
        }
      fprintf (out, "\n");
    }

  fflush (out);
  return 0;
}

/**
 * Transform the given binary data @var{t} (UTC time) to an ASCII time text
 * representation without any trailing characters.
 */
char *
svz_time (long t)
{
  static char *asc;
  char *p;

  p = asc = ctime ((time_t *) &t);
  while (*p)
    p++;
  while (*p < ' ')
    *(p--) = '\0';

  return asc;
}

/**
 * Convert the given string @var{str} to lower case text representation.
 */
char *
svz_tolower (char *str)
{
  char *p = str;

  while (*p)
    {
      *p = (char) (isupper ((uint8_t) * p) ?
                   tolower ((uint8_t) * p) : *p);
      p++;
    }
  return str;
}

#ifdef __MINGW32__
/*
 * This variable contains the last system or network error occurred if
 * it was detected and printed.  Needed for the "Resource unavailable" error
 * condition.
 */
int svz_errno = 0;

#define MESSAGE_BUF_SIZE 256

/*
 * There is no text representation of network (Winsock API) errors in
 * Win32.  That is why we translate it by hand.
 */
static char *
neterror (int error)
{
  static char message[MESSAGE_BUF_SIZE];

  switch (error)
    {
    case WSAEACCES:
      return "Permission denied.";
    case WSAEADDRINUSE:
      return "Address already in use.";
    case WSAEADDRNOTAVAIL:
      return "Cannot assign requested address.";
    case WSAEAFNOSUPPORT:
      return "Address family not supported by protocol family.";
    case WSAEALREADY:
      return "Operation already in progress.";
    case WSAECONNABORTED:
      return "Software caused connection abort.";
    case WSAECONNREFUSED:
      return "Connection refused.";
    case WSAECONNRESET:
      return "Connection reset by peer.";
    case WSAEDESTADDRREQ:
      return "Destination address required.";
    case WSAEFAULT:
      return "Bad address.";
    case WSAEHOSTDOWN:
      return "Host is down.";
    case WSAEHOSTUNREACH:
      return "No route to host.";
    case WSAEINPROGRESS:
      return "Operation now in progress.";
    case WSAEINTR:
      return "Interrupted function call.";
    case WSAEINVAL:
      return "Invalid argument.";
    case WSAEISCONN:
      return "Socket is already connected.";
    case WSAEMFILE:
      return "Too many open files.";
    case WSAEMSGSIZE:
      return "Message too long.";
    case WSAENETDOWN:
      return "Network is down.";
    case WSAENETRESET:
      return "Network dropped connection on reset.";
    case WSAENETUNREACH:
      return "Network is unreachable.";
    case WSAENOBUFS:
      return "No buffer space available.";
    case WSAENOPROTOOPT:
      return "Bad protocol option.";
    case WSAENOTCONN:
      return "Socket is not connected.";
    case WSAENOTSOCK:
      return "Socket operation on non-socket.";
    case WSAEOPNOTSUPP:
      return "Operation not supported.";
    case WSAEPFNOSUPPORT:
      return "Protocol family not supported.";
    case WSAEPROCLIM:
      return "Too many processes.";
    case WSAEPROTONOSUPPORT:
      return "Protocol not supported.";
    case WSAEPROTOTYPE:
      return "Protocol wrong type for socket.";
    case WSAESHUTDOWN:
      return "Cannot send after socket shutdown.";
    case WSAESOCKTNOSUPPORT:
      return "Socket type not supported.";
    case WSAETIMEDOUT:
      return "Connection timed out.";
    case WSAEWOULDBLOCK:
      return "Resource temporarily unavailable.";
    case WSAHOST_NOT_FOUND:
      return "Host not found.";
    case WSANOTINITIALISED:
      return "Successful WSAStartup not yet performed.";
    case WSANO_DATA:
      return "Valid name, no data record of requested type.";
    case WSANO_RECOVERY:
      return "This is a non-recoverable error.";
    case WSASYSNOTREADY:
      return "Network subsystem is unavailable.";
    case WSATRY_AGAIN:
      return "Non-authoritative host not found.";
    case WSAVERNOTSUPPORTED:
      return "WINSOCK.DLL version out of range.";
    case WSAEDISCON:
      return "Graceful shutdown in progress.";
    default:
      sprintf (message, "Network error code %d.", error);
      break;
    }
  return message;
}

/*
 * Routine which forms a valid error message under Win32.  It might either
 * use the @code{GetLastError} or @code{WSAGetLastError} in order to
 * get a valid error code.
 */
static char *
syserror (int nr)
{
  static char message[MESSAGE_BUF_SIZE];

  /* save the last error */
  svz_errno = nr;

  /* return a net error if necessary */
  if (nr >= WSABASEERR)
    return neterror (nr);

  /*
   * if the error is not valid (GetLastError returned zero)
   * fall back to the errno variable of the usual crtdll.
   */
  if (!nr)
    nr = errno;

  /* return a sys error */
  if (0 == FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM |
                          FORMAT_MESSAGE_ARGUMENT_ARRAY, NULL, nr,
                          MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
                          (char *) message, MESSAGE_BUF_SIZE, NULL))
    {
      sprintf (message, "FormatMessage (%d): error code %ld",
               nr, GetLastError ());
      return message;
    }

  message[strlen (message) - 2] = 0;
  return message;
}

/*
 * This variable contains the the runtime detected Win32 version.  Its value
 * is setup in @code{svz_sys_version} and can be @code{Win32s} for Windows 3.x,
 * @code{Win95} for Windows 95, @code{Win98} for Windows 98, @code{WinNT3x}
 * for Windows NT 3.x, @code{WinNT4x} for Windows NT 4.x, @code{Win2k} for
 * Windows 2000, @code{WinXP} for Windows XP and @code{WinME} for Windows ME.
 */
static int os_version;
#define Win32s  0
#define Win95   1
#define Win98   2
#define WinNT3x 3
#define WinNT4x 4
#define Win2k   5
#define WinXP   6
#define WinME   7

#endif /* __MINGW32__ */

/**
 * Return a string describing the most recent system error.
 */
const char *
svz_sys_strerror (void)
{
#ifdef __MINGW32__
  return syserror (GetLastError ());
#else
  return strerror (errno);
#endif
}

/*
 * Return a string describing the most recent network error.
 */
const char *
svz_net_strerror (void)
{
#ifdef __MINGW32__
  return syserror (WSAGetLastError ());
#else
  return strerror (errno);
#endif
}

/**
 * Return 1 if there was a "socket unavailable" error recently, 0
 * otherwise.  This checks @code{svz_errno} against @code{WSAEWOULDBLOCK}
 * (woe32) or @code{EAGAIN} (Unix).
 */
int
svz_socket_unavailable_error_p (void)
{
#ifdef __MINGW32__
  return WSAEWOULDBLOCK == svz_errno;
#else
  return EAGAIN == svz_errno;
#endif
}

/**
 * Return a statically-allocated string describing some operating system
 * version details.
 */
char *
svz_sys_version (void)
{
  static char os[256] = ""; /* contains the os string */

#ifdef __MINGW32__
  static char ver[][6] =
    { " 32s", " 95", " 98", " NT", " NT", " 2000", " XP", " ME" };
  OSVERSIONINFO osver;
#elif HAVE_SYS_UTSNAME_H
  struct utsname buf;
#endif

  /* detect only once */
  if (os[0])
    return os;

#ifdef __MINGW32__ /* Windows */
  osver.dwOSVersionInfoSize = sizeof (osver);
  if (!GetVersionEx (&osver))
    {
      svz_log_sys_error ("GetVersionEx");
      sprintf (os, "unknown Windows");
    }
  else
    {
      switch (osver.dwPlatformId)
        {
        case VER_PLATFORM_WIN32_NT: /* NT, Windows 2000 or Windows XP */
          if (osver.dwMajorVersion == 4)
            os_version = WinNT4x;
          else if (osver.dwMajorVersion <= 3)
            os_version = WinNT3x;
          else if (osver.dwMajorVersion == 5 && osver.dwMinorVersion < 1)
            os_version = Win2k;
          else if (osver.dwMajorVersion >= 5)
            os_version = WinXP;
          break;

        case VER_PLATFORM_WIN32_WINDOWS: /* Win95 or Win98 */
          if ((osver.dwMajorVersion > 4) ||
              ((osver.dwMajorVersion == 4) && (osver.dwMinorVersion > 0)))
            {
              if (osver.dwMinorVersion >= 90)
                os_version = WinME;
              else
                os_version = Win98;
            }
          else
            os_version = Win95;
          break;

        case VER_PLATFORM_WIN32s: /* Windows 3.x */
          os_version = Win32s;
          break;
        }

      sprintf (os, "Windows%s %ld.%02ld %s%s(Build %ld)",
               ver[os_version],
               osver.dwMajorVersion, osver.dwMinorVersion,
               osver.szCSDVersion, osver.szCSDVersion[0] ? " " : "",
               osver.dwBuildNumber & 0xFFFF);
    }
#elif HAVE_UNAME /* !__MINGW32__ */
  uname (&buf);
  sprintf (os, "%s %s on %s", buf.sysname, buf.release, buf.machine);
#endif /* not HAVE_UNAME */

  return os;
}

/**
 * Return 1 if running MinGW (Windows) NT4x or later,
 * otherwise 0.
 */
int
svz_mingw_at_least_nt4_p (void)
{
#ifdef __MINGW32__
  return WinNT4x <= os_version;
#else
  return 0;
#endif
}

/**
 * Convert an unsigned integer to its decimal string representation,
 * returning a pointer to an internal buffer.  (You should copy the result.)
 */
char *
svz_itoa (unsigned int i)
{
  static char buffer[32];
  char *p = buffer + sizeof (buffer) - 1;

  *p = '\0';
  do
    {
      p--;
      *p = (char) ((i % 10) + '0');
    }
  while ((i /= 10) != 0);
  return p;
}

/**
 * Convert string @var{str} in decimal format to an unsigned integer.
 * Stop conversion on any invalid characters.
 */
unsigned int
svz_atoi (char *str)
{
  unsigned int i = 0;

  while (*str >= '0' && *str <= '9')
    {
      i *= 10;
      i += *str - '0';
      str++;
    }
  return i;
}

#ifdef __MINGW32__
# define getcwd(buf, size)  (GetCurrentDirectory (size, buf) ? buf : NULL)
#endif

/**
 * Return the current working directory in a newly allocated string.
 * (You should @code{svz_free} it when done.)
 */
char *
svz_getcwd (void)
{
  char *buf, *dir;
  int len = 64;

  buf = dir = NULL;
  do
    {
      buf = svz_realloc (buf, len);
      dir = getcwd (buf, len);
      len *= 2;
    }
  while (dir == NULL);

  return dir;
}

/**
 * Check for the current and maximum limit of open files of the
 * current process and try to set the limit to @var{max_sockets}.
 */
int
svz_openfiles (int max_sockets)
{
#if HAVE_GETRLIMIT
  struct rlimit rlim;
#endif

#if HAVE_GETDTABLESIZE
  int openfiles;

  if ((openfiles = getdtablesize ()) == -1)
    {
      svz_log_sys_error ("getdtablesize");
      return -1;
    }
  svz_log (SVZ_LOG_NOTICE, "file descriptor table size: %d\n", openfiles);
#endif /* HAVE_GETDTABLESIZE */

#if HAVE_GETRLIMIT

# ifndef RLIMIT_NOFILE
#  define RLIMIT_NOFILE RLIMIT_OFILE
# endif

  if (getrlimit (RLIMIT_NOFILE, &rlim) == -1)
    {
      svz_log_sys_error ("getrlimit");
      return -1;
    }
  svz_log (SVZ_LOG_NOTICE, "current open file limit: %d/%d\n",
           rlim.rlim_cur, rlim.rlim_max);

  if ((int) rlim.rlim_max < (int) max_sockets ||
      (int) rlim.rlim_cur < (int) max_sockets)
    {
      rlim.rlim_max = max_sockets;
      rlim.rlim_cur = max_sockets;

      if (setrlimit (RLIMIT_NOFILE, &rlim) == -1)
        {
          svz_log_sys_error ("setrlimit");
          return -1;
        }
      getrlimit (RLIMIT_NOFILE, &rlim);
      svz_log (SVZ_LOG_NOTICE, "open file limit set to: %d/%d\n",
               rlim.rlim_cur, rlim.rlim_max);
    }

#elif defined (__MINGW32__)     /* HAVE_GETRLIMIT */

  unsigned sockets = 100;

  if (os_version == Win95 ||
      os_version == Win98 || os_version == WinME)
    {
      if (os_version == Win95)
        sockets = svz_windoze_get_reg_unsigned (MaxSocketKey,
                                                MaxSocketSubKey,
                                                MaxSocketSubSubKey, sockets);
      else
        sockets = svz_atoi (svz_windoze_get_reg_string (MaxSocketKey,
                                                        MaxSocketSubKey,
                                                        MaxSocketSubSubKey,
                                                        svz_itoa (sockets)));

      svz_log (SVZ_LOG_NOTICE, "current open file limit: %u\n", sockets);

      if (sockets < (unsigned) max_sockets)
        {
          sockets = max_sockets;

          if (os_version == Win95)
            svz_windoze_set_reg_unsigned (MaxSocketKey,
                                          MaxSocketSubKey,
                                          MaxSocketSubSubKey, sockets);
          else
            svz_windoze_set_reg_string (MaxSocketKey,
                                        MaxSocketSubKey,
                                        MaxSocketSubSubKey,
                                        svz_itoa (sockets));

          svz_log (SVZ_LOG_NOTICE, "open file limit set to: %u\n", sockets);
        }
    }
#endif /* MINGW32__ */

  return 0;
}

#define PREFIX_SIZE  256                /* 255 + one for '\0' */
#define ERRMSG_SIZE  128

static void
save_errmsg (char *buf, char const *source)
{
  *buf = '\0';
  strncat (buf, source, ERRMSG_SIZE - 1);
}

static void
log_error (char const *prefix, char const *errmsg)
{
  svz_log (SVZ_LOG_ERROR, "%s: %s\n", prefix, errmsg);
}

#define LOG_ERROR_FROM(SOURCE)  do                      \
    {                                                   \
      char prefix[PREFIX_SIZE];                         \
      char errmsg[ERRMSG_SIZE];                         \
      va_list args;                                     \
                                                        \
      save_errmsg (errmsg, SOURCE);                     \
                                                        \
      va_start (args, fmt);                             \
      vsnprintf (prefix, PREFIX_SIZE, fmt, args);       \
      va_end (args);                                    \
                                                        \
      log_error (prefix, errmsg);                       \
    }                                                   \
  while (0)

/**
 * Log the current @dfn{system error}.
 */
void
svz_log_sys_error (char const *fmt, ...)
{
  LOG_ERROR_FROM (svz_sys_strerror ());
}

/**
 * Log the current @dfn{network error}.
 */
void
svz_log_net_error (char const *fmt, ...)
{
  LOG_ERROR_FROM (svz_net_strerror ());
}
