dnl configure.ac --- food for -*-autoconf-*- et al
dnl
dnl Copyright (C) 2011-2013 Thien-Thi Nguyen
dnl Copyright (C) 2000, 2001, 2002, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
dnl Copyright (C) 2000, 2001 Raimund Jacob <raimi@lkcc.org>
dnl Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
dnl
dnl This is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.
dnl
dnl This software is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this package.  If not, see <http://www.gnu.org/licenses/>.
dnl

dnl ----------------------------------------------------------------------
dnl init

AC_PREREQ([2.64])
AC_INIT([GNU Serveez],[0.2.2],[bug-serveez@gnu.org])
AC_CONFIG_HEADERS([src/config.h src/svzconfig.h src/guile-missing.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([1.12.2 no-define no-dist-gzip dist-lzip dist-xz serial-tests])
AM_MAINTAINER_MODE

dnl ----------------------------------------------------------------------
dnl information on the package

PROGRAM_VERSION='1.4'
## 1.3  -- new option: --solitary
## 1.2  -- new option: --daemon
## 1.1  -- drop option: --port PORT
## 1.0  -- initial revision
AC_SUBST([PROGRAM_VERSION])
AC_DEFINE_UNQUOTED([PROGRAM_VERSION],["$PROGRAM_VERSION"],
  [Version of serveez(1).])

LIBSERVEEZ_INTERFACE='2:0:1'
## 2:0:1       -- new typedef: svz_sock_prefree_fn
##                new func: svz_sock_prefree
## 1:0:0       -- initial
AC_SUBST([LIBSERVEEZ_INTERFACE])
AC_DEFINE_UNQUOTED([LIBSERVEEZ_INTERFACE],["$LIBSERVEEZ_INTERFACE"],
  [The triple CUR:REV:AGE describing the libserveez API.])

dnl
dnl Check whether debug output should be included.
dnl
SVZ_FLAG([whether to enable debug output],
         [yes],[debug],[Include debug output],[
AC_DEFINE([ENABLE_DEBUG], 1,
  [Define if debug output should be supported.])
])

dnl
dnl Check whether to include support for heap counters.
dnl
SVZ_FLAG([whether to enable heap counters],
         [yes],[heap-count],[Include support for heap counters],[
AC_DEFINE([ENABLE_HEAP_COUNT], 1,
  [Define if heap counters should be enabled.])
])

dnl
dnl Check whether we try to list local interfaces.
dnl
SVZ_FLAG([whether to enable list of local interfaces],
         [yes],[iflist],[List local interfaces],[
AC_DEFINE([ENABLE_IFLIST], 1,
  [Define if we are able to list local network interfaces.])
])

dnl
dnl Check whether poll loop should be supported.
dnl
SVZ_FLAG([whether to enable poll loop],
         [yes],[poll],[Include poll(2) server loop],[
AC_DEFINE([ENABLE_POLL], 1,
  [Define if poll(2) should be supported if possible.])
])

dnl
dnl Check whether ‘sendfile’ should be supported.
dnl
SVZ_FLAG([whether to use sendfile(2) if possible],
         [yes],[sendfile],[Include sendfile(2) support],[
AC_DEFINE([ENABLE_SENDFILE], 1,
  [Define if sendfile(2) should be supported if possible.])
])

dnl
dnl Check whether the SNTP server should be included.
dnl
SVZ_FLAG([whether to enable the SNTP server],
         [yes],[sntp-proto],[Include the SNTP server],[
AC_DEFINE([ENABLE_SNTP_PROTO], 1,
  [Define if the SNTP server should be included.])
])
AM_CONDITIONAL([SNTP],[SVZ_Y([enable_sntp_proto])])

dnl
dnl Check whether the Gnutella server should be included.
dnl
SVZ_FLAG([whether to enable the Gnutella spider],
         [yes],[gnutella],[Include the Gnutella spider],[
AC_DEFINE([ENABLE_GNUTELLA], 1,
  [Define if the Gnutella spider should be included.])
])
AM_CONDITIONAL([GNUTELLA],[SVZ_Y([enable_gnutella])])

dnl
dnl Check whether the Tunnel server should be included.
dnl
SVZ_FLAG([whether to enable the port forwarder],
         [yes],[tunnel],[Include port forwarder],[
AC_DEFINE([ENABLE_TUNNEL], 1,
  [Define if the port forwarder should be included.])
])
AM_CONDITIONAL([TUNNEL],[SVZ_Y([enable_tunnel])])

dnl
dnl Check whether the control protocol should be included.
dnl
SVZ_FLAG([whether to enable support for the control protocol],
         [yes],[control-proto],[Include the control protocol],[
AC_DEFINE([ENABLE_CONTROL_PROTO], 1,
  [Define if the control protocol should be supported.])
])
AM_CONDITIONAL([CONTROL],[SVZ_Y([enable_control_proto])])

dnl
dnl Check whether the IRC protocol should be included.
dnl
SVZ_FLAG([whether to enable support for the IRC protocol],
         [yes],[irc-proto],[Include the IRC protocol],[
AC_DEFINE([ENABLE_IRC_PROTO], 1,
  [Define if the IRC protocol should be supported.])
])
AM_CONDITIONAL([IRC],[SVZ_Y([enable_irc_proto])])

dnl
dnl Check whether the IRC protocol "TimeStamp" extension should be included.
dnl
SVZ_FLAG([whether to enable TimeStamp for the IRC protocol],
         [yes],[irc-ts],[Include TimeStamp in IRC protocol],[
AC_DEFINE([ENABLE_TIMESTAMP], 1,
  [Define if the IRC protocol TimeStamp extension should be supported.])
])

dnl
dnl Check whether the password encryption should be included.
dnl
SVZ_FLAG([whether to enable password encryption],
         [no],[crypt],[Include password encryption])

dnl
dnl Check whether the HTTP protocol should be included.
dnl
SVZ_FLAG([whether to enable support for the HTTP protocol],
         [yes],[http-proto],[Include the HTTP protocol],[
AC_DEFINE([ENABLE_HTTP_PROTO], 1,
  [Define if the HTTP protocol should be supported.])
])
AM_CONDITIONAL([HTTP],[SVZ_Y([enable_http_proto])])

dnl
dnl Check whether the Fake Ident Server should be included.
dnl
SVZ_FLAG([whether to enable the fake ident server],
         [yes],[fakeident],[Include the fake ident server],[
AC_DEFINE([ENABLE_FAKEIDENT], 1,
  [Define if the Fake Ident Server should be included.])
])
AM_CONDITIONAL([FAKEIDENT],[SVZ_Y([enable_fakeident])])

dnl
dnl Check whether the passthrough server should be included.
dnl
SVZ_FLAG([whether to enable the passthrough server],
         [yes],[passthrough],[Include the passthrough server],[
AC_DEFINE([ENABLE_PROG_SERVER], 1,
  [Define if the program passthrough server should be included.])
])
AM_CONDITIONAL([PROG],[SVZ_Y([enable_passthrough])])

dnl
dnl Check whether the flood protection should be included.
dnl
SVZ_FLAG([whether to enable support for flood protection],
         [yes],[flood],[Include flood protection],[
AC_DEFINE([ENABLE_FLOOD_PROTECTION], 1,
  [Define if flood protection should be supported.])
])

dnl
dnl Check if guile servers should be included.
dnl
SVZ_FLAG([whether to include guile server support],
         [yes],[guile-server],[Include guile servers],[
AC_DEFINE([ENABLE_GUILE_SERVER], 1,
  [Define if guile servers should be supported.])
])

dnl
dnl Check for additional searchpath for MinGW32 headers and libraries.
dnl
SVZ_WITH([/usr/local/mingw],[mingw],[DIR],[MinGW32 files are in DIR],
         [no],[/usr/local/mingw],["$withval"])
MINGDIR="$with_mingw"

dnl
dnl Check if this is Win32 (CYGWIN/MINGW32).
dnl
AS_CASE([$host_os],
[*mingw32*],[MINGW32=yes CYGWIN=no],
[*cygwin*],[MINGW32=no CYGWIN=yes])
AM_CONDITIONAL([MINGW32],[SVZ_Y([MINGW32])]) dnl FIXME: What about ‘CYGWIN’?

dnl
dnl Check courage.
dnl
SVZ_FLAG([whether to install libserveez and headers],
         [no],[libserveez-install],[Install libserveez and headers],[
AC_DEFINE([INSTALL_LIBSERVEEZ_AND_HEADERS], 1,
  [Define if configure enabled installation of libserveez and headers.])
])
AM_CONDITIONAL([COURAGEOUS],[SVZ_Y([enable_libserveez_install])])

dnl ----------------------------------------------------------------------
dnl before doing any compilation
dnl (e.g., ‘AM_PROG_AR’ does ‘AC_COMPILE_IFELSE’, sigh)

AC_USE_SYSTEM_EXTENSIONS

dnl ----------------------------------------------------------------------
dnl checks for programs

AC_PROG_INSTALL
AC_PROG_GREP
AC_PROG_SED
AC_PROG_AWK
AM_PROG_AR
AC_CHECK_PROG([SOELIM],[soelim],[soelim],
  [\${top_builddir}/build-aux/fake-soelim])
AC_CHECK_PROG([WINDRES],[windres],[windres],[:])

dnl
dnl Check for a proper C compiler.
dnl
AC_PROG_CC_C99
AS_IF([test xno = x"$ac_cv_prog_cc_c99"],
[AC_MSG_ERROR([No C99 capable compiler found -- see README.])])

dnl
dnl Libtool.  FIXME: Is this the right place?
dnl
AC_MSG_NOTICE([beg: libtool stuff])
LT_PREREQ([2.4])
LT_INIT([win32-dll])dnl FIXME: Add ‘dlopen’.
AC_MSG_NOTICE([end: libtool stuff])

dnl
dnl Check Guile installation.
dnl
SVZ_GUILE_FLAGS

SVZ_CHECK_GUILE_MISSING([
 ntohs ntohl htons htonl
 inet-aton inet-ntoa
])

dnl ----------------------------------------------------------------------
dnl checks for libraries

dnl
dnl Check for codec libraries.
dnl
AC_CHECK_LIB([z],[zlibVersion])
AC_CHECK_LIB([bz2],[BZ2_bzCompress],,
[AC_CHECK_LIB([bz2],[bzCompress])])

AS_IF([SVZ_Y([ac_cv_lib_bz2_BZ2_bzCompress])],[
AC_DEFINE([HAVE_BZ2LIB_PREFIX], 1,
 [Define if you have a prefixed bz2 library (>= v1.0)])
])

dnl
dnl Check whether extra net libraries are required.
dnl
SVZ_LIBS_MAYBE([socketpair],[socket])

dnl
dnl Check for Solaris nsl and resolv library.
dnl
SVZ_LIBS_MAYBE([gethostbyaddr],[nsl])
SVZ_LIBS_MAYBE([inet_aton],[resolv])

dnl Solaris.
AC_CHECK_LIB([kstat],[kstat_open])

# woe

resource_FILES=""
dnl
dnl WARNING: Possible breakage!
dnl
dnl Each AC_CHECK_LIB used to explicitly append to shell var ‘EXTRALIBS’
dnl (growing left to right).  Now, by omitting the ACTION-IF-FOUND, they
dnl implicitly prepend to shell var ‘LIBS’ (growing right to left).
dnl To preserve the original ordering, we have reversed the order in
dnl each group of sequential AC_CHECK_LIB calls, as well as the order
dnl of the groups.  This introduces a change in ‘CFLAGS’ and ‘LDFLAGS’
dnl from the pov of ‘user32’, ‘shell32’ and ‘advapi32’, however:
dnl previously, they did not have ‘-mno-cygwin’ (etc); now, they do.
dnl
dnl Furthermore, these system-level libraries should probably find
dnl themselves at the end of ‘LIBS’.  To effect that, this woe32 block
dnl should probably be done earliest (before zlib).
dnl
dnl Summary: This (presumably functioning previously) code might now be
dnl broken.  We say "might" because the maintainers thoroughly avoid
dnl woe, so this change will go untested until someone complains.
dnl
AS_IF([SVZ_Y([CYGWIN]) || SVZ_Y([MINGW32])],[
  AC_DEFINE([FD_SETSIZE], 4096, [Make CygWin / MinGW32 use large FD sets.])
  AC_DEFINE([WIN32_LEAN_AND_MEAN], 1, [Define for faster code generation.])
  resource_FILES="serveez.rc"
  AS_IF([SVZ_Y([MINGW32])],[
    AC_CHECK_LIB([mswsock],[main])
    AC_CHECK_LIB([ws2_32],[main])
  ])
  AS_IF([test xno != x"$MINGDIR" && SVZ_NOT_Y([MINGW32])],[
    CFLAGS="$CFLAGS -mno-cygwin -I${MINGDIR}/include"
    LDFLAGS="$LDFLAGS -mno-cygwin -L${MINGDIR}/lib"
    AC_CHECK_LIB([mswsock],[main])
    AC_CHECK_LIB([ws2_32],[main])
    AC_MSG_RESULT([using MinGW32 headers and libraries in ${MINGDIR}])
  ])
  AC_CHECK_LIB([user32],[main])
  AC_CHECK_LIB([shell32],[main])
  AC_CHECK_LIB([advapi32],[main])
])
AC_SUBST([resource_FILES])

dnl ----------------------------------------------------------------------
dnl checks for header files

AC_HEADER_DIRENT

dnl There is no <wait.h> on AIX, but <sys/wait.h>.
AC_HEADER_SYS_WAIT

AC_CHECK_HEADERS_ONCE([wait.h])

AC_CHECK_HEADERS_ONCE([sys/utsname.h])

AC_CHECK_HEADERS_ONCE([
  netinet/in.h arpa/inet.h
  sys/time.h sys/poll.h pwd.h varargs.h
  getopt.h sys/sockio.h sys/resource.h sys/sendfile.h sys/uio.h
  ws2tcpip.h dirent.h sys/dirent.h direct.h dl.h dld.h grp.h
  mach-o/dyld.h zlib.h bzlib.h rpc/rpcent.h rpc/rpc.h rpc/pmap_clnt.h
  rpc/pmap_prot.h rpc/clnt_soc.h sys/ioctl.h pthread.h floss.h
])

AC_CHECK_HEADERS_ONCE([netinet/tcp.h])
AC_CHECK_HEADERS_ONCE([netdb.h])

dnl HP-UX.
AC_CHECK_HEADERS_ONCE([sys/param.h sys/pstat.h])
dnl IRIX.
AC_CHECK_HEADERS_ONCE([sys/sysget.h sys/sysinfo.h])

AS_IF([SVZ_Y([enable_crypt])],[AC_CHECK_HEADERS([crypt.h])])

AC_CHECK_HEADERS([guile/gh.h],,,[
#include <libguile.h>
])

dnl woe
AC_CHECK_HEADERS_ONCE([winsock2.h])
AC_CHECK_HEADERS([mswsock.h], [], [],[
#if HAVE_WINSOCK2_H
# include <winsock2.h>
#endif
])

dnl ----------------------------------------------------------------------
dnl checks for types

AC_TYPE_SIZE_T
AC_TYPE_OFF_T
AC_TYPE_MODE_T
AC_TYPE_UID_T

dnl
dnl Check for ‘scm_t_bits’ or ‘scm_bits_t’ (sigh).
dnl
AC_CHECK_TYPE([scm_t_bits],[smobtag=scm_t_bits],[],[[
#include <libguile.h>
]])
AS_IF([SVZ_NOT_Y([ac_cv_type_scm_t_bits])],
[AC_CHECK_TYPE([scm_bits_t],[smobtag=scm_bits_t],[smobtag=long],[[
#include <libguile.h>
]])])
AC_DEFINE_UNQUOTED([svz_smob_tag_t],[$smobtag],
  [Define to the type of a smob tag.])
AS_UNSET([smobtag])

dnl
dnl Check for typedef socklen_t in <sys/socket.h>.
dnl
AC_CHECK_TYPE([socklen_t], [],
 [AC_DEFINE([socklen_t], int,
   [Define to 'int' if <sys/socket.h> does not define.])],
 [[#include <sys/socket.h>]])

dnl
dnl Check for the typedef ‘SOCKET’ and ‘HANDLE’ in <winsock2.h>.
dnl
socket_defined="int"
handle_defined="int"

AS_IF([test no != "$MINGDIR"],
[AS_IF([SVZ_Y([ac_cv_header_winsock2_h])],[
  socket_defined="SOCKET"
  handle_defined="HANDLE"
  AC_DEFINE([Win32_Winsock], 1,
   [Define if you are using Windows Socket-API (not CYGWIN).])])])

AC_DEFINE_UNQUOTED([svz_t_socket],[$socket_defined],
  [Define to 'int' if <winsock2.h> does not define 'SOCKET'.])

AC_DEFINE_UNQUOTED([svz_t_handle],[$handle_defined],
  [Define to 'int' if <winsock2.h> does not define 'HANDLE'.])

AS_UNSET([socket_defined])
AS_UNSET([handle_defined])

dnl ----------------------------------------------------------------------
dnl checks for structures

dnl ----------------------------------------------------------------------
dnl checks for compiler characteristics

AC_C_BIGENDIAN

AC_CHECK_SIZEOF([long], 4)
AC_CHECK_SIZEOF([void *], 4)
AS_IF([test $ac_cv_sizeof_long -lt $ac_cv_sizeof_void_p],
[AC_MSG_ERROR([A 'void *' value does not fit into a 'long' value.])])

dnl
dnl Check for "hidden visibility" attribute support in GCC.
dnl
AS_IF([SVZ_Y([GCC])],
[AC_CACHE_CHECK([for "hidden visibility" attribute in GCC],
  [svz_cv_gcc_hvis],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
    [[__attribute__ ((__visibility__ ("hidden"))) int i;]])],
      [svz_cv_gcc_hvis=yes],
      [svz_cv_gcc_hvis=no])])])
AS_IF([SVZ_Y([svz_cv_gcc_hvis])],
[AC_DEFINE([GCC_HAS_ATTRIBUTE_VISIBILITY],[1],
  [Define if gcc supports "hidden visibility" attribute.])])

dnl
dnl Create additional CFLAGS for MinGW32 and CygWin.
dnl
SERVEEZ_CFLAGS=""
AS_IF([SVZ_Y([CYGWIN]) || SVZ_Y([MINGW32])],
[AS_IF([SVZ_Y([enable_shared])],
  [SERVEEZ_CFLAGS="-D__SERVEEZ_IMPORT__"])])
AC_SUBST([SERVEEZ_CFLAGS])

dnl ----------------------------------------------------------------------
dnl checks for library functions

AS_IF([SVZ_Y([ac_cv_header_sys_time_h])],
[AC_CHECK_DECLS([gettimeofday],[],[],[[
#include <sys/time.h>
]])])

AC_CHECK_FUNCS([inet_pton])
AC_CHECK_FUNCS([fwrite_unlocked])

AC_CHECK_FUNCS([mkfifo mknod sendfile])
AC_CHECK_FUNCS([times poll waitpid])
AC_CHECK_FUNCS([uname])

AC_CHECK_FUNCS([getrlimit getdtablesize getpwnam seteuid setegid geteuid \
  getegid shl_load NSAddImage])

dnl
dnl Check for ‘crypt’ function.
dnl
AS_IF([SVZ_Y([enable_crypt])],
[SVZ_LIBS_MAYBE([crypt],[descrypt crypt])])
AS_IF([test xno = x"$ac_cv_search_crypt"],
[AC_MSG_ERROR([--enable-crypt specified but crypt(3) not available
               (i.e., not in: libc, libdescrypt, libcrypt)])])

dnl MacOS.
AC_CHECK_FUNCS([host_statistics])

dnl
dnl Check for ‘getopt’ and ‘getopt_long’ functions and headers.
dnl
SVZ_LIBS_MAYBE([getopt_long],[iberty])
AS_IF([test no = "$ac_cv_search_getopt_long"],
[SVZ_LIBS_MAYBE([getopt],[iberty])])

dnl
dnl We might declare getopt_long ourselves if the headers do not.
dnl
AS_IF([test xno != x"$ac_cv_search_getopt_long"],[
  AC_CHECK_DECLS([getopt_long], [], [], [[#include <getopt.h>]])])

dnl
dnl Check for ‘timezone’ and ‘daylight’ variables
dnl (FreeBSD and NetBSD do not provide).
dnl
AC_CHECK_DECLS([timezone, daylight],,,[[
#include <time.h>
]])

dnl
dnl Check if ‘alphasort’ is declared.  Also check for ‘alphasort’,
dnl ‘strsignal’ and ‘scandir’ in -liberty.
dnl
AC_CHECK_FUNCS([alphasort scandir strsignal])

AS_IF([SVZ_Y([ac_cv_header_dirent_h])],
[AC_CHECK_DECLS([alphasort], [], [], [[#include <dirent.h>]])])

dnl
dnl Check for shared library loading interface.
dnl FIXME: Abrogate; libtool to the rescue!
dnl
SVZ_LIBS_MAYBE([dlopen],[dl svld])
AS_IF([test no = "$ac_cv_search_dlopen"],
[SVZ_LIBS_MAYBE([dld_link],[dld])])

dnl
dnl Check for thread libraries.
dnl
threadsp=false
AC_DEFUN([SVZ_SET_THREADSP],[AS_IF([$1],[threadsp=true])])
AS_IF([SVZ_NOT_Y([ac_cv_func_fwrite_unlocked])],[
  SVZ_SET_THREADSP([SVZ_Y([MINGW32])])
  # libpthread for POSIX, libc_r for FreeBSD
  AS_IF([! $threadsp],[SVZ_LIBS_MAYBE([pthread_create],[pthread c_r])])
  SVZ_SET_THREADSP([test no != "$ac_cv_search_pthread_create"])
])
AS_IF([$threadsp],
[AC_DEFINE([ENABLE_LOG_MUTEX], 1,
  [Define to 1 if svz_log should use a mutex around its stdio calls.])])
AS_UNSET([threadsp])

dnl
dnl Check for ‘hstrerror’, ‘h_errno’ and ‘strsignal’ functions.
dnl
AC_CHECK_DECLS([strsignal], [], [], [[
    #include <string.h>
]])

SVZ_LIBS_MAYBE([hstrerror],[resolv])

AS_IF([test no != "$ac_cv_search_hstrerror"],
[AC_CHECK_DECLS([hstrerror, h_errno], [], [], [[#include <netdb.h>]])])

dnl
dnl Checks for RPC services.
dnl
SVZ_LIBS_MAYBE([getrpcent],[nsl])dnl libnsl for HP-UX.
AC_CHECK_FUNCS([getrpcbyname getrpcbynumber setrpcent endrpcent \
  pmap_set pmap_unset pmap_getmaps get_myaddress])

AC_CHECK_DECLS([setrpcent, endrpcent], [], [], [[
     #ifdef HAVE_NETDB_H
     # include <netdb.h>
     #endif
     #ifdef HAVE_RPC_RPCENT_H
     # include <rpc/rpcent.h>
     #endif
]])

dnl ----------------------------------------------------------------------
dnl checks for system services

dnl
dnl Check for the '/proc/stat' file under Linux.
dnl
AC_CACHE_CHECK([whether /proc/stat exists],[svz_cv_sys_proc_stat],
[AS_IF([grep 'cpu  ' /proc/stat >/dev/null 2>&1],
       [svz_cv_sys_proc_stat=yes],
       [svz_cv_sys_proc_stat=no])])

AS_IF([SVZ_Y([svz_cv_sys_proc_stat])],
[AS_CASE([$host_os],
 [linux*],[AC_DEFINE([HAVE_PROC_STAT], 1,
            [Define if Linux supports the /proc/stat file.])],
 [AC_MSG_WARN([disabled '/proc/stat' since '$host_os' is not 'linux'])])])

dnl ----------------------------------------------------------------------
dnl miscellaneous checks

AC_DEFINE_UNQUOTED([SIZEOF_UINT16],[2],[Number of bytes for uint16_t.])
AC_DEFINE_UNQUOTED([SIZEOF_UINT32],[4],[Number of bytes for uint32_t.])

dnl ----------------------------------------------------------------------
dnl write it out

dnl These two are here because their components are computed in two
dnl sections: "checks for libraries" and "checks for header files".
AM_CONDITIONAL([WITH_BZLIB],[SVZ_Y([ac_cv_header_bzlib_h]) &&
                             SVZ_Y([ac_cv_lib_bz2_BZ2_bzCompress]) ||
                             SVZ_Y([ac_cv_lib_bz2_bzCompress])])

AM_CONDITIONAL([WITH_ZLIB],[SVZ_Y([ac_cv_header_zlib_h]) &&
                            SVZ_Y([ac_cv_lib_z_zlibVersion])])

dnl
dnl Setup configuration variables.
dnl
SERVEEZ_CONFIG_CFLAGS="-I$prefix/include"
AS_IF([test x != "x$SERVEEZ_CFLAGS"],
[SERVEEZ_CONFIG_CFLAGS="$SERVEEZ_CONFIG_CFLAGS $SERVEEZ_CFLAGS"])
AS_IF([SVZ_Y([enable_shared])],
[SERVEEZ_CONFIG_LDFLAGS="-L$prefix/lib -lserveez"],
[SERVEEZ_CONFIG_LDFLAGS="$prefix/lib/libserveez.a"])
AC_SUBST([SERVEEZ_CONFIG_CFLAGS])
AC_SUBST([SERVEEZ_CONFIG_LDFLAGS])

dnl
dnl Output files.
dnl
AC_CONFIG_FILES([build-aux/fake-soelim],[chmod +x build-aux/fake-soelim])
AC_CONFIG_FILES([
    Makefile src/Makefile src/libserveez/Makefile
    src/libserveez/coserver/Makefile
    src/libserveez/codec/Makefile
    src/serveez-config
    src/ctrl-server/Makefile
    src/http-server/Makefile
    src/irc-core/Makefile
    src/irc-server/Makefile
    src/foo-server/Makefile
    src/sntp-server/Makefile
    src/nut-server/Makefile
    src/tunnel-server/Makefile
    src/fakeident-server/Makefile
    src/prog-server/Makefile
    src/guile-server/Makefile
    doc/Makefile
    test/Makefile
])

AC_OUTPUT

AC_MSG_NOTICE([$PACKAGE_STRING successfully configured -- cool!])
AS_IF([grep "^$host" $srcdir/data/PLATFORMS 1>/dev/null 2>&1],[:],
[AC_MSG_NOTICE([Welcome!
  $PACKAGE_NAME has never been compiled or run on this platform.
  Please send an email to <$PACKAGE_BUGREPORT> including
  your compiler version and whether it worked correctly or not.
  '$host' will then be added to the file data/PLATFORMS.])])

dnl ----------------------------------------------------------------------
dnl configure.ac ends here
