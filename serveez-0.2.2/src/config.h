/* src/config.h.  Generated from config.h.in by configure.  */
/* src/config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define if the control protocol should be supported. */
#define ENABLE_CONTROL_PROTO 1

/* Define if debug output should be supported. */
#define ENABLE_DEBUG 1

/* Define if the Fake Ident Server should be included. */
#define ENABLE_FAKEIDENT 1

/* Define if flood protection should be supported. */
#define ENABLE_FLOOD_PROTECTION 1

/* Define if the Gnutella spider should be included. */
#define ENABLE_GNUTELLA 1

/* Define if guile servers should be supported. */
/* #undef ENABLE_GUILE_SERVER */

/* Define if heap counters should be enabled. */
#define ENABLE_HEAP_COUNT 1

/* Define if the HTTP protocol should be supported. */
#define ENABLE_HTTP_PROTO 1

/* Define if we are able to list local network interfaces. */
#define ENABLE_IFLIST 1

/* Define if the IRC protocol should be supported. */
#define ENABLE_IRC_PROTO 1

/* Define to 1 if svz_log should use a mutex around its stdio calls. */
/* #undef ENABLE_LOG_MUTEX */

/* Define if poll(2) should be supported if possible. */
#define ENABLE_POLL 1

/* Define if the program passthrough server should be included. */
#define ENABLE_PROG_SERVER 1

/* Define if sendfile(2) should be supported if possible. */
#define ENABLE_SENDFILE 1

/* Define if the SNTP server should be included. */
#define ENABLE_SNTP_PROTO 1

/* Define if the IRC protocol TimeStamp extension should be supported. */
#define ENABLE_TIMESTAMP 1

/* Define if the port forwarder should be included. */
#define ENABLE_TUNNEL 1

/* Make CygWin / MinGW32 use large FD sets. */
/* #undef FD_SETSIZE */

/* Define if gcc supports "hidden visibility" attribute. */
#define GCC_HAS_ATTRIBUTE_VISIBILITY 1

/* Define to 1 if you have the `alphasort' function. */
/* #undef HAVE_ALPHASORT */

/* Define to 1 if you have the <arpa/inet.h> header file. */
#define HAVE_ARPA_INET_H 1

/* Define if you have a prefixed bz2 library (>= v1.0) */
/* #undef HAVE_BZ2LIB_PREFIX */

/* Define to 1 if you have the <bzlib.h> header file. */
/* #undef HAVE_BZLIB_H */

/* Define to 1 if you have the crypt function. */
/* #undef HAVE_CRYPT */

/* Define to 1 if you have the <crypt.h> header file. */
/* #undef HAVE_CRYPT_H */

/* Define to 1 if you have the declaration of `alphasort', and to 0 if you
   don't. */
#define HAVE_DECL_ALPHASORT 1

/* Define to 1 if you have the declaration of `daylight', and to 0 if you
   don't. */
#define HAVE_DECL_DAYLIGHT 1

/* Define to 1 if you have the declaration of `endrpcent', and to 0 if you
   don't. */
#define HAVE_DECL_ENDRPCENT 1

/* Define to 1 if you have the declaration of `getopt_long', and to 0 if you
   don't. */
/* #undef HAVE_DECL_GETOPT_LONG */

/* Define to 1 if you have the declaration of `gettimeofday', and to 0 if you
   don't. */
#define HAVE_DECL_GETTIMEOFDAY 1

/* Define to 1 if you have the declaration of `hstrerror', and to 0 if you
   don't. */
/* #undef HAVE_DECL_HSTRERROR */

/* Define to 1 if you have the declaration of `h_errno', and to 0 if you
   don't. */
/* #undef HAVE_DECL_H_ERRNO */

/* Define to 1 if you have the declaration of `setrpcent', and to 0 if you
   don't. */
#define HAVE_DECL_SETRPCENT 1

/* Define to 1 if you have the declaration of `strsignal', and to 0 if you
   don't. */
#define HAVE_DECL_STRSIGNAL 1

/* Define to 1 if you have the declaration of `timezone', and to 0 if you
   don't. */
#define HAVE_DECL_TIMEZONE 1

/* Define to 1 if you have the <direct.h> header file. */
/* #undef HAVE_DIRECT_H */

/* Define to 1 if you have the <dirent.h> header file. */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the <dld.h> header file. */
/* #undef HAVE_DLD_H */

/* Define to 1 if you have the dld_link function. */
/* #undef HAVE_DLD_LINK */

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the dlopen function. */
/* #undef HAVE_DLOPEN */

/* Define to 1 if you have the <dl.h> header file. */
/* #undef HAVE_DL_H */

/* Define to 1 if you have the `endrpcent' function. */
/* #undef HAVE_ENDRPCENT */

/* Define to 1 if you have the <floss.h> header file. */
/* #undef HAVE_FLOSS_H */

/* Define to 1 if you have the `fwrite_unlocked' function. */
/* #undef HAVE_FWRITE_UNLOCKED */

/* Define to 1 if you have the `getdtablesize' function. */
/* #undef HAVE_GETDTABLESIZE */

/* Define to 1 if you have the `getegid' function. */
/* #undef HAVE_GETEGID */

/* Define to 1 if you have the `geteuid' function. */
/* #undef HAVE_GETEUID */

/* Define to 1 if you have the gethostbyaddr function. */
/* #undef HAVE_GETHOSTBYADDR */

/* Define to 1 if you have the getopt function. */
/* #undef HAVE_GETOPT */

/* Define to 1 if you have the <getopt.h> header file. */
#define HAVE_GETOPT_H 1

/* Define to 1 if you have the getopt_long function. */
/* #undef HAVE_GETOPT_LONG */

/* Define to 1 if you have the `getpwnam' function. */
/* #undef HAVE_GETPWNAM */

/* Define to 1 if you have the `getrlimit' function. */
/* #undef HAVE_GETRLIMIT */

/* Define to 1 if you have the `getrpcbyname' function. */
/* #undef HAVE_GETRPCBYNAME */

/* Define to 1 if you have the `getrpcbynumber' function. */
/* #undef HAVE_GETRPCBYNUMBER */

/* Define to 1 if you have the getrpcent function. */
/* #undef HAVE_GETRPCENT */

/* Define to 1 if you have the `get_myaddress' function. */
/* #undef HAVE_GET_MYADDRESS */

/* Define to 1 if you have the <grp.h> header file. */
#define HAVE_GRP_H 1

/* Define to 1 if you have the <guile/gh.h> header file. */
/* #undef HAVE_GUILE_GH_H */

/* Define to 1 if you have the `host_statistics' function. */
/* #undef HAVE_HOST_STATISTICS */

/* Define to 1 if you have the hstrerror function. */
/* #undef HAVE_HSTRERROR */

/* Define to 1 if you have the inet_aton function. */
/* #undef HAVE_INET_ATON */

/* Define to 1 if you have the `inet_pton' function. */
/* #undef HAVE_INET_PTON */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `advapi32' library (-ladvapi32). */
/* #undef HAVE_LIBADVAPI32 */

/* Define to 1 if you have the `bz2' library (-lbz2). */
/* #undef HAVE_LIBBZ2 */

/* Define to 1 if you have the `kstat' library (-lkstat). */
/* #undef HAVE_LIBKSTAT */

/* Define to 1 if you have the `mswsock' library (-lmswsock). */
/* #undef HAVE_LIBMSWSOCK */

/* Define to 1 if you have the `shell32' library (-lshell32). */
/* #undef HAVE_LIBSHELL32 */

/* Define to 1 if you have the `user32' library (-luser32). */
/* #undef HAVE_LIBUSER32 */

/* Define to 1 if you have the `ws2_32' library (-lws2_32). */
/* #undef HAVE_LIBWS2_32 */

/* Define to 1 if you have the `z' library (-lz). */
/* #undef HAVE_LIBZ */

/* Define to 1 if you have the <mach-o/dyld.h> header file. */
/* #undef HAVE_MACH_O_DYLD_H */

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mkfifo' function. */
/* #undef HAVE_MKFIFO */

/* Define to 1 if you have the `mknod' function. */
/* #undef HAVE_MKNOD */

/* Define to 1 if you have the <mswsock.h> header file. */
/* #undef HAVE_MSWSOCK_H */

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define to 1 if you have the <netdb.h> header file. */
#define HAVE_NETDB_H 1

/* Define to 1 if you have the <netinet/in.h> header file. */
#define HAVE_NETINET_IN_H 1

/* Define to 1 if you have the <netinet/tcp.h> header file. */
#define HAVE_NETINET_TCP_H 1

/* Define to 1 if you have the `NSAddImage' function. */
/* #undef HAVE_NSADDIMAGE */

/* Define to 1 if you have the `pmap_getmaps' function. */
/* #undef HAVE_PMAP_GETMAPS */

/* Define to 1 if you have the `pmap_set' function. */
/* #undef HAVE_PMAP_SET */

/* Define to 1 if you have the `pmap_unset' function. */
/* #undef HAVE_PMAP_UNSET */

/* Define to 1 if you have the `poll' function. */
/* #undef HAVE_POLL */

/* Define if Linux supports the /proc/stat file. */
#define HAVE_PROC_STAT 1

/* Define to 1 if you have the pthread_create function. */
/* #undef HAVE_PTHREAD_CREATE */

/* Define to 1 if you have the <pthread.h> header file. */
#define HAVE_PTHREAD_H 1

/* Define to 1 if you have the <pwd.h> header file. */
#define HAVE_PWD_H 1

/* Define to 1 if you have the <rpc/clnt_soc.h> header file. */
/* #undef HAVE_RPC_CLNT_SOC_H */

/* Define to 1 if you have the <rpc/pmap_clnt.h> header file. */
#define HAVE_RPC_PMAP_CLNT_H 1

/* Define to 1 if you have the <rpc/pmap_prot.h> header file. */
#define HAVE_RPC_PMAP_PROT_H 1

/* Define to 1 if you have the <rpc/rpcent.h> header file. */
/* #undef HAVE_RPC_RPCENT_H */

/* Define to 1 if you have the <rpc/rpc.h> header file. */
#define HAVE_RPC_RPC_H 1

/* Define to 1 if you have the `scandir' function. */
/* #undef HAVE_SCANDIR */

/* Define to 1 if you have the `sendfile' function. */
/* #undef HAVE_SENDFILE */

/* Define to 1 if you have the `setegid' function. */
/* #undef HAVE_SETEGID */

/* Define to 1 if you have the `seteuid' function. */
/* #undef HAVE_SETEUID */

/* Define to 1 if you have the `setrpcent' function. */
/* #undef HAVE_SETRPCENT */

/* Define to 1 if you have the `shl_load' function. */
/* #undef HAVE_SHL_LOAD */

/* Define to 1 if you have the socketpair function. */
/* #undef HAVE_SOCKETPAIR */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strsignal' function. */
/* #undef HAVE_STRSIGNAL */

/* Define to 1 if you have the <sys/dirent.h> header file. */
/* #undef HAVE_SYS_DIRENT_H */

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_DIR_H */

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_NDIR_H */

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/poll.h> header file. */
#define HAVE_SYS_POLL_H 1

/* Define to 1 if you have the <sys/pstat.h> header file. */
/* #undef HAVE_SYS_PSTAT_H */

/* Define to 1 if you have the <sys/resource.h> header file. */
#define HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/sendfile.h> header file. */
#define HAVE_SYS_SENDFILE_H 1

/* Define to 1 if you have the <sys/sockio.h> header file. */
/* #undef HAVE_SYS_SOCKIO_H */

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/sysget.h> header file. */
/* #undef HAVE_SYS_SYSGET_H */

/* Define to 1 if you have the <sys/sysinfo.h> header file. */
#define HAVE_SYS_SYSINFO_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/uio.h> header file. */
#define HAVE_SYS_UIO_H 1

/* Define to 1 if you have the <sys/utsname.h> header file. */
#define HAVE_SYS_UTSNAME_H 1

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#define HAVE_SYS_WAIT_H 1

/* Define to 1 if you have the `times' function. */
/* #undef HAVE_TIMES */

/* Define to 1 if you have the `uname' function. */
/* #undef HAVE_UNAME */

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the <varargs.h> header file. */
/* #undef HAVE_VARARGS_H */

/* Define to 1 if you have the `waitpid' function. */
/* #undef HAVE_WAITPID */

/* Define to 1 if you have the <wait.h> header file. */
#define HAVE_WAIT_H 1

/* Define to 1 if you have the <winsock2.h> header file. */
/* #undef HAVE_WINSOCK2_H */

/* Define to 1 if you have the <ws2tcpip.h> header file. */
/* #undef HAVE_WS2TCPIP_H */

/* Define to 1 if you have the <zlib.h> header file. */
#define HAVE_ZLIB_H 1

/* Define if configure enabled installation of libserveez and headers. */
/* #undef INSTALL_LIBSERVEEZ_AND_HEADERS */

/* The triple CUR:REV:AGE describing the libserveez API. */
#define LIBSERVEEZ_INTERFACE "2:0:1"

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "bug-serveez@gnu.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "GNU Serveez"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "GNU Serveez 0.2.2"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "serveez"

/* Define to the home page for this package. */
#define PACKAGE_URL "http://www.gnu.org/software/serveez/"

/* Define to the version of this package. */
#define PACKAGE_VERSION "0.2.2"

/* Version of serveez(1). */
#define PROGRAM_VERSION "1.4"

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 0

/* Number of bytes for uint16_t. */
#define SIZEOF_UINT16 2

/* Number of bytes for uint32_t. */
#define SIZEOF_UINT32 4

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 0

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Define for faster code generation. */
/* #undef WIN32_LEAN_AND_MEAN */

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Define if you are using Windows Socket-API (not CYGWIN). */
/* #undef Win32_Winsock */

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* Define to `int' if <sys/types.h> doesn't define. */
/* #undef gid_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef mode_t */

/* Define to `long int' if <sys/types.h> does not define. */
/* #undef off_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define to 'int' if <sys/socket.h> does not define. */
/* #undef socklen_t */

/* Define to the type of a smob tag. */
#define svz_smob_tag_t scm_t_bits

/* Define to 'int' if <winsock2.h> does not define 'HANDLE'. */
#define svz_t_handle int

/* Define to 'int' if <winsock2.h> does not define 'SOCKET'. */
#define svz_t_socket int

/* Define to `int' if <sys/types.h> doesn't define. */
/* #undef uid_t */
