dnl
dnl Autoconf macros for configuring the Serveez package.
dnl
dnl AC_SERVEEZ([USEFLAG]) -- Locate the Serveez core library.
dnl When USEFLAG is given (optional argument) the variables SERVEEZ_CFLAGS
dnl and SERVEEZ_LDFLAGS are set. Otherwise the macro adds these flags to
dnl the overall linker and compiler flags produced by the ./configure script.
dnl

AC_DEFUN([AC_SERVEEZ], [
  AC_ARG_WITH([serveez],
    [  --with-serveez=DIR      serveez installation in DIR @<:@/usr/local@:>@],
    [case "$withval" in
     no)  SVZDIR="no" ;;
     yes) SVZDIR="/usr/local" ;;
     *)   SVZDIR="$withval" ;;
    esac],
    SVZDIR="/usr/local")

  AC_MSG_CHECKING([for serveez installation])
  if test "x$SVZDIR" != "xno" ; then
    SVZDIR="`eval cd "$SVZDIR" 2>/dev/null && pwd`"
    case $build_os in
    mingw*)
	SVZDIR="`eval cygpath -w -i "$SVZDIR"`"
	SVZDIR="`echo "$SVZDIR" | sed -e 's%\\\\%/%g'`"
	;;
    esac
    if test -f "$SVZDIR/lib/libserveez.so" -o \
	    -f "$SVZDIR/lib/libserveez.dylib" -o \
	    -f "$SVZDIR/bin/libserveez.dll" -o \
	    -f "$SVZDIR/bin/cygserveez.dll" \
	    ; then
      if test "x$1" = "x" ; then
        CFLAGS="$CFLAGS -I$SVZDIR/include"
        LDFLAGS="$LDFLAGS -L$SVZDIR/lib"
        LIBS="$LIBS -lserveez"
        if test "x$CYGWIN" = "xyes" -o "x$MINGW32" = "xyes" ; then
          if test "x$enable_shared" = "xyes" ; then
	    CFLAGS="$CFLAGS -D__SERVEEZ_IMPORT__"
    	  fi
	fi
      else
        SERVEEZ_CFLAGS="-I$SVZDIR/include"
        SERVEEZ_LDFLAGS="-L$SVZDIR/lib -lserveez"
        if test "x$CYGWIN" = "xyes" -o "x$MINGW32" = "xyes" ; then
          if test "x$enable_shared" = "xyes" ; then
	    SERVEEZ_CFLAGS="$SERVEEZ_CFLAGS -D__SERVEEZ_IMPORT__"
    	  fi
	fi
        AC_SUBST(SERVEEZ_CFLAGS)
        AC_SUBST(SERVEEZ_LDFLAGS)
      fi
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([missing])
    fi
  else
    AC_MSG_RESULT([disabled])
  fi
  unset SVZDIR
])
