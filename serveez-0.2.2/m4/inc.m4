dnl inc.m4 --- some -*-autoconf-*- macros for configuring GNU Serveez
dnl
dnl Copyright (C) 2011-2013 Thien-Thi Nguyen
dnl Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

dnl ----------------------------------------------------------------------
dnl Common bits.

dnl SVZ_Y(VAR)
dnl
dnl Expand to:
dnl  test xyes = x"$VAR"
dnl
AC_DEFUN([SVZ_Y],[test xyes = x"$]$1["])

dnl SVZ_NOT_Y(VAR)
dnl
dnl Expand to:
dnl  test xyes != x"$VAR"
dnl
AC_DEFUN([SVZ_NOT_Y],[test xyes != x"$]$1["])

dnl SVZ_GOT_X(FUNC)
dnl
dnl Do ‘AC_DEFINE’ on FUNC (upcasing it first and prefixing "HAVE_").
dnl
AC_DEFUN([SVZ_GOT_X],[AC_DEFINE([HAVE_]m4_toupper([$1]), 1,
 [Define to 1 if you have the $1 function.])])

dnl SVZ_LIBS_MAYBE(FUNC,LIBRARIES)
dnl
dnl Do ‘AC_SEARCH_LIBS’ on FUNC and LIBRARIES, and
dnl (if successful) ‘SVZ_GOT_X’ on FUNC.
dnl
AC_DEFUN([SVZ_LIBS_MAYBE],[AC_SEARCH_LIBS([$1],[$2],[SVZ_GOT_X([$1])])])

dnl SVZ_HAVE_FUNC_MAYBE_IN_LIB(FUNC,VAR)
dnl
dnl First, do ‘SVZ_GOT_X’ on FUNC.  Next, if shell variable
dnl ac_cv_search_FUNC does not have value "none required", then
dnl append it to VAR.
dnl
AC_DEFUN([SVZ_HAVE_FUNC_MAYBE_IN_LIB],[
SVZ_GOT_X([$1])
AS_VAR_PUSHDEF([VAR],[ac_cv_search_$1])dnl
test 'xnone required' = x"$VAR" || $2="[$]$2 $VAR"
AS_VAR_POPDEF([VAR])dnl
])

dnl SVZ_HELP_STRING(LHS,DEFAULT,BLURB)
dnl
dnl Wrap ‘AS_HELP_STRING’, expanding the right-hand-side as:
dnl   BLURB [default=DEFAULT]
dnl
AC_DEFUN([SVZ_HELP_STRING],
[AS_HELP_STRING([$1],[$3 @<:@default=$2@:>@])])dnl

dnl SVZ_FLAG(MSG,DEFAULT,NICK,BLURB[,IF-YES[,IF-NO]])
dnl
dnl Say "checking MSG"; then check for --{en,dis}able-NICK;
dnl assigning var enable_NICK the value ‘yes’ or ‘no’, or DEFAULT
dnl if not specified on the configure script command-line.  BLURB
dnl is for --help output (see ‘SVZ_HELP_STRING’).  Report the
dnl result.  If specified, do IF-YES or IF-NO for values of ‘yes’
dnl and ‘no’, respectively.
dnl
AC_DEFUN([SVZ_FLAG],[dnl
AS_VAR_PUSHDEF([VAR],[enable_$3])dnl
AC_MSG_CHECKING([$1])
AC_ARG_ENABLE([$3],
  [SVZ_HELP_STRING([--]m4_case([$2],[yes],[dis],[en])[able-$3],
                   [$2],[$4])],
  [SVZ_Y(VAR) || VAR=no],
  [VAR=$2])
AC_MSG_RESULT([$VAR])
m4_ifnblank([$5$6],[AS_IF([SVZ_Y(VAR)],[$5],[$6])])
AS_VAR_POPDEF([VAR])dnl
])dnl

dnl SVZ_WITH(DEFAULT,NICK,ARGS,BLURB,IF-NO,IF-YES,OTHERWISE)
dnl
dnl Wrap ‘AC_ARG_WITH’, setting the shell variable with_NICK to
dnl IF-NO, IF-YES, or OTHERWISE (if specified on the configure
dnl script command-line); or DEFAULT (if unspecified).  ARGS and
dnl BLURB are for --help output (see ‘SVZ_HELP_STRING’).
dnl
AC_DEFUN([SVZ_WITH],[dnl
AS_VAR_PUSHDEF([VAR],[with_$2])dnl
AC_ARG_WITH([$2],[SVZ_HELP_STRING([--with-$2]m4_ifnblank([$3],[=$3]),
                                  [$1],[$4])],
  [AS_CASE(["$VAR"],
           [no],[VAR=$5],
           [yes],[VAR=$6],
           [VAR=$7])],
  [VAR=$1])
AS_VAR_POPDEF([VAR])dnl
])dnl

dnl SVZ_GUILE_FLAGS
dnl
dnl First, call ‘SNUGGLE_PROGS’ to define shell var ‘GUILE_CONFIG’.
dnl (If this fails, signal error and exit failurefully.)
dnl Use that program to determine -I, -L and -l options, appended
dnl respectively to shell vars ‘CPPFLAGS’, ‘LDFLAGS’, and ‘LIBS’.

AC_DEFUN([SVZ_GUILE_FLAGS],[
dnl Find guile(1), guile-config(1) and guile-tools(1).
SNUGGLE_PROGS
dnl Set ‘CPPFLAGS’ directly.
AS_VAR_APPEND([CPPFLAGS],[" `$GUILE_CONFIG compile`"])
dnl Split the ‘link’ output to set ‘LDFLAGS’ and ‘LIBS’.
guile_link_flags=`$GUILE_CONFIG link`
guile_ldflags=`echo $guile_link_flags | sed 's/ -l.*//'`
guile_squash=`echo $guile_ldflags | sed 's/././g'`
guile_libs=`echo $guile_link_flags | sed 's|^'${guile_squash}'||'`
AS_VAR_APPEND([LDFLAGS],[" $guile_ldflags"])
AS_VAR_APPEND([LIBS],["$guile_libs"])
dnl Clean up.
m4_foreach([var],[link_flags, ldflags, squash, libs],[
AS_UNSET([guile_]var)])
])dnl

dnl SVZ_GUILE_HAS_PROC(PROC)
dnl
dnl Check if Guile defineds procedure PROC.  If not, arrange to
dnl ‘AC_DEFINE’ a variable named GUILE_MISSING_FOO, where FOO
dnl is PROC, appropriately massaged for C syntax.
dnl
AC_DEFUN([SVZ_GUILE_HAS_PROC],[
AS_VAR_PUSHDEF([VAR],[svz_cv_guile_has_$1])dnl
AS_VAR_PUSHDEF([DEF],[GUILE_MISSING_$1])dnl
AC_CACHE_CHECK([if guile has procedure `$1'],[VAR],
[AS_IF([${GUILE-guile} -c "$1 (exit 0)" >/dev/null 2>&1],
       [AS_VAR_SET([VAR],[yes])],
       [AS_VAR_SET([VAR],[no])])])
AS_VAR_IF([VAR],[no],[AC_DEFINE_UNQUOTED([DEF], 1)])
AS_VAR_POPDEF([DEF])dnl
AS_VAR_POPDEF([VAR])dnl
])dnl

dnl SVZ_CHECK_GUILE_MISSING(PROCS)
dnl
dnl Do SVZ_GUILE_HAS_PROC for every proc in PROCS.
dnl
AC_DEFUN([SVZ_CHECK_GUILE_MISSING],[for proc in m4_normalize([$1])
do SVZ_GUILE_HAS_PROC([$proc])
done])dnl

dnl inc.m4 ends here
