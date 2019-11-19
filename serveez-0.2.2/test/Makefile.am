## Process this file with automake to produce Makefile.in
#
# test/Makefile.am
#
# Automake input file for the test directory.
#
# Copyright (C) 2011-2013 Thien-Thi Nguyen
# Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this package.  If not, see <http://www.gnu.org/licenses/>.
#

include $(top_srcdir)/build-aux/common.mk

# FIXME: Make ‘SVZ_GUILE’ (../configure.ac) set this.
GUILE = guile

TESTS_ENVIRONMENT =					\
	GUILE_LOAD_PATH=".:$(srcdir):$$GUILE_LOAD_PATH"	\
	$(GUILE) -s
XFAIL_TESTS =
TESTS = t000 t001 t002 t003 t004 t005 t006 t007 t008

CLEANFILES += *.log

EXTRA_DIST = common $(TESTS) \
	test-suite.scm

check_PROGRAMS = btdt
check_DATA = but-of-course

btdt_SOURCES = btdt.c

LDADD = ../src/libserveez/libserveez.la

but-of-course: ../src/config.h
	{ echo '(define (boc? symbol) (memq symbol (quote (' ; \
	  $(SED) '/#define \(.*\) 1/!d;s//\1/' $< ; \
	  echo '))))' ; } > $@
