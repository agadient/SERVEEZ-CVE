## Process this file with automake to produce Makefile.in
#
# src/http-server/Makefile.am
#
# Small and fast minimal webserver.
#
# Copyright (C) 2011-2013 Thien-Thi Nguyen
# Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
# Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
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

noinst_LIBRARIES = libhttp.a

libhttp_a_SOURCES = \
	http-cache.c http-cache.h \
	http-cgi.c http-cgi.h \
	http-dirlist.c http-dirlist.h \
	http-proto.c http-proto.h \
	http-core.c http-core.h
