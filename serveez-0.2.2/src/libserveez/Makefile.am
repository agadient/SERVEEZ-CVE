## Process this file with automake to produce Makefile.in
#
# src/libserveez/Makefile.am
#
# Automake input file for the serveez core library.
#
# Copyright (C) 2011-2013 Thien-Thi Nguyen
# Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

EXTRA_DIST = mutex.h

SUBDIRS = coserver codec

# the serveez core library rules

BUILT_SOURCES = svzpath.h

if COURAGEOUS
lib_LTLIBRARIES = libserveez.la
else !COURAGEOUS
noinst_LTLIBRARIES = libserveez.la
endif !COURAGEOUS

libserveez_la_LDFLAGS = -export-dynamic -version-info $(LIBSERVEEZ_INTERFACE)
libserveez_la_LIBADD = coserver/libcoserver.la codec/libcodec.la

libserveez_la_SOURCES = address.c alloc.c util.c hash.c                   \
  tcp-socket.c pipe-socket.c udp-socket.c icmp-socket.c raw-socket.c      \
  server-core.c server-loop.c boot.c server.c server-socket.c             \
  interface.c dynload.c core.c socket.c array.c portcfg.c                 \
  binding.c passthrough.c cfg.c mutex.c

# internal
libserveez_la_SOURCES += soprop.c
EXTRA_DIST            += soprop.h

if MINGW32
libserveez_la_SOURCES += windoze.c
endif MINGW32

svzpath.h: $(top_builddir)/config.status
	@echo "Generating svzpath.h..."
	@echo '#define SVZ_PKGDATADIR "$(pkgdatadir)"' > $@
	@echo '#define SVZ_PKGLIBDIR "$(pkglibdir)"' >> $@

noinst_HEADERS = ipdata.h iphlpapi.h raw-socket.h server-socket.h

CLEANFILES += svzpath.h

if MAINTAINER_MODE
noinst_DATA = libserveez.tsar
endif

docable = \
  address.h array.h boot.h cfg.h \
  codec/codec.c \
  coserver/coserver.c \
  $(libserveez_la_SOURCES)

if !MINGW32
docable += windoze.c
EXTRA_DIST += windoze.h
endif !MINGW32

libserveez.tsar: $(docable)
	$(gx) s3as -o $@ $^
