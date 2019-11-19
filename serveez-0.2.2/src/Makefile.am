## Process this file with automake to produce Makefile.in
#
# src/Makefile.am
#
# Automake input file.
#
# Copyright (C) 2011-2013 Thien-Thi Nguyen
# Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
# Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
# Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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

if COURAGEOUS
#
# Installation of the autoconf macro used to locate Serveez
#
aclocaldir = $(datadir)/aclocal
aclocal_DATA = serveez.m4

# Yet another script
bin_SCRIPTS = serveez-config
endif COURAGEOUS

# Perplexingly, Automake does not automatically distribute these.
EXTRA_DIST = $(aclocal_DATA) guile-boot.scm

CLEANFILES += libserveez.h
BUILT_SOURCES = libserveez.h guile-boot.c
BUILT_SOURCES += \
  gsk.c \
  guile-bin.x \
  guile-server.x \
  guile.x

SUBDIRS = libserveez guile-server
PROTOLIBS =

SUBDIRS   += foo-server
PROTOLIBS += foo-server/libfoo.a

if SNTP
SUBDIRS   += sntp-server
PROTOLIBS += sntp-server/libsntp.a
endif
if GNUTELLA
SUBDIRS   += nut-server
PROTOLIBS += nut-server/libnut.a
endif
if TUNNEL
SUBDIRS   += tunnel-server
PROTOLIBS += tunnel-server/libtunnel.a
endif
if CONTROL
SUBDIRS   += ctrl-server
PROTOLIBS += ctrl-server/libctrl.a
endif
if IRC
SUBDIRS   += irc-server                irc-core
PROTOLIBS += irc-server/libircserver.a irc-core/libirccore.a
endif
if HTTP
SUBDIRS   += http-server
PROTOLIBS += http-server/libhttp.a
endif
if FAKEIDENT
SUBDIRS   += fakeident-server
PROTOLIBS += fakeident-server/libfakeident.a
endif
if PROG
SUBDIRS   += prog-server
PROTOLIBS += prog-server/libprog.a
endif

EXTRA_DIST += gi-sup
bin_PROGRAMS = serveez

if COURAGEOUS
include_HEADERS = libserveez.h
endif COURAGEOUS

# NB: Order is important here -- don't screw it up!
# See also the ‘EXTRA_DIST’ comment, below.
hbits = \
 libserveez/defines.h \
 libserveez/address.h \
 libserveez/boot.h \
 libserveez/alloc.h \
 libserveez/array.h \
 libserveez/hash.h \
 libserveez/util.h \
 libserveez/socket.h \
 libserveez/core.h \
 libserveez/pipe-socket.h \
 libserveez/portcfg.h \
 libserveez/cfg.h \
 libserveez/server.h \
 libserveez/binding.h \
 libserveez/tcp-socket.h \
 libserveez/udp-socket.h \
 libserveez/icmp-socket.h \
 libserveez/server-core.h

if MINGW32
hbits += \
 libserveez/windoze.h
endif MINGW32

hbits += \
 libserveez/coserver/coserver.h \
 libserveez/interface.h \
 libserveez/dynload.h \
 libserveez/passthrough.h \
 libserveez/codec/codec.h

libserveez.h: $(hbits) svzconfig.h
	for f in svzconfig.h $(hbits) ; do			\
	  part=`echo $$f | $(SED) 's|^[^/]*/||;s/.h$$//'` ;	\
	  printf '\f\n/* (%s) */' $$part ;			\
	  $(SED) -e '1,/^#ifndef/d'				\
	         -e '/_H__/d'					\
	         -e '/^#include ".*"/d'				\
	         -e '/ends here/d'				\
	         -e 's/ HAVE_/ SVZ_HAVE_/g'			\
	         $(srcdir)/$$f ;				\
	done							\
	| $(SED) -e '/begin svzint/,/end svzint/d'		\
	         -e '/^SBO.*;$$/d'				\
	         -e '/^SBO/,/;$$/d'				\
	> $@T
	< $(srcdir)/$@-TEMPLATE					\
	$(SED) -e '/libserveez.h/s//($(PACKAGE_STRING)) &/'	\
	       -e '/#define/r $@T' 				\
	> $@
	rm -f $@T

# NB: This means we need not mention those files in $(hbits) in the
# libserveez{/codec,/coserver}/Makefile.am files (tested w/ Automake 1.12.4).
EXTRA_DIST += $(hbits) libserveez.h-TEMPLATE

serveez_SOURCES = \
	action.h cpp-tricks.h le-u32-hash.h timidity.h \
	gi.c gi.h \
	serveez.c \
	guile.c guile.h \
	cfgfile.c cfgfile.h \
	option.c option.h \
	guile-server.c guile-server.h \
	guile-bin.c guile-bin.h

noinst_HEADERS = changedir.h misc-macros.h \
	networking-headers.h o-binary.h \
	unused.h \
	woe-statpred.h woe-wait.h

EXTRA_serveez_SOURCES = guile-api.c guile-api.h

EXTRA_DIST += gsk-make
gsk.c: gsk-make
	$(gx) $(srcdir)/gsk-make > $@

serveez_DEPENDENCIES = $(PROTOLIBS) \
	libserveez/libserveez.la

serveez_LDADD = $(resource_OBJECTS) $(PROTOLIBS) \
	libserveez/libserveez.la

serveez_LDFLAGS = $(SERVEEZ_LDFLAGS) $(LDFLAGS)

snarfcppopts = $(DEFAULT_INCLUDES) $(CPPFLAGS)

.c.x:
	$(gx) c2x -o $@ $< -- $(snarfcppopts)

if MAINTAINER_MODE
noinst_DATA = scm.tsar c.tsar
endif

foo_ar_opts = -v -l scheme -c utf-8 -m '(guile)'
foo_ar_update = $(foo_ar_opts) -f $@ update $^

scm_docable = guile-boot.scm

scm.tsar: $(scm_docable)
	$(gx) tsar $(foo_ar_update)

c_docable = \
  guile-api.c \
  guile-bin.c \
  guile-server.c \
  guile.c

c.tsar: $(c_docable)
	$(gx) c-tsar $(foo_ar_update) -- $(snarfcppopts)

dist-hook:
	rm -f $(distdir)/svzconfig.h

SUFFIXES = .scm .puny .c

.puny.c:
	$(gx) as-C-byte-array -v high -t char $< > $@

.scm.puny:
	$(gx) punify -n $< > $@

# woe

resource_OBJECTS = $(resource_FILES:.rc=.ro)
serveez_DEPENDENCIES += $(resource_OBJECTS)
# We don't do this to avoid disturbing the linker command-line order (above).
# serveez_LDADD += $(resource_OBJECTS)

SUFFIXES += .rc .ro

.rc.ro:
	$(WINDRES) -i $< -o $@

CLEANFILES += *.ro
