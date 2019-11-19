## Process this file with automake to produce Makefile.in
#
# Makefile.am
#
# Automake input file.
#
# Copyright (C) 2011-2013 Thien-Thi Nguyen
# Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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

ACLOCAL_AMFLAGS = -I m4

include $(top_srcdir)/build-aux/common.mk

SUBDIRS = src test doc

EXTRA_DIST = BUGS autogen.sh README-alpha README-cross
EXTRA_DIST += build-aux/guile-baux
EXTRA_DIST += build-aux/epsf.tex
EXTRA_DIST += data

CLEANFILES += *.rpm *.deb *.changes build-stamp
MAINTAINERCLEANFILES += aclocal.m4 config.h.in configure \
	stamp-h.in stamp-h[0-9].in
DISTCLEANFILES = config.cache config.log

dist-hook:
	rm -f $(distdir)/README
	$(SED) -e '/^>BLURB</{'			\
	       -e ' s/.*//'			\
	       -e ' r $(srcdir)/doc/blurb.texi'	\
	       -e '}'				\
	  $(srcdir)/README			\
	| $(SED) '/^@c/d' > $(distdir)/README

uninstall-hook:
	 rmdir "$(DESTDIR)$(pkgdatadir)"
