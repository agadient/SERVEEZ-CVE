## Process this file with automake to produce Makefile.in
#
# src/guile-server/Makefile.am
#
# Copyright (C) 2011-2013 Thien-Thi Nguyen
# Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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

EXTRA_DIST = mandel-server.scm mandel-client.scm mandel-shared.scm \
	echo-server.scm eval-server.scm inetd.scm icecast-server.scm

pkgdata_DATA = inetd.scm echo-server.scm eval-server.scm \
	mandel-server.scm mandel-client.scm mandel-shared.scm \
	icecast-server.scm
