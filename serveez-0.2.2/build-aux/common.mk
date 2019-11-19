# Copyright (C) 2011-2013 Thien-Thi Nguyen
#
# This file is part of GNU Serveez.
#
# GNU Serveez is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Serveez is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this package.  If not, see <http://www.gnu.org/licenses/>.

AM_CPPFLAGS = $(SERVEEZ_CFLAGS) -I$(top_srcdir)/src
CLEANFILES = *~ *.orig *.rej
MAINTAINERCLEANFILES = Makefile.in

gx = $(top_srcdir)/build-aux/guile-baux/gbaux-do
