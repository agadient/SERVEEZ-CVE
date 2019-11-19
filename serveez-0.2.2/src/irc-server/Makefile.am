## Process this file with automake to produce Makefile.in
#
# src/irc-server/Makefile.am
#
# EFNet compatible IRC server.
#
# Copyright (C) 2011-2013 Thien-Thi Nguyen
# Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
# Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
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

noinst_LIBRARIES = libircserver.a

libircserver_a_SOURCES = \
	irc-proto.c irc-proto.h \
	irc-crypt.c irc-crypt.h \
	irc-event.h \
	irc-event-1.c \
	irc-event-2.c \
	irc-event-3.c \
	irc-event-4.c \
	irc-event-5.c \
	irc-event-6.c \
	irc-event-7.c \
	irc-server.h irc-server.c \
	irc-config.h irc-config.c

BUILT_SOURCES = timestamp.c

timestamp.c: $(top_builddir)/config.status
	date -r $< -u +'static char created[] = "%Y-%m-%d %R UTC";' > $@
