## Process this file with automake to produce Makefile.in
#
# src/libserveez/codec/Makefile.am
#
# Automake input file for the encoder/decoder interface.
#
# Copyright (C) 2011-2013 Thien-Thi Nguyen
# Copyright (C) 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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

noinst_LTLIBRARIES = libcodec.la

libcodec_la_SOURCES = codec.c
if WITH_ZLIB
libcodec_la_SOURCES += gzlib.c
endif
if WITH_BZLIB
libcodec_la_SOURCES += bzip2.c
endif

noinst_HEADERS = gzlib.h bzip2.h
