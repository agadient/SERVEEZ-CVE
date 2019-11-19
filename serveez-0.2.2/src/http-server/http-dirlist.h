/*
 * http-dirlist.h - http protocol dirlist header file
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __HTTP_DIRLIST_H__
#define __HTTP_DIRlIST_H__

/*
 * Create a directory listing of the directory given as dirname.  If the
 * name contains a trailing slash it is removed (not if it is '/' though).
 * docroot is a document root.  The output suppresses this part of
 * the directory name.
 * The return value is a string containing a directory listing in some
 * hard coded way.  The global variable http_dirlist_size is set
 * to the actual size of this buffer (for debugging/memory counting).
 * If NULL is returned, something was wrong with the directory.  You may
 * check errno for details.
 * ...and don't forget too ‘free’ the data somewhere somewhen...
 */
char *http_dirlist (char *dirname, char *docroot, char *userdir);
extern int http_dirlist_size;


/* Internal buffer sizes */

#define DIRLIST_SPACE 1024        /* Initial size of buffer */
#define DIRLIST_SPACE_GROW 512    /* Growsize of buffer */
#define DIRLIST_SPACE_NAME 1024   /* Bufferspace for stat'ed filenames */
#define DIRLIST_SPACE_ENTRY 512   /* Max. size of a single line (1 per file) */
#define DIRLIST_SPACE_POST 1024   /* Max. size of postamble */

#define HAVE_SORTED_LIST (HAVE_SCANDIR && HAVE_ALPHASORT)

#endif /* __HTTP_DIRLIST_H__ */
