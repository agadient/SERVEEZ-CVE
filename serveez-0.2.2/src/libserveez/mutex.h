/*
 * mutex.h - thread mutex definitions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __MUTEX_H__
#define __MUTEX_H__ 1

#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif

#ifdef __MINGW32__
# include <windows.h>
#endif

#ifdef __MINGW32__

/* Windows native */
typedef svz_t_handle svz_mutex_t;
# define SVZ_MUTEX_INITIALIZER NULL

#else

/* POSIX threads */
typedef pthread_mutex_t svz_mutex_t;
# define SVZ_MUTEX_INITIALIZER PTHREAD_MUTEX_INITIALIZER

#endif

SBO int svz_mutex_create (svz_mutex_t *);
SBO int svz_mutex_destroy (svz_mutex_t *);
SBO int svz_mutex_lock (svz_mutex_t *);
SBO int svz_mutex_unlock (svz_mutex_t *);

#endif /* not __MUTEX_H__ */
