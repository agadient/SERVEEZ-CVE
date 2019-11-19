/*
 * mutex.c - thread mutex implementations
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

#include "config.h"

#if ENABLE_LOG_MUTEX

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif

#include "woe-wait.h"
#include "libserveez/util.h"
#include "libserveez/mutex.h"

/* Creates and initializes the given @var{mutex} object.  The mutex is
   in an unlocked state.  The function must be called before using
   @code{svz_mutex_lock} or @code{svz_mutex_unlock}.  The user
   must call @code{svz_mutex_destroy} for each mutex created by this
   function.  */
int
svz_mutex_create (svz_mutex_t *mutex)
{
#ifdef __MINGW32__ /* Windoze native */
  if ((*mutex = CreateMutex (NULL, FALSE, NULL)) == NULL)
    {
      svz_log_sys_error ("CreateMutex");
      return -1;
    }
  return 0;
#else /* POSIX threads */
  return pthread_mutex_init (mutex, NULL);
#endif
}

/* Destroys the given @var{mutex} object which has been created by
   @code{svz_mutex_create}.  */
int
svz_mutex_destroy (svz_mutex_t *mutex)
{
#ifdef __MINGW32__
  if (!CloseHandle (*mutex))
    {
      svz_log_sys_error ("CloseHandle");
      return -1;
    }
  *mutex = SVZ_MUTEX_INITIALIZER;
  return 0;
#else
  if (pthread_mutex_destroy (mutex) != 0)
    {
      svz_log_sys_error ("pthread_mutex_destroy");
      return -1;
    }
  return 0;
#endif
}

/* Locks a @var{mutex} object and sets the current thread into an idle
   state if the @var{mutex} object has been currently locked by another
   thread.  */
int
svz_mutex_lock (svz_mutex_t *mutex)
{
#ifdef __MINGW32__
  if (WOE_WAIT_INF (*mutex) == WAIT_FAILED)
    {
      WOE_WAIT_LOG_ERROR_ANONYMOUSLY ();
      return -1;
    }
  return 0;
#else
  if (pthread_mutex_lock (mutex) != 0)
    {
      svz_log_sys_error ("pthread_mutex_lock");
      return -1;
    }
  return 0;
#endif
}

/* Releases the given @var{mutex} object and thereby possibly resumes
   a waiting thread calling @code{svz_mutex_lock}.  */
int
svz_mutex_unlock (svz_mutex_t *mutex)
{
#ifdef __MINGW32__
  if (!ReleaseMutex (mutex))
    {
      svz_log_sys_error ("ReleaseMutex");
      return -1;
    }
  return 0;
#else
  if (pthread_mutex_unlock (mutex) != 0)
    {
      svz_log_sys_error ("pthread_mutex_unlock");
      return -1;
    }
  return 0;
#endif
}

#endif  /* ENABLE_LOG_MUTEX */
