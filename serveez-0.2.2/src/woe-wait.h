/* woe-wait.h --- sugar for ‘WaitForSingleObject’ (woe32)
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __WOE_WAIT_H__
#define __WOE_WAIT_H__ 1

#ifdef __MINGW32__

#define WOE_WAIT_1(x)    WaitForSingleObject (x, 1)
#define WOE_WAIT_INF(x)  WaitForSingleObject (x, INFINITE)

#define WOE_WAIT_LOG_ERROR(prefix)                      \
  svz_log_sys_error (prefix "WaitForSingleObject")

#define WOE_WAIT_LOG_ERROR_ANONYMOUSLY()  WOE_WAIT_LOG_ERROR ("")

#endif  /* __MINGW32__ */

#endif  /* !defined __WOE_WAIT_H__ */
