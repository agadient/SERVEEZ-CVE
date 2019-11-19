/* action.h --- support for context-sensitive messages
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

/* These macros support building a "current action" string, useful for
   diagnostic messages.  Because the string is built in a (function-)
   local buffer, it's cheap and low-maintenance.  The buffer MUST be
   declared exactly as written here:

   char action[ACTIONBUFSIZE];

   and the file should #include "action.h", of course.  */

#define ACTIONBUFSIZE  256

#define DOING(fmt, ...)  do                     \
    {                                           \
      if (ACTIONBUFSIZE                         \
          <= snprintf (action, ACTIONBUFSIZE,   \
                       fmt, __VA_ARGS__))       \
        action[ACTIONBUFSIZE - 1] = '\0';       \
    }                                           \
  while (0)

#define DEFINING(rest, ...)  DOING ("defining " rest, __VA_ARGS__)
