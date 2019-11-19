/* timidity.h --- provide ‘assert’, conditionally suppressed
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

#ifndef __TIMIDITY_H__
#define __TIMIDITY_H__ 1

#if !defined ENABLE_DEBUG && !defined NDEBUG
#define NDEBUG 1
#endif

#include <assert.h>

#endif  /* !defined __TIMIDITY_H__ */
