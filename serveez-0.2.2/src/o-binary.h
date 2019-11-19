/* o-binary.h --- maybe #define O_BINARY
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

#ifndef __O_BINARY_H__
#define __O_BINARY_H__ 1

#include <fcntl.h>

/* `open ()' files with this additional flag */
#ifndef O_BINARY
# define O_BINARY 0
#endif

#endif  /* !defined __O_BINARY_H__ */
