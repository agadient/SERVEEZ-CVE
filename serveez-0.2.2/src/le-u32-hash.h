/* le-u32-hash.h --- a simple hash function
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __LE_U32_HASH_H__
#define __LE_U32_HASH_H__ 1

#include <stdint.h>

/* Historical note: This used to be called ‘SVZ_UINT32’.
   The "le" in its new name stands for "little-endian".  */
#define le_u32_hash(p)                          \
  ((uint8_t) *p                                 \
   | ((uint8_t) *(p + 1) << 8)                  \
   | ((uint8_t) *(p + 2) << 16)                 \
   | ((uint8_t) *(p + 3) << 24))

#endif  /* !defined __LE_U32_HASH_H__ */
