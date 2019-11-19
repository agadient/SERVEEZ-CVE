/*
 * irc-crypt.c - IRC crypt routines
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
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

#include <stdio.h>
#include <string.h>
#include "networking-headers.h"
#include "libserveez.h"
#include "irc-proto.h"
#include "irc-crypt.h"

/*
 * Generate a key for the de- and encryption routine.
 */
uint8_t
irc_gen_key (char *pass)
{
  uint8_t *p;
  int n;
  uint8_t key;

  key = 0;
  n = 0;
  p = (uint8_t *) pass;
  while (*p)
    {
      key += (uint8_t) ((*p + n) ^ IRC_CRYPT_BYTE);
      n++;
      p++;
    }
  return key;
}

/*
 * Encrypt a string by a given key.
 */
void
irc_encrypt_text (char *text, uint8_t key)
{
  char crypt[MAX_MSG_LEN];
  char *t, *c;
  uint8_t code;

  memset (crypt, 0, MAX_MSG_LEN);
  t = text;
  c = crypt;

  while (*t)
    {
      code = (uint8_t) (*t ^ key);
      if (code < (uint8_t) 0x20 || code == IRC_CRYPT_PREFIX)
        {
          *c++ = IRC_CRYPT_PREFIX;
          *c++ = (char) (code + IRC_CRYPT_PREFIX);
        }
      else
        {
          *c++ = code;
        }
      t++;
    }
  strcpy (text, crypt);
}

/*
 * Decrypt a string by a given key.
 */
char *
irc_decrypt_text (char *crypt, uint8_t key)
{
  static char text[MAX_MSG_LEN];
  char *t, *c;

  memset (text, 0, MAX_MSG_LEN);
  t = text;
  c = crypt;

  while (*c)
    {
      if (*c == IRC_CRYPT_PREFIX)
        {
          c++;
          *t++ = (char) ((*c - IRC_CRYPT_PREFIX) ^ key);
        }
      else
        {
          *t++ = (char) (*c ^ key);
        }
      c++;
    }
  return text;
}
