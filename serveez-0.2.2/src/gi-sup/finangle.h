/* finangle.h --- abstractions for Scheme objects to C string conversion
   serial 2

   Copyright (C) 2012 Thien-Thi Nguyen

   This is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this package; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA  02110-1301  USA.
*/

typedef struct {
  char *s;
  size_t len;
} range_t;

#define RS(svar)    c ## svar .s
#define RLEN(svar)  c ## svar .len

#if GI_LEVEL_1_8

#define FINANGLABLE_SCHEME_STRING_FROM_SYMBOL  scm_symbol_to_string

/* TODO: Make it configurable whether to implement ‘_FINANGLE’ as a function
   plus macro (as is currently), or simply as a macro (serial 1).  */

static void
_finangle (SCM svar, range_t *r, int p1)
{
  r->s = scm_to_locale_stringn (svar, &r->len);
  if (r->s)
    {
      if (p1 && r->s[r->len])
        {
          r->s = realloc (r->s, 1 + r->len);
          r->s[r->len] = '\0';
        }
    }
  else
    r->s = strdup ("");
}

#define _FINANGLE(svar,p1)  _finangle (svar, &c ## svar, p1)

#define UNFINANGLE(svar)  free (RS (svar))

#else  /* !GI_LEVEL_1_8 */

#define FINANGLABLE_SCHEME_STRING_FROM_SYMBOL(sym)      \
  scm_string_copy (scm_symbol_to_string (sym))

/* Coerce a Scheme (sub)string that is to be used in contexts where the
   extracted C string is expected to be read-only and zero-terminated.  We
   check this condition precisely instead of simply coercing all (sub)strings,
   to avoid waste for those substrings that may in fact (by lucky accident)
   already satisfy the condition.  */
#define ROZT_X(x)                                       \
  if (SCM_ROCHARS (x) [SCM_ROLENGTH (x)])               \
    x = BSTRING (SCM_ROCHARS (x), SCM_ROLENGTH (x))

#define _FINANGLE(svar,p1)  do                  \
    {                                           \
      if (p1)                                   \
        ROZT_X (svar);                          \
      RS (svar) = SCM_ROCHARS (svar);           \
      RLEN (svar) = SCM_ROLENGTH (svar);        \
    }                                           \
  while (0)

#define UNFINANGLE(svar)

#endif  /* !GI_LEVEL_1_8 */

/* Use ‘FINANGLE_RAW’ when the consumer of the C string takes full range
   (start address plus length) info.  Otherwise, ‘FINANGLE’.  */

#define FINANGLE_RAW(svar)  _FINANGLE (svar, 0)
#define FINANGLE(svar)      _FINANGLE (svar, 1)

/* finangle.h ends here */
