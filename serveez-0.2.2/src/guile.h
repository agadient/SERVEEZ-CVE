/*
 * guile.h - interface to guile core library declarations
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __GUILE_H__
#define __GUILE_H__ 1

/* FAIL breaks to the label `out' and sets an error condition.  */
#define FAIL() do { err = -1; goto out; } while(0)

/* Global error flag.  */
int guile_global_error;

/* Export these functions.  */
int guile_to_integer (SCM, int *);
int guile_to_boolean (SCM, int *);
svz_array_t *guile_to_intarray (SCM, const char *);
svz_array_t *guile_to_strarray (SCM, const char *);
svz_hash_t *guile_to_hash (SCM, const char *);
svz_hash_t *guile_to_optionhash (SCM, char *, int);
SCM guile_strarray_to_guile (svz_array_t *);
SCM guile_intarray_to_guile (svz_array_t *);
SCM guile_hash_to_guile (svz_hash_t *);
void guile_error (char *, ...);
int guile_load_config (char *);

int optionhash_validate (svz_hash_t *, int, char *, char *);
void optionhash_destroy (svz_hash_t *);
SCM optionhash_get (svz_hash_t *, char *);
int optionhash_extract_string (svz_hash_t *, char *, int, char *, char **,
                               char *);

#endif /* not __GUILE_H__ */
