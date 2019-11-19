/*
 * guile.c - interface to Guile core library
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2002 Andreas Rottmann <a.rottmann@gmx.at>
 * Copyright (C) 2001 Raimund Jacob <raimi@lkcc.org>
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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef __MINGW32__
# include <io.h>
#endif

#include <libguile.h>
#include "networking-headers.h"
#include "action.h"
#include "libserveez.h"
#include "misc-macros.h"
#include "gi.h"
#include "guile-api.h"
#include "guile-server.h"
#include "guile.h"
#include "unused.h"

/* Port configuration items.  */
#define PORTCFG_PORT    "port"
#define PORTCFG_PROTO   "proto"
#define PORTCFG_TCP     "tcp"
#define PORTCFG_UDP     "udp"
#define PORTCFG_ICMP    "icmp"
#define PORTCFG_RAW     "raw"
#define PORTCFG_PIPE    "pipe"
#define PORTCFG_IP      "ipaddr"
#define PORTCFG_DEVICE  "device"
#define PORTCFG_BACKLOG "backlog"
#define PORTCFG_TYPE    "type"

/* Pipe definitions.  */
#define PORTCFG_RECV  "recv"
#define PORTCFG_SEND  "send"
#define PORTCFG_NAME  "name"
#define PORTCFG_PERMS "permissions"
#define PORTCFG_USER  "user"
#define PORTCFG_GROUP "group"
#define PORTCFG_UID   "uid"
#define PORTCFG_GID   "gid"

/* Miscellaneous definitions.  */
#define PORTCFG_SEND_BUFSIZE "send-buffer-size"
#define PORTCFG_RECV_BUFSIZE "recv-buffer-size"
#define PORTCFG_FREQ         "connect-frequency"
#define PORTCFG_ALLOW        "allow"
#define PORTCFG_DENY         "deny"

/*
 * Global error flag that indicating failure of one of the parsing
 * functions.
 */
int guile_global_error = 0;

/*
 * Global variable containing the current load port in exceptions.
 * FIXME: Where should I aquire it?  In each procedure?
 */
static SCM load_port_box;
#define stashed_load_port()  (SCM_CDR (load_port_box))
#define GUILE_PRECALL() guile_set_current_load_port()

/*
 * What is an 'option-hash' ?
 * We build up that data structure from a scheme pairlist.  The pairlist has
 * to be an alist which is a key => value mapping.  We read that mapping and
 * construct a @code{svz_hash_t} from it.  The values of this hash are
 * pointers to @code{guile_value_t} structures.  The @code{guile_value_t}
 * structure contains a @code{defined} field which counts the number of
 * occurrences of the key.  Use @code{optionhash_validate} to make sure it
 * is 1 for each key.  The @code{use} field is to make sure that each key
 * was needed exactly once.  Use @code{optionhash_validate} again to find
 * out which ones were not needed.
 */

/*
 * Used as value in option-hashes.
 */
typedef struct guile_value
{
  SCM value;     /* the scheme value itself, invalid when defined != 1 */
  int defined;   /* the number of definitions, 1 to be valid */
  int use;       /* how often was it looked up in the hash, 1 to be valid */
}
guile_value_t;

/*
 * Create a guile value structure with the given @var{value}.  Initializes
 * the usage counter to zero.  The define counter is set to 1.
 */
static guile_value_t *
guile_value_create (SCM value)
{
  guile_value_t *v = svz_malloc (sizeof (guile_value_t));
  v->value = value;
  v->defined = 1;
  v->use = 0;
  return v;
}

/*
 * Create a fresh option-hash.
 */
svz_hash_t *
optionhash_create (void)
{
  return svz_hash_create (4, svz_free);
}

/*
 * Destroy the given option-hash @var{options}.
 */
void
optionhash_destroy (svz_hash_t *options)
{
  svz_hash_destroy (options);
}

/*
 * Save the current load port for later usage in the @code{guile_error}
 * function.
 */
static void
guile_set_current_load_port (void)
{
  SCM p = scm_current_load_port ();
  if (SCM_PORTP (p))
    scm_set_cdr_x (load_port_box, p);
}

/*
 * Returns the current load port.  This is either the "real" one or the one
 * saved by the procedure @code{guile_set_current_load_port}.  If neither
 * is possible return @code{SCM_UNDEFINED}.
 */
static SCM
guile_get_current_load_port (void)
{
  SCM p = scm_current_load_port ();
  if (!SCM_FALSEP (p) && SCM_PORTP (p))
    return p;
  p = stashed_load_port ();
  if (BOUNDP (p) && SCM_PORTP (p))
    return p;
  return SCM_UNDEFINED;
}

/*
 * Report some error at the current scheme position.  Prints to stderr
 * but lets the program continue.  The format string @var{format} does not
 * need a trailing newline.
 */
void
guile_error (char *format, ...)
{
  va_list args;
  /* FIXME: Why is this port undefined in guile exceptions?  */
  SCM lp = guile_get_current_load_port ();
  int lp_valid_p = BOUNDP (lp)
    /* TODO: Investigate why ‘SCM_PORTP’ is (was) sufficient for
       Guile 1.8 but not 2.0 (which is succeptible to segv).
       TODO: Do all validation in ‘guile_get_current_load_port’.  */
    && SCM_OPINPORTP (lp);
  SCM filename = lp_valid_p ? SCM_FILENAME (lp) : SCM_BOOL_F;
  char file[1024];

  /* guile counts lines from 0, we have to add one */
  fprintf (stderr, "%s:%ld:%ld: ",
           (lp_valid_p && GI_GET_XREP_MAYBE (file, filename)
            ? file
            : "undefined"),
           lp_valid_p ? (long) SCM_LINUM (lp) + 1 : 0,
           lp_valid_p ? (long) SCM_COL (lp) : 0);

  va_start (args, format);
  vfprintf (stderr, format, args);
  va_end (args);
  fprintf (stderr, "\n");
}

struct optionhash_validate_closure
{
  int what;
  int errors;
  char *type;
  char *name;
};

static void
optionhash_validate_internal (void *k, void *v, void *closure)
{
  char *key = k, *blurb;
  guile_value_t *value = v;
  struct optionhash_validate_closure *x = closure;

  switch (x->what)
    {
    case 1:
      /* Check definition counter.  */
      if (value->defined == 1)
        return;
      blurb = "Multiple definitions of";
      break;

    case 0:
      /* Check usage counter.  */
      if (value->use != 0)
        return;
      blurb = "Unused variable";
      break;

    default:
      abort ();                         /* PEBKAC */
    }

  x->errors++;
  guile_error ("%s `%s' in %s `%s'", blurb, key, x->type, x->name);
}

/*
 * Validate the values of an option-hash.  Returns the number of errors.
 * what = 0 : check if all 'use' fields are 1
 * what = 1 : check if all 'defined' fields are 1
 * type     : what kind of thing the option-hash belongs to
 * name     : current variable name (specifying the alist)
 */
int
optionhash_validate (svz_hash_t *hash, int what, char *type, char *name)
{
  struct optionhash_validate_closure x = { what, 0, type, name };

  svz_hash_foreach (optionhash_validate_internal, hash, &x);
  return x.errors;
}

/*
 * Get a scheme value from an option-hash and increment its 'use' field.
 * Returns SCM_UNSPECIFIED when @var{key} was not found.
 */
SCM
optionhash_get (svz_hash_t *hash, char *key)
{
  guile_value_t *val = svz_hash_get (hash, key);

  if (NULL != val)
    {
      val->use++;
      return val->value;
    }
  return SCM_UNSPECIFIED;
}

/*
 * Traverse a scheme pairlist that is an associative list and build up
 * a hash from it.  Emits error messages and returns NULL when it did
 * so.  Hash keys are the key names.  Hash values are pointers to
 * guile_value_t structures.  If @var{dounpack} is set the pairlist's
 * car is used instead of the pairlist itself.  You have to unpack when
 * no "." is in front of the pairlist definition (in scheme
 * code).  Some please explain the "." to Stefan and Raimi...
 */
svz_hash_t *
guile_to_optionhash (SCM pairlist, char *suffix, int dounpack)
{
  svz_hash_t *hash = optionhash_create ();
  guile_value_t *old_value;
  guile_value_t *new_value;
  int err = 0;

  /* Unpack if requested, ignore if null already (null == not existent).  */
  if (dounpack && !SCM_NULLP (pairlist) && BOUNDP (pairlist))
    pairlist = SCM_CAR (pairlist);

  for ( ; SCM_PAIRP (pairlist); pairlist = SCM_CDR (pairlist))
    {
      SCM pair = SCM_CAR (pairlist);
      SCM key, val;
      char str[64];

      /* The car must be another pair which contains key and value.  */
      if (!SCM_PAIRP (pair))
        {
          guile_error ("Not a pair %s", suffix);
          err = 1;
          break;
        }
      key = SCM_CAR (pair);
      val = SCM_CDR (pair);

      if (! GI_GET_XREP_MAYBE (str, key))
        {
          /* Unknown key type, must be string or symbol.  */
          guile_error ("Invalid key type (string expected) %s", suffix);
          err = 1;
          break;
        }

      /* Remember key and value.  */
      new_value = guile_value_create (val);
      if (NULL != (old_value = svz_hash_get (hash, str)))
        {
          /* Multiple definition, let caller croak about that error.  */
          new_value->defined += old_value->defined;
          svz_free_and_zero (old_value);
        }
      svz_hash_put (hash, str, (void *) new_value);
    }

  /* Pairlist must be ‘SCM_NULLP’ now or that was not a good pairlist.  */
  if (!err && !SCM_NULLP (pairlist))
    {
      guile_error ("Invalid pairlist %s", suffix);
      err = 1;
    }

  if (err)
    {
      svz_hash_destroy (hash);
      return NULL;
    }

  return hash;
}

SCM_DEFINE
(libserveez_features,
 "libserveez-features", 0, 0, 0,
 (void),
 doc: /***********
Return a list of symbols representing the features of the underlying
libserveez.  For details, @xref{Library features}.  */)
{
#define FUNC_NAME s_libserveez_features
  SCM rv = SCM_EOL;
  size_t count;
  const char * const *ls = svz_library_features (&count);

  while (count--)
    rv = scm_cons (gi_symbol2scm (ls[count]), rv);

  return rv;
#undef FUNC_NAME
}

/*
 * Parse an integer value from a scheme cell.  Returns zero when successful.
 * Stores the integer value where @var{target} points to.  Does not emit
 * error messages.
 */
int
guile_to_integer (SCM cell, int *target)
{
#define FUNC_NAME __func__
  int err = 0;
  char str[64], *endp;

  /* Usual guile exact number.  */
  if (gi_exactp (cell))
    {
      *target = gi_scm2int (cell);
    }
  /* Try string (or even symbol) to integer conversion.  */
  else if (GI_GET_XREP_MAYBE (str, cell))
    {
      errno = 0;
      *target = strtol (str, &endp, 10);
      if (*endp != '\0' || errno == ERANGE)
        err = 1;
    }
  /* No chance.  */
  else
    {
      err = 2;
    }
  return err;
#undef FUNC_NAME
}

/*
 * Parse a boolean value from a scheme cell.  We consider integers and #t/#f
 * as boolean boolean values.  Represented as integers internally.  Returns
 * zero when successful.  Stores the boolean/integer where @var{target} points
 * to.  Does not emit error messages.
 */
int
guile_to_boolean (SCM cell, int *target)
{
#define FUNC_NAME __func__
  int i;
  int err = 0;
  char str[8];

  /* Usual guile boolean.  */
  if (SCM_BOOLP (cell))
    {
      i = gi_nfalsep (cell);
      *target = (i == 0 ? 0 : 1);
    }
  /* Try with the integer converter.  */
  else if (guile_to_integer (cell, &i) == 0)
    {
      *target = (i == 0 ? 0 : 1);
    }
  /* Neither integer nor boolean, try text conversion.  */
  /* FIXME: Use symbols and ‘memq’.  */
  else if (GI_GET_XREP_MAYBE (str, cell))
    {
#define STR_IS_CI(k)  (! strncasecmp (k, str, sizeof (k)))
      /* Note: We use ‘sizeof (k)’ instead of ‘sizeof (k) - 1’
         to deliberately include the terminating '\0', thus
         preventing false matches, e.g., "yesssss".  */

      if (STR_IS_CI ("yes")
          || STR_IS_CI ("on")
          || STR_IS_CI ("true"))
        *target = 1;
      else if (STR_IS_CI ("no")
               || STR_IS_CI ("off")
               || STR_IS_CI ("false"))
        *target = 0;
      else
        err = 1;
#undef STR_IS_CI
    }
  else
    {
      err = 2;
    }
  return err;
#undef FUNC_NAME
}

/*
 * Convert the given guile list @var{list} into a hash.  Return
 * @code{NULL} on failure.  Error messages will be emitted if
 * necessary.
 */
svz_hash_t *
guile_to_hash (SCM list, const char *func)
{
#define FUNC_NAME func
  int err = 0, i;
  svz_hash_t *hash;

  /* Is this valid guile list at all?  */
  if (!SCM_LISTP (list))
    {
      err = -1;
      guile_error ("%s: Not a valid list for hash", func);
      return NULL;
    }

  /* Iterate the alist.  */
  hash = svz_hash_create (gi_scm2ulong (scm_length (list)),
                          svz_free);
  for (i = 0; SCM_PAIRP (list); list = SCM_CDR (list), i++)
    {
      SCM k, v, pair = SCM_CAR (list);
      char str[2048], *keystr, *valstr;

      if (!SCM_PAIRP (pair))
        {
          err = -1;
          guile_error ("%s: Element #%d of hash is not a pair", func, i);
          continue;
        }
      k = SCM_CAR (pair);
      v = SCM_CDR (pair);

      /* Obtain key character string.  */
      if (! GI_GET_XREP_MAYBE (str, k))
        {
          err = -1;
          guile_error ("%s: Element #%d of hash has no valid key "
                       "(string expected)", func, i);
          keystr = NULL;
        }
      else
        {
          keystr = svz_strdup (str);
        }

      /* Obtain value character string.  */
      if (! GI_GET_XREP_MAYBE (str, v))
        {
          err = -1;
          guile_error ("%s: Element #%d of hash has no valid value "
                       "(string expected)", func, i);
          valstr = NULL;
        }
      else
        {
          valstr = svz_strdup (str);
        }

      /* Add to hash if key and value look good.  */
      if (keystr != NULL && valstr != NULL)
        {
          svz_hash_put (hash, keystr, valstr);
          svz_free (keystr);
        }
    }

  /* Free the values, keys are freed by hash destructor.  */
  if (err)
    {
      svz_hash_destroy (hash);
      return NULL;
    }
  return hash;
#undef FUNC_NAME
}

/*
 * Convert the given non-empty @var{list} into an array of duplicated strings.
 * Return @code{NULL} if it is not a valid non-empty list.  Print an error
 * message if one of the list's elements is not a string.  The additional
 * argument @var{func} should be the name of the caller.
 */
svz_array_t *
guile_to_strarray (SCM list, const char *func)
{
#define FUNC_NAME func
  svz_array_t *array;
  int i;
  char str[2048];

  /* Check if the given scheme cell is a valid list.  */
  if (!SCM_LISTP (list))
    {
      guile_error ("%s: String array is not a valid list", func);
      return NULL;
    }

  /* Iterate over the list and build up the array of strings.  */
  array = svz_array_create (gi_scm2ulong (scm_length (list)),
                            svz_free);
  for (i = 0; SCM_PAIRP (list); list = SCM_CDR (list), i++)
    {
      SCM head = SCM_CAR (list);

      if (! gi_stringp (head)
          || 0 > GI_GET_XREP (str, head))
        {
          guile_error ("%s: String expected in position %d", func, i);
          guile_global_error = -1;
          continue;
        }
      svz_array_add (array, svz_strdup (str));
    }

  /* Reject the empty array.  */
  if (! svz_array_size (array))
    {
      svz_array_destroy (array);
      array = NULL;
    }
  return array;
#undef FUNC_NAME
}

/*
 * Convert the given scheme cell @var{list} which needs to be a valid guile
 * list into an array of integers.  The additional argument @var{func} is the
 * name of the caller.  Return NULL on failure.
 */
svz_array_t *
guile_to_intarray (SCM list, const char *func)
{
#define FUNC_NAME func
  svz_array_t *array;
  int i, n;

  /* Check if the given scheme cell is a valid associative list.  */
  if (!SCM_LISTP (list))
    {
      guile_error ("%s: Integer array is not a valid list", func);
      return NULL;
    }

  /* Iterate over the list and build up the array of strings.  */
  array = svz_array_create (gi_scm2ulong (scm_length (list)), NULL);
  for (i = 0; SCM_PAIRP (list); list = SCM_CDR (list), i++)
    {
      if (guile_to_integer (SCM_CAR (list), &n) != 0)
        {
          guile_error ("%s: Integer expected in position %d", func, i);
          guile_global_error = -1;
          continue;
        }
      svz_array_add (array, SVZ_NUM2PTR (n));
    }

  /* Check the size of the resulting integer array.  */
  if (svz_array_size (array) == 0)
    {
      svz_array_destroy (array);
      array = NULL;
    }
  return array;
#undef FUNC_NAME
}

/*
 * Extract an integer value from an option hash.  Returns zero if it worked.
 */
static int
optionhash_extract_int (svz_hash_t *hash,
                        char *key,          /* the key to find      */
                        int hasdef,         /* is there a default ? */
                        int defvar,         /* the default          */
                        int *target,        /* where to put it      */
                        char *txt)          /* appended to error    */
{
  int err = 0;
  SCM hvalue = optionhash_get (hash, key);

  /* Is there such an integer in the option-hash?  */
  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      /* Nothing in hash, try to use default.  */
      if (hasdef)
        *target = defvar;
      else
        {
          guile_error ("No default value for integer `%s' %s", key, txt);
          err = 1;
        }
    }
  /* Convert the integer value.  */
  else if (guile_to_integer (hvalue, target))
    {
      guile_error ("Invalid integer value for `%s' %s", key, txt);
      err = 1;
    }
  return err;
}

/*
 * Extract a string value from an option hash.  Returns zero if it worked.
 * The memory for the string is newly allocated, no matter where it came
 * from.
 */
int
optionhash_extract_string (svz_hash_t *hash,
                           char *key,        /* the key to find       */
                           int hasdef,       /* if there is a default */
                           char *defvar,     /* default               */
                           char **target,    /* where to put it       */
                           char *txt)        /* appended to error     */
{
  SCM hvalue = optionhash_get (hash, key);
  int err = 0;
  char str[2048];

  /* Is there such a string in the option-hash?  */
  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      /* Nothing in hash, try to use default.  */
      if (hasdef)
        *target = svz_strdup (defvar);
      else
        {
          guile_error ("No default value for string `%s' %s", key, txt);
          err = 1;
        }
    }
  else
    {
      /* Try getting the character string.  */
      if (! GI_GET_XREP_MAYBE (str, hvalue))
        {
          guile_error ("Invalid string value for `%s' %s", key, txt);
          err = 1;
        }
      else
        {
          *target = svz_strdup (str);
        }
    }
  return err;
}

/* Before callback for configuring a server.  */
static int
optionhash_cb_before (char *server, void *arg)
{
  svz_hash_t *options = arg;
  if (0 == optionhash_validate (options, 1, "server", server))
    return SVZ_ITEM_OK;
  return SVZ_ITEM_FAILED;
}

/* Integer callback for configuring a server.  */
static int
optionhash_cb_integer (char *server, void *arg, char *key, int *target,
                       int hasdef, UNUSED int def)
{
  svz_hash_t *options = arg;
  SCM hvalue = optionhash_get (options, key);

  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      if (hasdef)
        return SVZ_ITEM_DEFAULT_ERRMSG;

      guile_error ("%s: You have to define an integer called `%s'",
                   server, key);
      return SVZ_ITEM_FAILED;
    }

  if (guile_to_integer (hvalue, target))
    {
      guile_error ("%s: Invalid integer value for `%s'", server, key);
      return SVZ_ITEM_FAILED;
    }

  return SVZ_ITEM_OK;
}

/* Boolean callback for configuring a server.  */
static int
optionhash_cb_boolean (char *server, void *arg, char *key, int *target,
                       int hasdef, UNUSED int def)
{
  svz_hash_t *options = arg;
  SCM hvalue = optionhash_get (options, key);

  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      if (hasdef)
        return SVZ_ITEM_DEFAULT_ERRMSG;

      guile_error ("%s: You have to define a boolean called `%s'",
                   server, key);
      return SVZ_ITEM_FAILED;
    }

  if (guile_to_boolean (hvalue, target))
    {
      guile_error ("%s: Invalid boolean value for `%s'", server, key);
      return SVZ_ITEM_FAILED;
    }

  return SVZ_ITEM_OK;
}

/* Integer array callback for configuring a server.  */
static int
optionhash_cb_intarray (char *server, void *arg, char *key,
                        svz_array_t **target, int hasdef,
                        UNUSED svz_array_t *def)
{
  svz_hash_t *options = arg;
  SCM hvalue = optionhash_get (options, key);

  /* Is that integer array defined in the option-hash?  */
  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      if (hasdef)
        return SVZ_ITEM_DEFAULT_ERRMSG;

      guile_error ("%s: You have to define a integer array called `%s'",
                   server, key);
      return SVZ_ITEM_FAILED;
    }
  /* Yes, start parsing it.  */
  else
    {
      svz_array_t *array;

      if ((array = guile_to_intarray (hvalue, key)) == NULL)
        {
          guile_error ("%s: Failed to parse integer array `%s'", server, key);
          return SVZ_ITEM_FAILED;
        }
      *target = array;
    }
  return SVZ_ITEM_OK;
}

/* String callback for configuring a server.  */
static int
optionhash_cb_string (char *server, void *arg, char *key,
                      char **target, int hasdef, UNUSED char *def)
{
  svz_hash_t *options = arg;
  SCM hvalue = optionhash_get (options, key);
  char str[2048];

  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      if (hasdef)
        return SVZ_ITEM_DEFAULT_ERRMSG;

      guile_error ("%s: You have to define a string called `%s'",
                   server, key);
      return SVZ_ITEM_FAILED;
    }

  if (! GI_GET_XREP_MAYBE (str, hvalue))
    {
      guile_error ("%s: Invalid string value for `%s'", server, key);
      return SVZ_ITEM_FAILED;
    }

  *target = svz_strdup (str);
  return SVZ_ITEM_OK;
}

/* String array callback for configuring a server.  */
static int
optionhash_cb_strarray (char *server, void *arg, char *key,
                        svz_array_t **target, int hasdef,
                        UNUSED svz_array_t *def)
{
  svz_hash_t *options = arg;
  SCM hvalue = optionhash_get (options, key);

  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      if (hasdef)
        return SVZ_ITEM_DEFAULT_ERRMSG;

      guile_error ("%s: You have to define a string array called `%s'",
                   server, key);
      return SVZ_ITEM_FAILED;
    }
  else
    {
      svz_array_t *array;

      if ((array = guile_to_strarray (hvalue, key)) == NULL)
        {
          guile_error ("%s: Failed to parse string array `%s'", server, key);
          return SVZ_ITEM_FAILED;
        }
      *target = array;
    }
  return SVZ_ITEM_OK;
}

/* Hash callback for configuring a server.  */
static int
optionhash_cb_hash (char *server, void *arg, char *key,
                    svz_hash_t **target, int hasdef,
                    UNUSED svz_hash_t *def)
{
  svz_hash_t *options = arg;
  SCM hvalue = optionhash_get (options, key);

  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      if (hasdef)
        return SVZ_ITEM_DEFAULT_ERRMSG;

      guile_error ("%s: You have to define a hash called `%s'",
                   server, key);
      return SVZ_ITEM_FAILED;
    }
  else
    {
      svz_hash_t *hash;

      if ((hash = guile_to_hash (hvalue, key)) == NULL)
        {
          guile_error ("%s: Failed to parse hash `%s'", server, key);
          return SVZ_ITEM_FAILED;
        }
      *target = hash;
    }
  return SVZ_ITEM_OK;
}

/* Port configuration callback for configuring a server.  */
static int
optionhash_cb_portcfg (char *server, void *arg, char *key,
                       svz_portcfg_t **target, int hasdef,
                       UNUSED svz_portcfg_t *def)
{
  svz_hash_t *options = arg;
  SCM hvalue = optionhash_get (options, key);
  svz_portcfg_t *port;
  char str[2048];

  /* Is the requested port configuration defined?  */
  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      if (hasdef)
        return SVZ_ITEM_DEFAULT_ERRMSG;

      guile_error ("%s: Port configuration `%s' required", server, key);
      return SVZ_ITEM_FAILED;
    }

  /* Convert Scheme cell into string.  */
  if (! GI_GET_XREP_MAYBE (str, hvalue))
    {
      guile_error ("%s: Invalid string value for port configuration `%s' "
                   "(string expected)", server, key);
      return SVZ_ITEM_FAILED;
    }

  /* Check existence of this named port configuration.  */
  if ((port = svz_portcfg_get (str)) == NULL)
    {
      guile_error ("%s: No such port configuration: `%s'", server, str);
      return SVZ_ITEM_FAILED;
    }

  /* Duplicate this port configuration.  */
  *target = svz_portcfg_dup (port);
  return SVZ_ITEM_OK;
}

/* After callback for configuring a server.  */
static int
optionhash_cb_after (char *server, void *arg)
{
  svz_hash_t *options = arg;
  if (0 == optionhash_validate (options, 0, "server", server))
    return SVZ_ITEM_OK;
  return SVZ_ITEM_FAILED;
}

/*
 * Extract a full pipe from an option hash.  Returns zero if it worked.
 */
static int
optionhash_extract_pipe (svz_hash_t *hash,
                         char *key,        /* the key to find      */
                         svz_pipe_t *pipe, /* where to put it      */
                         char *txt)        /* appended to error    */
{
  int err = 0;

  err |= optionhash_validate (hash, 1, "pipe", key);
  err |= optionhash_extract_string (hash, PORTCFG_NAME, 0, NULL,
                                    &(pipe->name), txt);
  err |= optionhash_extract_string (hash, PORTCFG_USER, 1, NULL,
                                    &(pipe->user), txt);
  err |= optionhash_extract_string (hash, PORTCFG_GROUP, 1, NULL,
                                    &(pipe->group), txt);
  err |= optionhash_extract_int (hash, PORTCFG_UID, 1, -1,
                                 (int *) &(pipe->uid), txt);
  err |= optionhash_extract_int (hash, PORTCFG_GID, 1, -1,
                                 (int *) &(pipe->gid), txt);
  err |= optionhash_extract_int (hash, PORTCFG_PERMS, 1, -1,
                                 (int *) &(pipe->perm), txt);
  err |= optionhash_validate (hash, 0, "pipe", key);
  return err;
}

SCM_DEFINE
(guile_config_instantiate,
 "instantiate-config-type!", 3, 1, 0,
 (SCM type, SCM name, SCM instance, SCM opts),
 doc: /***********
Instantiate a configurable type.  The four arguments are: the name of
the configurable @var{type}, the type @var{name} in the configurable
type's domain, the @var{instance} name the newly instantiated type
should get, and the configuration alist for the new instance.
Emit error messages (to stderr).  Return @code{#t} on success,
@code{#f} in case of any error.  */)
{
#define FUNC_NAME s_guile_config_instantiate
  int err = 0;
  char c_type[24], c_name[64], c_instance[64];
  svz_hash_t *options = NULL;
  char action[ACTIONBUFSIZE];
  char ebuf[ACTIONBUFSIZE];

  /* Configure callbacks for the ‘svz_config_type_instantiate’ thing.  */
  svz_config_accessor_t accessor = {
    optionhash_cb_before,   /* before */
    optionhash_cb_integer,  /* integers */
    optionhash_cb_boolean,  /* boolean */
    optionhash_cb_intarray, /* integer arrays */
    optionhash_cb_string,   /* strings */
    optionhash_cb_strarray, /* string arrays */
    optionhash_cb_hash,     /* hashes */
    optionhash_cb_portcfg,  /* port configurations */
    optionhash_cb_after     /* after */
  };

  if (! GI_GET_XREP_MAYBE (c_type, type))
    {
      guile_error ("Invalid configurable type (string expected)");
      FAIL ();
    }
  if (! GI_GET_XREP_MAYBE (c_name, name))
    {
      guile_error ("Invalid type identifier (string expected)");
      FAIL ();
    }
  if (! GI_GET_XREP_MAYBE (c_instance, instance))
    {
      guile_error ("Invalid instance identifier (string expected)");
      FAIL ();
    }

  DEFINING ("%s `%s'", c_type, c_instance);

  /* Extract options if any.  */
  if (!BOUNDP (opts))
    options = optionhash_create ();
  else if (NULL == (options = guile_to_optionhash (opts, action, 0)))
    FAIL ();                    /* Message already emitted.  */

  err = svz_config_type_instantiate (c_type, c_name, c_instance,
                                     options, &accessor,
                                     ACTIONBUFSIZE, ebuf);
  if (err)
    {
      guile_error ("%s", ebuf);
      FAIL ();
    }

 out:
  optionhash_destroy (options);

  guile_global_error |= err;
  return err ? SCM_BOOL_F : SCM_BOOL_T;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_define_server,
 "define-server!", 1, 1, 0,
 (SCM name, SCM args),
 doc: /***********
Define a server.  @var{name} is a (unique) server name of the form
@code{@var{type}-@var{something}}, where @var{type} is the shortname of a
servertype.  @var{args} is the optionhash that is special for the server.
Emit error messages (to stderr).  Return @code{#t} on success, @code{#f}
in case of any error.  */)
{
#define FUNC_NAME s_guile_define_server
  /* Note: This function could now as well be implemented in Scheme.
     [rotty] */
  int err = 0;
  char servername[64], *servertype = NULL, *p = NULL;
  SCM retval;

  GUILE_PRECALL ();

  /* Check if the given server name is valid.  */
  if (! GI_GET_XREP_MAYBE (servername, name))
    {
      guile_error ("%s: Invalid server name (string expected)", FUNC_NAME);
      FAIL ();
    }

  /* Separate server description.  */
  p = servertype = svz_strdup (servername);
  while (*p && *p != '-')
    p++;

  /* Extract server type and sanity check.  */
  if (*p == '-' && *(p + 1) != '\0')
    *p = '\0';
  else
    {
      guile_error ("%s: Not a valid server name: `%s'", FUNC_NAME, servername);
      FAIL ();
    }

  /* Instantiate and configure this server.  */
  retval = guile_config_instantiate (gi_string2scm ("server"),
                                     gi_string2scm (servertype),
                                     name, args);
 out:
  svz_free (servertype);

  return err ? SCM_BOOL_F : retval;
#undef FUNC_NAME
}

/* Validate a network port value.  */
#define GUILE_VALIDATE_PORT(port, name, proto) do {                       \
  if ((port) < 0 || (port) > 65535) {                                     \
    guile_error ("%s: %s port requires a short (0..65535)", proto, name); \
    err = -1; } } while (0)

SCM_DEFINE
(guile_define_port,
 "define-port!", 2, 0, 0,
 (SCM name, SCM args),
 doc: /***********
Define a port configuration.  @var{name} is a (unique) name for the
port configuration.  @var{args} is optionhash for various settings.
Return @code{#t} on success, otherwise @code{#f}.
Emit error messages (to stderr).  */)
{
#define FUNC_NAME s_guile_define_port
  int err = 0;
  svz_portcfg_t *prev = NULL;
  svz_portcfg_t *cfg = svz_portcfg_create ();
  svz_hash_t *options = NULL;
  char portname[64], proto[16];
  SCM protoname;
  char action[ACTIONBUFSIZE];

  GUILE_PRECALL ();

  /* Check validity of first argument.  */
  if (! GI_GET_XREP_MAYBE (portname, name))
    {
      guile_error ("%s: Invalid port configuration name (string expected)",
                   FUNC_NAME);
      FAIL ();
    }

  DEFINING ("port `%s'", portname);

  if (NULL == (options = guile_to_optionhash (args, action, 0)))
    FAIL ();                    /* Message already emitted.  */

  /* Every key defined only once?  */
  if (0 != optionhash_validate (options, 1, "port", portname))
    err = -1;

  /* Find out what protocol this portcfg will be about.  */
  protoname = optionhash_get (options, PORTCFG_PROTO);
  if (! GI_GET_XREP_MAYBE (proto, protoname))
    {
      guile_error ("Port `%s' requires a `" PORTCFG_PROTO "' string field",
                   portname);
      FAIL ();
    }

  /* Is that TCP?  */
  if (!strcmp (proto, PORTCFG_TCP))
    {
      int port;
      cfg->proto = SVZ_PROTO_TCP;
      err |= optionhash_extract_int (options, PORTCFG_PORT, 0, 0,
                                     &port, action);
      GUILE_VALIDATE_PORT (port, "TCP", portname);
      SVZ_CFG_TCP (cfg, port) = port;
      err |= optionhash_extract_int (options, PORTCFG_BACKLOG, 1, 0,
                                     &SVZ_CFG_TCP (cfg, backlog), action);
      err |= optionhash_extract_string (options, PORTCFG_IP, 1,
                                        SVZ_PORTCFG_NOIP,
                                        &SVZ_CFG_TCP (cfg, ipaddr), action);
      err |= optionhash_extract_string (options, PORTCFG_DEVICE, 1, NULL,
                                        &SVZ_CFG_TCP (cfg, device), action);
    }
  /* Maybe UDP?  */
  else if (!strcmp (proto, PORTCFG_UDP))
    {
      int port;
      cfg->proto = SVZ_PROTO_UDP;
      err |= optionhash_extract_int (options, PORTCFG_PORT,
                                     0, 0, &port, action);
      GUILE_VALIDATE_PORT (port, "UDP", portname);
      SVZ_CFG_UDP (cfg, port) = port;
      err |= optionhash_extract_string (options, PORTCFG_IP, 1,
                                        SVZ_PORTCFG_NOIP,
                                        &SVZ_CFG_UDP (cfg, ipaddr), action);
      err |= optionhash_extract_string (options, PORTCFG_DEVICE, 1, NULL,
                                        &SVZ_CFG_UDP (cfg, device), action);
    }
  /* Maybe ICMP?  */
  else if (!strcmp (proto, PORTCFG_ICMP))
    {
      int type;
      cfg->proto = SVZ_PROTO_ICMP;
      err |= optionhash_extract_string (options, PORTCFG_IP, 1,
                                        SVZ_PORTCFG_NOIP,
                                        &SVZ_CFG_ICMP (cfg, ipaddr), action);
      err |= optionhash_extract_string (options, PORTCFG_DEVICE, 1, NULL,
                                        &SVZ_CFG_ICMP (cfg, device), action);
      err |= optionhash_extract_int (options, PORTCFG_TYPE, 1,
                                     SVZ_ICMP_SERVEEZ, &type, action);
      if (type & ~0xff)
        {
          guile_error ("ICMP type `%s' requires a byte (0..255) %s",
                       PORTCFG_TYPE, action);
          err = -1;
        }
      SVZ_CFG_ICMP (cfg, type) = (uint8_t) (type & 0xff);
    }
  /* Maybe RAW?  */
  else if (!strcmp (proto, PORTCFG_RAW))
    {
      cfg->proto = SVZ_PROTO_RAW;
      err |= optionhash_extract_string (options, PORTCFG_IP, 1,
                                        SVZ_PORTCFG_NOIP,
                                        &SVZ_CFG_RAW (cfg, ipaddr), action);
      err |= optionhash_extract_string (options, PORTCFG_DEVICE, 1, NULL,
                                        &SVZ_CFG_RAW (cfg, device), action);
    }
  /* Finally a PIPE?  */
  else if (!strcmp (proto, PORTCFG_PIPE))
    {
      svz_hash_t *poptions;
      SCM p;
      char str[1024];
      cfg->proto = SVZ_PROTO_PIPE;

      /* Handle receiving pipe.  */
      DEFINING ("pipe `%s' in port `%s'", PORTCFG_RECV, portname);

      /* Check if it is a plain string.  */
      p = optionhash_get (options, PORTCFG_RECV);
      if (GI_GET_XREP_MAYBE (str, p))
        {
          svz_pipe_t *r = &SVZ_CFG_PIPE (cfg, recv);

          r->name = svz_strdup (str);
          r->gid = (gid_t) -1;
          r->uid = (uid_t) -1;
          r->perm = (mode_t) -1;
        }
      /* Create local optionhash for receiving pipe direction.  */
      else if (SCM_EQ_P (p, SCM_UNSPECIFIED))
        {
          guile_error ("%s: You have to define a pipe called `%s'",
                       portname, PORTCFG_RECV);
          err = -1;
        }
      else if ((poptions = guile_to_optionhash (p, action, 0)) == NULL)
        {
          err = -1;             /* Message already emitted.  */
        }
      else
        {
          err |= optionhash_extract_pipe (poptions, PORTCFG_RECV,
                                          &SVZ_CFG_PIPE (cfg, recv), action);
          optionhash_destroy (poptions);
        }

      /* Try getting send pipe.  */
      DEFINING ("pipe `%s' in port `%s'", PORTCFG_SEND, portname);

      /* Check plain string.  */
      p = optionhash_get (options, PORTCFG_SEND);
      if (GI_GET_XREP_MAYBE (str, p))
        {
          svz_pipe_t *s = &SVZ_CFG_PIPE (cfg, send);

          s->name = svz_strdup (str);
          s->gid = (gid_t) -1;
          s->uid = (uid_t) -1;
          s->perm = (mode_t) -1;
        }
      else if (SCM_EQ_P (p, SCM_UNSPECIFIED))
        {
          guile_error ("%s: You have to define a pipe called `%s'",
                       portname, PORTCFG_SEND);
          err = -1;
        }
      else if ((poptions = guile_to_optionhash (p, action, 0)) == NULL)
        {
          err = -1;             /* Message already emitted.  */
        }
      else
        {
          err |= optionhash_extract_pipe (poptions, PORTCFG_SEND,
                                          &SVZ_CFG_PIPE (cfg, send), action);
          optionhash_destroy (poptions);
        }
    }
  else
    {
      guile_error ("Invalid `" PORTCFG_PROTO "' field `%s' in port `%s'",
                   proto, portname);
      FAIL ();
    }

  /* Access the send and receive buffer sizes.  */
  err |= optionhash_extract_int (options, PORTCFG_SEND_BUFSIZE, 1, 0,
                                 &(cfg->send_buffer_size), action);
  err |= optionhash_extract_int (options, PORTCFG_RECV_BUFSIZE, 1, 0,
                                 &(cfg->recv_buffer_size), action);

  /* Acquire the connect frequency.  */
  if (cfg->proto & SVZ_PROTO_TCP)
    err |= optionhash_extract_int (options, PORTCFG_FREQ, 1, 0,
                                   &(cfg->connect_freq), action);

  /* Obtain the access lists "allow" and "deny".  */
  if (!(cfg->proto & SVZ_PROTO_PIPE))
    {
      SCM list;

      cfg->deny = NULL;
      list = optionhash_get (options, PORTCFG_DENY);
      if (!SCM_EQ_P (list, SCM_UNSPECIFIED))
        {
          if ((cfg->deny = guile_to_strarray (list, PORTCFG_DENY)) == NULL)
            {
              guile_error ("Failed to parse string array `" PORTCFG_DENY
                           "' in port `%s'", portname);
              err = -1;
            }
        }
      cfg->allow = NULL;
      list = optionhash_get (options, PORTCFG_ALLOW);
      if (!SCM_EQ_P (list, SCM_UNSPECIFIED))
        {
          if ((cfg->allow = guile_to_strarray (list, PORTCFG_ALLOW)) == NULL)
            {
              guile_error ("Failed to parse string array `" PORTCFG_ALLOW
                           "' in port `%s'", portname);
              err = -1;
            }
        }
    }

  /* Check for unused keys in input.  */
  if (0 != optionhash_validate (options, 0, "port", portname))
    FAIL ();                    /* Message already emitted.  */

  /* Now remember the name and add that port configuration.  */
  cfg->name = svz_strdup (portname);
  err |= svz_portcfg_mkaddr (cfg);

  if (err)
    FAIL ();

  if ((prev = svz_portcfg_get (portname)) != NULL)
    {
      /* We've overwritten something.  Report and dispose.  */
      guile_error ("Duplicate definition of port `%s'", portname);
      err = -1;
    }
  else
    svz_portcfg_add (portname, cfg);

 out:
  if (err)
    svz_portcfg_destroy (cfg);
  optionhash_destroy (options);
  guile_global_error |= err;
  return err ? SCM_BOOL_F : SCM_BOOL_T;
#undef FUNC_NAME
}


SCM_DEFINE
(guile_bind_server,
 "bind-server!", 2, 0, 0,
 (SCM port, SCM server),
 doc: /***********
Do generic port to server(s) port binding.
Both @var{port} and @var{server} must be either a string or symbol.
See function @code{svz_server_bind}.  */)
{
#define FUNC_NAME s_guile_bind_server
  char portname[64];
  char servername[64];
  svz_server_t *s;
  svz_portcfg_t *p;
  int err = 0;

  GUILE_PRECALL ();

  /* Check arguments.  */
  if (! STRING_OR_SYMBOL_P (port))
    {
      guile_error ("%s: Port name must be string or symbol", FUNC_NAME);
      FAIL ();
    }
  if (! STRING_OR_SYMBOL_P (server))
    {
      guile_error ("%s: Server name must be string or symbol", FUNC_NAME);
      FAIL ();
    }

  GI_GET_XREP (portname, port);
  GI_GET_XREP (servername, server);

  /* Check id there is such a port configuration.  */
  if ((p = svz_portcfg_get (portname)) == NULL)
    {
      guile_error ("%s: No such port: `%s'", FUNC_NAME, portname);
      err++;
    }

  /* Get one of the servers in the list.  */
  if ((s = svz_server_get (servername)) == NULL)
    {
      guile_error ("%s: No such server: `%s'", FUNC_NAME, servername);
      err++;
    }

  /* Bind a given server instance to a port configuration.  */
  if (s != NULL && p != NULL)
    {
      if (svz_server_bind (s, p) < 0)
        err++;
    }

 out:
  guile_global_error |= err;
  return err ? SCM_BOOL_F : SCM_BOOL_T;
#undef FUNC_NAME
}

/*
 * Converts the given array of strings @var{array} into a guile list.
 */
SCM
guile_strarray_to_guile (svz_array_t *array)
{
  SCM list;
  size_t i;

  /* Check validity of the give string array.  */
  if (array == NULL)
    return SCM_UNDEFINED;

  /* Go through all the strings and add these to a guile list.  */
  for (list = SCM_EOL, i = 0; i < svz_array_size (array); i++)
    list = scm_cons (gi_string2scm ((char *) svz_array_get (array, i)),
                     list);
  return scm_reverse (list);
}

/*
 * Converts the given array of integers @var{array} into a guile list.
 */
SCM
guile_intarray_to_guile (svz_array_t *array)
{
  SCM list;
  size_t i;

  /* Check validity of the give string array.  */
  if (array == NULL)
    return SCM_UNDEFINED;

  /* Go through all the strings and add these to a guile list.  */
  for (list = SCM_EOL, i = 0; i < svz_array_size (array); i++)
    list = scm_cons (gi_integer2scm ((long) svz_array_get (array, i)), list);
  return scm_reverse (list);
}

static void
strhash_acons (void *k, void *v, void *closure)
{
  SCM *alist = closure;

  *alist = scm_acons (gi_string2scm (k),
                      gi_string2scm (v),
                      *alist);
}

/*
 * Converts the given string hash @var{hash} into a guile alist.
 */
SCM
guile_hash_to_guile (svz_hash_t *hash)
{
  SCM alist = SCM_EOL;

  svz_hash_foreach (strhash_acons, hash, &alist);
  return alist;
}

static int
access_interfaces_internal (const svz_interface_t *ifc, void *closure)
{
  SCM *list = closure;
  char buf[64];

  *list = scm_cons (gi_string2scm (SVZ_PP_ADDR (buf, ifc->addr)), *list);
  return 0;
}

SCM_DEFINE
(guile_access_interfaces,
 "serveez-interfaces", 0, 1, 0,
 (SCM args),
 doc: /***********
Make the list of local interfaces accessible to Scheme.  Return the
local interfaces as a list of ip addresses in dotted decimal form.  If
@var{args} are specified, they are added as additional local interfaces.  */)
{
#define FUNC_NAME s_guile_access_interfaces
  size_t n;
  SCM list = SCM_EOL;
  char *str, description[64];
  struct sockaddr_in addr;
  svz_array_t *array;

  GUILE_PRECALL ();

  svz_foreach_interface (access_interfaces_internal, &list);

  /* Is there an argument given to the guile procedure?  */
  if (BOUNDP (args))
    {
      if ((array = guile_to_strarray (args, FUNC_NAME)) != NULL)
        {
          svz_array_foreach (array, str, n)
            {
              if (svz_inet_aton (str, &addr) == -1)
                {
                  guile_error ("%s: IP address in dotted decimals expected",
                               FUNC_NAME);
                  guile_global_error = -1;
                  continue;
                }
              sprintf (description, "guile interface %zu", n);
              svz_interface_add (n, description, AF_INET,
                                 &addr.sin_addr.s_addr, 0);
            }
          svz_array_destroy (array);
        }
    }

  return scm_reverse_x (list, SCM_EOL);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_access_loadpath,
 "serveez-loadpath", 0, 1, 0,
 (SCM args),
 doc: /***********
Make the search path for the Serveez core library accessible to Scheme.
Return a list a each path as previously defined.  If @var{args} is specified,
override the current definition of this load path with it.
The load path is used to tell Serveez where
it can find additional server modules.  */)
{
#define FUNC_NAME s_guile_access_loadpath
  SCM list;
  svz_array_t *paths = svz_dynload_path_get ();

  GUILE_PRECALL ();

  /* Create a guile list containing each search path.  */
  list = guile_strarray_to_guile (paths);
  svz_array_destroy (paths);

  /* Set the load path if argument is given.  */
  if (BOUNDP (args))
    {
      if ((paths = guile_to_strarray (args, FUNC_NAME)) != NULL)
        svz_dynload_path_set (paths);
    }
  return list;
#undef FUNC_NAME
}

/*
 * Create a checker procedure body which evaluates the boolean
 * expression @var{expression}.  The argument for this procedure
 * is a character string which can be uses as @var{str} within
 * the expression.
 */
#define STRING_CHECKER_BODY(expression)         \
  char str[64];                                 \
                                                \
  GUILE_PRECALL ();                             \
  return (GI_GET_XREP_MAYBE (str, arg)          \
          && (expression))                      \
    ? SCM_BOOL_T                                \
    : SCM_BOOL_F

SCM_DEFINE
(guile_check_port,
 "serveez-port?", 1, 0, 0,
 (SCM arg),
 doc: /***********
Return @code{#t} if the given string @var{name} corresponds with a
registered port configuration, otherwise @code{#f}.

-args: (name)  */)
{
#define FUNC_NAME s_guile_check_port
  STRING_CHECKER_BODY (svz_portcfg_get (str) != NULL);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_check_server,
 "serveez-server?", 1, 0, 0,
 (SCM arg),
 doc: /***********
Check whether the given string @var{name} corresponds with an
instantiated server name and return @code{#t} if so.

-args: (name)  */)
{
#define FUNC_NAME s_guile_check_server
  STRING_CHECKER_BODY (svz_server_get (str) != NULL);
#undef FUNC_NAME
}

SCM_DEFINE
(guile_check_stype,
 "serveez-servertype?", 1, 0, 0,
 (SCM arg),
 doc: /***********
Check whether the given string @var{name} is a valid
server type prefix known in Serveez and return @code{#t} if so.
Otherwise return @code{#f}.

-args: (name)  */)
{
#define FUNC_NAME s_guile_check_stype
  STRING_CHECKER_BODY (svz_servertype_get (str, 0) != NULL);
#undef FUNC_NAME
}

static SCM
parm_accessor (char const *who, int param, SCM arg)
{
  SCM value;
  int n;

  GUILE_PRECALL ();
  value = gi_integer2scm (svz_runparm (-1, param));
  if (BOUNDP (arg))
    {
      if (guile_to_integer (arg, &n))
        {
          guile_error ("%s: Invalid integer value", who);
          guile_global_error = -1;
        }
      else
        /* FIXME: Check return value.  */
        svz_runparm (param, n);
    }
  return value;
}

static SCM
string_accessor (char const *who, char **x, SCM arg)
{
  SCM value = gi_string2scm (*x);
  char str[2048];

  GUILE_PRECALL ();

  if (BOUNDP (arg))
    {
      if (! GI_GET_XREP_MAYBE (str, arg))
        {
          guile_error ("%s: Invalid string value", who);
          guile_global_error = -1;
        }
      else
        {
          svz_free (*x);
          *x = svz_strdup (str);
        }
    }
  return value;
}

SCM_DEFINE
(guile_access_verbosity,
 "serveez-verbosity", 0, 1, 0,
 (SCM level),
 doc: /***********
Return the verbosity level (an integer).  Optional
arg @var{level} means set it to that level, instead.  This
setting is overridden by the command-line @samp{-v} option.  */)
{
  return parm_accessor (s_guile_access_verbosity,
                        SVZ_RUNPARM_VERBOSITY,
                        level);
}

SCM_DEFINE
(guile_access_maxsockets,
 "serveez-maxsockets", 0, 1, 0,
 (SCM max),
 doc: /***********
Return the maximum number of open sockets permitted (an integer).
Optional arg @var{max} means set it to that number, instead.
This setting is overridden by the command-line @samp{-m} option.  */)
{
  return parm_accessor (s_guile_access_maxsockets,
                        SVZ_RUNPARM_MAX_SOCKETS,
                        max);
}

#if ENABLE_CONTROL_PROTO
extern char *control_protocol_password;
#else
static char *control_protocol_password;
#endif

SCM_DEFINE
(guile_access_passwd,
 "serveez-passwd", 0, 1, 0,
 (SCM pw),
 doc: /***********
Return the control password (a string).
Optional arg @var{pw} sets it to that, instead.  This effectively
does nothing if the control protocol is not enabled.  */)
{
  return string_accessor
    (s_guile_access_passwd, &control_protocol_password, pw);
}

/*
 * Exception handler for guile.  It is called if the evaluation of the file
 * evaluator or a guile procedure call failed.
 */
static SCM
guile_exception (UNUSED void *data, SCM tag, SCM args)
{
  /* FIXME: current-load-port is not defined in this state.  Why?  */
  char str[64];
  SCM ep = scm_current_error_port ();

  GI_GET_XREP (str, tag);
  guile_error ("Exception due to `%s'", str);

  /* `tag' contains internal exception name */
  scm_puts ("guile-error: ", ep);

  /* on quit/exit */
  if (SCM_NULLP (args))
    {
      scm_display (tag, ep);
      scm_puts ("\n", ep);
      return SCM_BOOL_F;
    }

  if (!SCM_FALSEP (SCM_CAR (args)))
    {
      scm_display (SCM_CAR (args), ep);
      scm_puts (": ", ep);
    }
  args = SCM_CDR (args);
  scm_display_error_message (SCM_CAR (args), SCM_CADR (args), ep);
  return SCM_BOOL_F;
}

/*
 * Initialize Guile.  Make certain variables and procedures defined above
 * available to Guile.
 */
static void
guile_init (void)
{
  load_port_box = scm_permanent_object
    (scm_cons (SCM_BOOL_F, SCM_UNDEFINED));

  /* The default module changed after Guile 1.3.4 from (guile) to
     (guile-user).  Set it back explicitly, until the time when we
     can arrange to define the primitives in (serveez) proper.  */
  gi_eval_string ("(define-module (guile))");

#include "guile.x"

  /* define some variables */
  gi_define ("serveez-version", gi_string2scm (PACKAGE_VERSION));

  {
#include "guile-boot.c"

    gi_eval_string (high);
  }

#if ENABLE_GUILE_SERVER
  guile_server_init ();
#endif /* ENABLE_GUILE_SERVER */
}

/* Wrapper function for the file loader exception handler.  */
static SCM
guile_eval_file (void *data)
{
  char *file = data;
  struct stat buf;
  int error;

  /* Parse configuration from standard input stream.  */
#ifdef __MINGW32__
  error = svz_fstat ((int) GetStdHandle (STD_INPUT_HANDLE), &buf);
#else
  error = svz_fstat (fileno (stdin), &buf);
#endif
  if (file == NULL || (error != -1 && !isatty (fileno (stdin)) &&
                       !S_ISCHR (buf.st_mode) && !S_ISBLK (buf.st_mode)))
    {
      SCM form, inp = scm_current_input_port ();

      while (!SCM_EOF_OBJECT_P (form = scm_read (inp)))
        gi_primitive_eval (form);
      return SCM_BOOL_T;
    }

  /* Load configuration from file.  */
  return gi_primitive_load (file);
}

/*
 * Get server settings from the file @var{cfgfile} and instantiate servers
 * as needed.  Return non-zero on errors.
 */
int
guile_load_config (char *cfgfile)
{
  SCM ret;
  guile_global_error = 0;
  guile_init ();

  ret = scm_internal_catch (SCM_BOOL_T,
                            guile_eval_file, cfgfile,
                            guile_exception, NULL);

  if (SCM_FALSEP (ret))
    guile_global_error = -1;

  /* Kick the garbage collection now since no Guile is involved anymore
     unless a Guile server is implemented.  */
  scm_gc ();

  return guile_global_error ? -1 : 0;
}
