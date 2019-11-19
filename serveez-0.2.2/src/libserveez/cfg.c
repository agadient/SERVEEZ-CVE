/*
 * cfg.c - configuration object functions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2002 Andreas Rottmann <a.rottmann@gmx.at>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
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
#include <string.h>
#include "networking-headers.h"
#include "libserveez/cfg.h"
#include "libserveez/util.h"
#include "libserveez/server.h"
#include "misc-macros.h"

/*
 * Make a plain copy of the given integer array @var{intarray}.  If this
 * value is @code{NULL} no operation is performed and the return value
 * is @code{NULL} too.
 */
static svz_array_t *
intarray_dup (svz_array_t *intarray)
{
  svz_array_t *array = NULL;

  if (intarray)
    {
      array = svz_array_dup (intarray);
    }
  return array;
}

/*
 * Duplicate the given array of strings @var{strarray}.  Return @code{NULL}
 * if @var{strarray} equals @code{NULL}.
 */
static svz_array_t *
strarray_dup (svz_array_t *strarray)
{
  svz_array_t *array = NULL;

  if (strarray)
    {
      array = svz_array_strdup (strarray);
    }
  return array;
}

static void
accumulate (void *k, void *v, void *to)
{
  svz_hash_put (to, k, svz_strdup (v));
}

/*
 * Duplicate the given hash table @var{strhash} assuming it is a hash
 * associating strings with strings.  Return @code{NULL} if @var{strhash} is
 * @code{NULL} too.
 */
static svz_hash_t *
hash_dup (svz_hash_t *strhash)
{
  svz_hash_t *hash = NULL;

  if (strhash)
    {
      hash = svz_hash_create (4, strhash->destroy);
      svz_hash_foreach (accumulate, strhash, hash);
    }
  return hash;
}

/**
 * Release the configuration @var{cfg} of the given configuration
 * prototype @var{prototype}.  If @var{cfg} is @code{NULL}, do nothing.
 */
void
svz_config_free (svz_config_prototype_t *prototype, void *cfg)
{
  int n;
  void **target;

  /* Return here if there nothing to do.  */
  if (prototype == NULL || cfg == NULL)
    return;

  /* Go through the list of configuration items.  */
  for (n = 0; prototype->items[n].type != SVZ_ITEM_END; n++)
    {
      /* Calculate the target address.  */
      target = (void **) ((long) cfg +
                          (long) ((long) prototype->items[n].address -
                                  (long) prototype->start));

      /* Depending on the type of configuration item we need to free
         different data structures.  */
      switch (prototype->items[n].type)
        {
          /* Integer array or array of strings.  */
        case SVZ_ITEM_INTARRAY:
        case SVZ_ITEM_STRARRAY:
          svz_array_destroy (*target);
          break;

          /* Simple character string.  */
        case SVZ_ITEM_STR:
          if (*target)
            svz_free (*target);
          break;

          /* Hash table.  */
        case SVZ_ITEM_HASH:
          svz_hash_destroy (*target);
          break;

          /* Port configuration.  */
        case SVZ_ITEM_PORTCFG:
          if (*target)
            svz_portcfg_destroy (*target);
          break;
        }
    }
  svz_free (cfg);
}

/*
 * Clear each configuration item within the given configuration
 * @var{cfg} of the configuration prototype @var{prototype}.  This
 * function is used by @code{svz_config_instantiate} after copying
 * the default configuration.
 */
static void
clobber (svz_config_prototype_t *prototype, void *cfg)
{
  int n;
  void **target;

  /* Return here if there nothing to do.  */
  if (prototype == NULL || cfg == NULL)
    return;

  /* Go through the list of configuration items.  */
  for (n = 0; prototype->items[n].type != SVZ_ITEM_END; n++)
    {
      /* Calculate the target address.  */
      target = (void **) ((long) cfg +
                          (long) prototype->items[n].address -
                          (long) prototype->start);

      /* Clobber only configuration items which are pointers.  */
      if (prototype->items[n].type == SVZ_ITEM_INTARRAY ||
          prototype->items[n].type == SVZ_ITEM_STR ||
          prototype->items[n].type == SVZ_ITEM_STRARRAY ||
          prototype->items[n].type == SVZ_ITEM_HASH ||
          prototype->items[n].type == SVZ_ITEM_PORTCFG)
        {
          *target = NULL;
        }
    }
}

/*
 * This functions is used to instantiate the configuration prototype
 * @var{prototype} using the accessor callbacks @var{accessor}
 * depending on the type of items in the @var{prototype}.  The
 * additional argument @var{arg} is passed to each of these callbacks.
 * The @var{name} argument should be used to pass the instance name of
 * the object which is going to be configured.
 */
void *
svz_config_instantiate (svz_config_prototype_t *prototype, char *name,
                        void *arg, svz_config_accessor_t *accessor)
{
  int e, n, error = 0;
  int hasdef;
  void *cfg = NULL, *def, *target = NULL;
  unsigned long offset;

  /* Run the 'before' callback first.  */
  if (accessor && accessor->before)
    if (SVZ_ITEM_OK != accessor->before (name, arg))
      return NULL;

  /* Make a simple copy of the example configuration structure definition
     for that prototype instance.  */
  if (prototype->size == 0)
    goto out;
  cfg = svz_malloc (prototype->size);
  memcpy (cfg, prototype->start, prototype->size);

  /* Clear all prototype configuration items which are pointers.  Thus we
     are able to reverse the changes below.  */
  clobber (prototype, cfg);

  /* Go through list of configuration items.  */
  for (n = 0; prototype->items[n].type != SVZ_ITEM_END; n++)
    {
      /* Calculate the target address.  */
      offset = (char *) prototype->items[n].address -
        (char *) prototype->start;
      hasdef = prototype->items[n].defaultable;
      def = prototype->items[n].address;
      target = (char *) cfg + offset;
      e = SVZ_ITEM_DEFAULT_ERRMSG;

      /* Check the address of the target.  */
      if ((unsigned long) target < (unsigned long) cfg ||
          (unsigned long) target >= ((unsigned long) cfg +
                                     (unsigned long) prototype->size))
        {
          svz_log (SVZ_LOG_FATAL, "%s: invalid target address for %s `%s'\n",
                   prototype->description,
                   SVZ_ITEM_TEXT (prototype->items[n].type),
                   prototype->items[n].name);
          error = -1;
          continue;
        }

      /* Depending on the type of configuration item we need at this
         point we call the given callbacks and check their return values.  */
      switch (prototype->items[n].type)
        {
          /* Integer value.  */
        case SVZ_ITEM_INT:
          if (accessor && accessor->integer)
            e = accessor->integer (name, arg, prototype->items[n].name,
                                   (int *) target, hasdef, *(int *) def);
          break;

          /* Boolean value.  */
        case SVZ_ITEM_BOOL:
          if (accessor && accessor->boolean)
            e = accessor->boolean (name, arg, prototype->items[n].name,
                                   (int *) target, hasdef, *(int *) def);
          break;

          /* Integer array.  */
        case SVZ_ITEM_INTARRAY:
          if (accessor && accessor->intarray)
            e = accessor->intarray (name, arg, prototype->items[n].name,
                                    (svz_array_t **) target, hasdef,
                                    *(svz_array_t **) def);
          break;

          /* Simple string.  */
        case SVZ_ITEM_STR:
          if (accessor && accessor->string)
            e = accessor->string (name, arg, prototype->items[n].name,
                                  (char **) target, hasdef, *(char **) def);
          break;

          /* Array of strings.  */
        case SVZ_ITEM_STRARRAY:
          if (accessor && accessor->strarray)
            e = accessor->strarray (name, arg, prototype->items[n].name,
                                    (svz_array_t **) target, hasdef,
                                    *(svz_array_t **) def);
          break;

          /* Hash table.  */
        case SVZ_ITEM_HASH:
          if (accessor && accessor->hash)
            e = accessor->hash (name, arg, prototype->items[n].name,
                                (svz_hash_t **) target, hasdef,
                                *(svz_hash_t **) def);
          break;

          /* Port configuration.  */
        case SVZ_ITEM_PORTCFG:
          if (accessor && accessor->portcfg)
            e = accessor->portcfg (name, arg, prototype->items[n].name,
                                   (svz_portcfg_t **) target, hasdef,
                                   *(svz_portcfg_t **) def);
          break;

          /* Unknown configuration item.  */
        default:
          svz_log (SVZ_LOG_FATAL,
                   "inconsistent SVZ_ITEM_ data in prototype `%s'\n",
                   prototype->description);
          error = -1;
          e = -1; /* special */
        }

      /* Check the return value of the accessor functions.  */
      switch (e)
        {
          /* Special case: skip.  */
        case -1:
          break;
          /* Successfully accessed.  */
        case SVZ_ITEM_OK:
          break;
          /* Use the default value, if any.  */
        case SVZ_ITEM_DEFAULT:
        case SVZ_ITEM_DEFAULT_ERRMSG:
          /* Target not accessed.  Defaultable?  */
          if (!prototype->items[n].defaultable)
            {
              if (SVZ_ITEM_DEFAULT_ERRMSG == e)
                svz_log (SVZ_LOG_ERROR,
                         "`%s' lacks a default %s for `%s' in `%s'\n",
                         prototype->description,
                         SVZ_ITEM_TEXT (prototype->items[n].type),
                         prototype->items[n].name, name);
              error = -1;
              break;
            }
          /* Go on, using default values.  */
          switch (prototype->items[n].type)
            {
            case SVZ_ITEM_INT: /* Normal integer.  */
              *(int *) target = *(int *) def;
              break;

            case SVZ_ITEM_BOOL: /* Boolean value.  */
              *(int *) target = *(int *) def;
              break;

            case SVZ_ITEM_INTARRAY: /* Integer array.  */
              *(svz_array_t **) target =
                intarray_dup (*(svz_array_t **) def);
              break;

            case SVZ_ITEM_STR: /* Character string.  */
              *(char **) target = (char *) svz_strdup (*(char **) def);
              break;

            case SVZ_ITEM_STRARRAY: /* Array of strings.  */
              *(svz_array_t **) target =
                strarray_dup (*(svz_array_t **) def);
              break;

            case SVZ_ITEM_HASH: /* Hash table.  */
              *(svz_hash_t **) target =
                hash_dup (*(svz_hash_t **) def);
              break;

            case SVZ_ITEM_PORTCFG: /* Port configuration.  */
              *(svz_portcfg_t **) target =
                svz_portcfg_dup (*(svz_portcfg_t **) def);
              break;
            }
          break;

          /* Configuring failed.  Skip error messages.  */
        case SVZ_ITEM_FAILED:
          error = -1;
          break;

          /* Configuring failed.  Print error messages.  */
        case SVZ_ITEM_FAILED_ERRMSG:
          svz_log (SVZ_LOG_ERROR,
                   "invalid %s value for `%s' in `%s'\n",
                   SVZ_ITEM_TEXT (prototype->items[n].type),
                   prototype->items[n].name, name);
          error = -1;
          break;

          /* Special case: Accessor callback invalid.  */
        default:
          svz_log (SVZ_LOG_FATAL,
                   "invalid SVZ_ITEM_ value (%d) returned by %s "
                   "callback for `%s'\n",
                   e, SVZ_ITEM_TEXT (prototype->items[n].type),
                   prototype->items[n].name);
          error = -1;
        }
    }

 out:
  /* Run the 'after' callback last.  */
  if (accessor && accessor->after)
    if (SVZ_ITEM_OK != accessor->after (name, arg))
      error = -1;

  /* Release memory reserved for configuration on errors.  This means
     to reverse the above changes.  */
  if (error)
    {
      svz_config_free (prototype, cfg);
      cfg = NULL;
    }

  return cfg;
}

static svz_hash_t *config_types = NULL;

/*
 * Add the configurable type described by @var{type} to the list of
 * known configurable types.
 */
static void
add_type (svz_config_type_t *type)
{
  svz_hash_put (config_types, type->name, type);
}

/**
 * Instantiate a configurable type.  The @var{type} argument specifies
 * the configurable type name, @var{name} the name of the type (in the
 * domain of the configurable type) and @var{instance} the instance
 * name of the type.  Return zero on success, otherwise -1.
 */
int
svz_config_type_instantiate (char *type, char *name, char *instance,
                             void *options, svz_config_accessor_t *accessor,
                             size_t ebufsz, char *ebuf)
{
  svz_config_type_t *cfgtype;

  cfgtype = svz_hash_get (config_types, type);
  if (cfgtype == NULL)
    {
      snprintf (ebuf, ebufsz, "No such configurable type `%s'", type);
      return -1;
    }
  return cfgtype->instantiate (name, instance, options, accessor,
                               ebufsz, ebuf);
}

/*
 * Adds the configurable types of Serveez.
 */
static void
type_init (void)
{
  config_types = svz_hash_create (4, NULL);

  /* add the configurable types of Serveez */
  add_type (&svz_servertype_definition);
}

/*
 * Removes the list of known configurable types.
 */
static void
type_finalize (void)
{
  svz_hash_destroy (config_types);
}

/**
 * Create a collection of @var{type}, given the @var{count}
 * items of @var{data}.  Valid values of @var{type} are one of:
 * @code{SVZ_INTARRAY}, @code{SVZ_STRARRAY}, @code{SVZ_STRHASH}.
 * For a string hash, @var{data} should be alternating keys and values;
 * the returned hash table will have @code{@var{count} / 2} elements.
 * The C type of @var{data} for an int array should be @code{int[]},
 * and for string array or hash it should be @code{char*[]}.
 * On error (either bad @var{type} or odd @var{count} for string hash),
 * return @code{NULL}.
 */
void *
svz_collect (int type, size_t count, void *data)
{
  switch (type)
    {
    case SVZ_INTARRAY:
      {
        svz_array_t *array = svz_array_create (1, NULL);
        int *from = data;

        while (count--)
          svz_array_add (array, SVZ_NUM2PTR (*from++));
        return array;
      }

    case SVZ_STRARRAY:
      {
        svz_array_t *array = svz_array_create (1, svz_free);
        char **from = data;

        while (count--)
          svz_array_add (array, svz_strdup (*from++));
        return array;
      }

    case SVZ_STRHASH:
      if (count & 1)                    /* pebkac */
        return NULL;
      count /= 2;
      {
        svz_hash_t *hash = svz_hash_create (4, svz_free);
        char **from = data;

        while (count--)
          {
            char *k = *from++;
            char *v = *from++;

            svz_hash_put (hash, k, svz_strdup (v));
          }
        return hash;
      }

    default:
      return NULL;
    }
}


void
svz__config_type_updn (int direction)
{
  (direction
   ? type_init
   : type_finalize)
    ();
}
