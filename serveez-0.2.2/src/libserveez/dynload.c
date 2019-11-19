/*
 * dynload.c - dynamic server loading implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
#include <stdlib.h>
#include <errno.h>
#if HAVE_DL_H
# include <dl.h>
#endif
#ifdef __BEOS__
# include <kernel/image.h>
#endif
#if HAVE_DLD_H
# include <dld.h>
#endif
#if HAVE_DLFCN_H
# include <dlfcn.h>
# ifndef RTLD_GLOBAL
#  define RTLD_GLOBAL 0
# endif
# ifndef RTLD_NOW
#  define RTLD_NOW 0
# endif
#endif
#if HAVE_MACH_O_DYLD_H
#include <mach-o/dyld.h>
#endif
#include "networking-headers.h"
#include "svzpath.h"
#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/server.h"
#include "libserveez/core.h"
#include "libserveez/dynload.h"
#include "misc-macros.h"

/* Structure holding a shared libraries info.  */
typedef struct
{
  void *handle; /* its handle */
  char *file;   /* the shared libraries filename */
  int ref;      /* reference counter */
}
dyn_library_t;

/* Internal list of shared libraries.  */
static size_t dyn_libraries = 0;
static dyn_library_t *dyn_library = NULL;

/* Print the current shared library error description.  */
#if HAVE_DLOPEN
# define dyn_error() dlerror ()
#elif HAVE_NSADDIMAGE
static char *
dyn_error (void)
{
  NSLinkEditErrors errors;
  int n;
  const char *file, *err;
  NSLinkEditError (&errors, &n, &file, &err);
  return (char *) err;
}
#else
# define dyn_error()  svz_sys_strerror ()
#endif

/* Define library prefix and suffix.  */
#if defined (__MINGW32__) || defined (__CYGWIN__)
# if defined (__CYGWIN__)
#  define DYNLOAD_PREFIX "cyg"
# else
#  define DYNLOAD_PREFIX "lib"
# endif
# define DYNLOAD_SUFFIX "dll"
# define DYNLOAD_PATH_SEPERATOR ';'
# define DYNLOAD_SYMBOL_PREFIX ""
#elif defined (HAVE_NSADDIMAGE)
# define DYNLOAD_PREFIX "lib"
# define DYNLOAD_SUFFIX "dylib"
# define DYNLOAD_PATH_SEPERATOR ':'
# define DYNLOAD_SYMBOL_PREFIX "_"
#elif defined (__hpux)
# define DYNLOAD_PREFIX "lib"
# define DYNLOAD_SUFFIX "sl"
# define DYNLOAD_PATH_SEPERATOR ':'
# define DYNLOAD_SYMBOL_PREFIX ""
#else
# define DYNLOAD_PREFIX "lib"
# define DYNLOAD_SUFFIX "so"
# define DYNLOAD_PATH_SEPERATOR ':'
# define DYNLOAD_SYMBOL_PREFIX ""
#endif

/* Name of the additional search path environment variable.  */
#define DYNLOAD_PATH "SERVEEZ_LOAD_PATH"

/*
 * This functions tries to link a library called @var{file} and returns
 * its handle or NULL if it failed.  If the argument @var{path} is given
 * it prepends this to the file name of the library.
 */
static void *
dyn_get_library (char *path, char *file)
{
  void *handle = NULL;
  char *lib = NULL;

  if (file)
    {
      size_t len = (path ? strlen (path) + 1 : 0) + strlen (file) + 1;

      lib = svz_malloc (len);
      snprintf (lib, len, "%s%s%s",
                path ? path : "",
                path ? "/" : "",
                file);
    }

#if HAVE_DLOPEN
  handle = dlopen (lib, RTLD_NOW | RTLD_GLOBAL);
#elif defined (__BEOS__)
  handle = load_add_on (lib);
  if ((image_id) handle <= 0)
    handle = NULL;
#elif HAVE_DLD_LINK
  handle = dld_link (lib);
#elif defined (__MINGW32__)
  handle = LoadLibrary (lib);
#elif HAVE_SHL_LOAD
  handle = shl_load (lib, BIND_IMMEDIATE | BIND_NONFATAL | DYNAMIC_PATH, 0L);
#elif HAVE_NSADDIMAGE
  handle = (void *) NSAddImage (lib, NSADDIMAGE_OPTION_RETURN_ON_ERROR |
                                NSADDIMAGE_OPTION_WITH_SEARCHING);
#endif

  svz_free (lib);
  return handle;
}

/**
 * Set the additional search paths for the serveez library.  The given array
 * of strings gets @code{svz_free}d.
 */
void
svz_dynload_path_set (svz_array_t *paths)
{
  char *str, *env, *w;
  size_t n, len;

  /* Return here if necessary.  */
  if (paths == NULL)
    return;

  /* Compute total allocation required.  */
  len = sizeof (DYNLOAD_PATH) - 1;
  svz_array_foreach (paths, str, n)
    len += 1 + strlen (str);            /* sep + STR */
  len++;                                /* '\0' */

  /* Create environment variable.  */
  w = env = svz_malloc (len);
  len = sizeof (DYNLOAD_PATH) - 1;
  memcpy (w, DYNLOAD_PATH, len);
  w += len;
  svz_array_foreach (paths, str, n)
    {
      *w++ = n ? DYNLOAD_PATH_SEPERATOR : '=';
      len = strlen (str);
      memcpy (w, str, len);
      w += len;
    }
  *w = '\0';
  svz_array_destroy (paths);

  /* Set environment variable.  */
  if (putenv (svz_pstrdup (env)) < 0)
    svz_log_sys_error ("putenv");
  svz_free (env);
}

/**
 * Create an array of strings containing each an additional search path.
 * The loadpath is hold in the environment variable @samp{SERVEEZ_LOAD_PATH}
 * which can be set from outside the library or modified using
 * @code{svz_dynload_path_set}.  The returned array needs to be destroyed
 * after usage.
 */
svz_array_t *
svz_dynload_path_get (void)
{
  svz_array_t *paths = svz_array_create (1, svz_free);
  char *path, *p, *start, *val;
  size_t len, n;

  /* Add some default paths.  */
  svz_array_add (paths, svz_strdup (SVZ_PKGLIBDIR));
  svz_array_add (paths, svz_strdup (SVZ_PKGDATADIR));

  /* Add environment variable paths.  */
  if ((p = getenv (DYNLOAD_PATH)) != NULL)
    {
      while (*p)
        {
          start = p;
          while (*p && *p != DYNLOAD_PATH_SEPERATOR)
            p++;
          if (p > start)
            {
              len = p - start;
              path = svz_malloc (len + 1);
              memcpy (path, start, len);
              start = path + len;
              *start-- = 0;
              while ((*start == '/' || *start == '\\') && start > path)
                *start-- = 0;

              /* Do not put duplicate paths here.  */
              svz_array_foreach (paths, val, n)
                if (!strcmp (val, path))
                  {
                    svz_free_and_zero (path);
                    break;
                  }
              if (path)
                svz_array_add (paths, path);
            }
          if (*p)
            p++;
        }
    }
  return paths;
}

/*
 * Open the given library @var{file} and put it into the currently load
 * library list.  Return a valid library handle entry on success.
 */
static dyn_library_t *
dyn_load_library (char *file)
{
  size_t n;
  void *handle = NULL;
  svz_array_t *paths;
  char *path;

  /* go through all loaded libraries and check if there is such a */
  for (n = 0; n < dyn_libraries; n++)
    if (!strcmp (dyn_library[n].file, file))
      {
        dyn_library[n].ref++;
        return &dyn_library[n];
      }

  /* try open the library */
  if ((handle = dyn_get_library (NULL, file)) == NULL)
    {
      paths = svz_dynload_path_get ();
      svz_array_foreach (paths, path, n)
        if ((handle = dyn_get_library (path, file)) != NULL)
          break;
      svz_array_destroy (paths);
    }

  if (handle == NULL)
    {
      svz_log (SVZ_LOG_ERROR, "link: unable to locate %s\n", file);
      return NULL;
    }

  /* check if this handle already exists */
  for (n = 0; n < dyn_libraries; n++)
    if (dyn_library[n].handle == handle)
      {
        dyn_library[n].ref++;
        return &dyn_library[n];
      }

  /* add the shared library to the handle list */
  n = dyn_libraries;
  dyn_libraries++;
  dyn_library = svz_realloc (dyn_library, sizeof (dyn_library_t) *
                             dyn_libraries);
  dyn_library[n].file = svz_strdup (file);
  dyn_library[n].handle = handle;
  dyn_library[n].ref = 1;
  return &dyn_library[n];
}

/*
 * Unload a given library @var{lib} if possible.  Return the reference
 * counter or zero if the library has been unloaded.  Return -1 if there is
 * no such library at all or on other errors.
 */
static int
dyn_unload_library (dyn_library_t *lib)
{
  size_t n;
  int err = 0;
  void *handle;

  /* check if this library really exists */
  for (n = 0; n < dyn_libraries; n++)
    if (&dyn_library[n] == lib)
      {
        /* return the remaining reference counter */
        if (--lib->ref > 0)
          return lib->ref;

        /* unload the library */
        handle = lib->handle;
#if HAVE_DLOPEN
        err = dlclose (handle);
#elif defined (__BEOS__)
        err = (unload_add_on ((image_id) handle) != B_OK);
#elif HAVE_DLD_LINK
        err = dld_unlink_by_file (lib->file);
#elif defined (__MINGW32__)
        err = (FreeLibrary (handle) == 0);
#elif HAVE_SHL_LOAD
        err = shl_unload ((shl_t) handle);
#elif HAVE_NSADDIMAGE
        /* TODO: Find out.  This isn't correct...  */
        /* err = (NSUnLinkModule ((void *) handle, 0) == 0); */
#endif
        if (err)
          {
            svz_log (SVZ_LOG_ERROR, "unlink: %s (%s)\n", dyn_error (), lib->file);
            return -1;
          }

        /* rearrange the library structure */
        svz_free (lib->file);
        if (--dyn_libraries > 0)
          {
            *lib = dyn_library[dyn_libraries];
            svz_realloc (dyn_library, sizeof (dyn_library_t) * dyn_libraries);
          }
        else
          {
            svz_free (dyn_library);
            dyn_library = NULL;
          }
        return 0;
      }
  return -1;
}

/*
 * Get a function or data symbol @var{symbol} from the given library
 * @var{lib}.  Return @code{NULL} on errors.
 */
static void *
dyn_load_symbol (dyn_library_t *lib, char *symbol)
{
  size_t n;
  void *address = NULL;

  /* check if there is such a library */
  for (n = 0; n < dyn_libraries; n++)
    if (&dyn_library[n] == lib)
      {
#if HAVE_DLOPEN
        address = dlsym (lib->handle, symbol);
#elif defined (__BEOS__)
        if (get_image_symbol ((image_id) lib->handle, symbol,
                              B_SYMBOL_TYPE_ANY, &address) != B_OK)
          address = NULL;
#elif HAVE_DLD_LINK
        address = dld_get_func (symbol);
#elif defined (__MINGW32__)
        *((FARPROC *) &address) = GetProcAddress (lib->handle, symbol);
#elif HAVE_SHL_LOAD
        if (shl_findsym ((shl_t *) &lib->handle,
                         symbol, TYPE_UNDEFINED, &address) != 0)
          address = NULL;
#elif HAVE_NSADDIMAGE
        address = NSLookupSymbolInImage
          ((struct mach_header *) lib->handle, symbol,
           NSLOOKUPSYMBOLINIMAGE_OPTION_BIND_NOW |
           NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
        if (address != NULL)
          address = NSAddressOfSymbol (address);
#endif
        if (address == NULL)
          {
            svz_log (SVZ_LOG_ERROR, "lookup: %s (%s)\n", dyn_error (), symbol);
          }
        return address;
      }
  return NULL;
}

/*
 * Finalize the shared library interface of the core library.  Unload
 * all libraries no matter if referenced or not.
 */
static void
dynload_finalize (void)
{
  while (dyn_libraries)
    {
      dyn_library[0].ref = 1;
      dyn_unload_library (&dyn_library[0]);
    }
}

/*
 * Initialize the shared library interface of the core library.
 */
static void
dynload_init (void)
{
  if (dyn_libraries)
    dynload_finalize ();
  dyn_libraries = 0;
  dyn_library = NULL;
}

/*
 * Create a file name of a shared library for a given servers
 * descriptive name @var{description}.
 */
static char *
dyn_create_file (char *description)
{
  char *file;

  file = svz_malloc (strlen (description) +
                     strlen (DYNLOAD_PREFIX "." DYNLOAD_SUFFIX) + 1);
  sprintf (file, DYNLOAD_PREFIX "%s." DYNLOAD_SUFFIX, description);
  return file;
}

/*
 * Create a symbol name of server definition for a given servers
 * descriptive name @var{description}.
 */
static char *
dyn_create_symbol (char *description)
{
  char *symbol;

  symbol = svz_malloc (strlen (description) + 1 +
                       strlen (DYNLOAD_SYMBOL_PREFIX "_server_definition"));
  sprintf (symbol, DYNLOAD_SYMBOL_PREFIX "%s_server_definition", description);
  return symbol;
}

/*
 * Load an additional server definition from a shared library.  The given
 * descriptive name @var{description} must be part of the library's name.
 */
svz_servertype_t *
svz_servertype_load (char *description)
{
  char *file, *def;
  dyn_library_t *lib;
  svz_servertype_t *server;

  /* load library */
  file = dyn_create_file (description);
  if ((lib = dyn_load_library (file)) == NULL)
    {
      svz_free (file);
      return NULL;
    }
  svz_free (file);

  /* obtain exported data */
  def = dyn_create_symbol (description);
  if ((server = dyn_load_symbol (lib, def)) == NULL)
    {
      dyn_unload_library (lib);
      svz_free (def);
      return NULL;
    }
  svz_free (def);

  return server;
}


void
svz__dynload_updn (int direction)
{
  (direction
   ? dynload_init
   : dynload_finalize)
    ();
}
