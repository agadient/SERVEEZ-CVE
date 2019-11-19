/*
 * serveez.c - main module
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <libguile.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#include "networking-headers.h"
#include "libserveez.h"
#include "misc-macros.h"
#include "cfgfile.h"
#include "option.h"
#include "guile-api.h"
#include "guile.h"
#include "guile-server.h"
#include "unused.h"

/* Command line option structure.  */
option_t *options = NULL;

/* This is for ‘serveez-nuke’ to set.  */
int global_exit_value;

/* Our private launch pad.  */
void
guile_launch_pad (void *closure, int argc, char **argv)
{
  void (* entry) (int, char **) = (void (*) (int, char **)) closure;

  global_exit_value = EXIT_SUCCESS;
  entry (argc, argv);
  exit (global_exit_value);
}

#if ENABLE_CONTROL_PROTO
extern char *control_protocol_password;
#endif

/*
 * This is the entry point for the guile interface.
 */
static void
guile_entry (UNUSED int argc, UNUSED char **argv)
{
  /* Detect operating system.  */
  svz_log (SVZ_LOG_NOTICE, "%s\n", svz_sys_version ());

  /* Start loading the configuration file.  */
  if (guile_load_config (options->cfgfile) == -1)
    {
      svz_log (SVZ_LOG_ERROR, "error loading config file\n");
      exit (3);
    }

  /*
   * Make command line arguments overriding the configuration
   * file settings.
   */
  if (options->verbosity != -1)
    SVZ_RUNPARM_X (VERBOSITY, options->verbosity);

  if (options->sockets != -1)
    SVZ_RUNPARM_X (MAX_SOCKETS, options->sockets);

#if ENABLE_CONTROL_PROTO
  if (options->pass)
    {
      svz_free (control_protocol_password);
      /* Transfer: set destination and reset source.  */
      control_protocol_password = options->pass;
      options->pass = NULL;
    }
#endif

#if ENABLE_DEBUG
  svz_log (SVZ_LOG_NOTICE, "serveez starting, debugging enabled\n");
#endif /* ENABLE_DEBUG */

  svz_openfiles (SVZ_RUNPARM (MAX_SOCKETS));
  svz_log (SVZ_LOG_NOTICE, "using %d socket descriptors\n",
           SVZ_RUNPARM (MAX_SOCKETS));

  /* Startup the internal coservers here.  */
  if (svz_updn_all_coservers (options->coservers) == -1)
    {
      exit (4);
    }

  /* Initialize server instances.  */
  if (svz_updn_all_servers (1) == -1)
    {
      exit (6);
    }

  svz_loop ();

  /* Run the finalizers.  */
  svz_updn_all_servers (0);

  /* Disconnect the previously invoked internal coservers.  */
  svz_log (SVZ_LOG_NOTICE, "destroying internal coservers\n");
  svz_updn_all_coservers (0);

#if ENABLE_GUILE_SERVER
  guile_server_finalize ();
#endif /* ENABLE_GUILE_SERVER */

#if ENABLE_CONTROL_PROTO
  svz_free_and_zero (control_protocol_password);
#endif
  svz_halt ();

#if ENABLE_DEBUG
  {
    size_t cur[2];

    svz_get_curalloc (cur);
    svz_log (SVZ_LOG_DEBUG, "%d byte(s) of memory in %d block(s) wasted\n",
             cur[0], cur[1]);
  }

#if DEBUG_MEMORY_LEAKS
  svz_heap ();
#endif
#endif /* ENABLE_DEBUG */

#ifdef __MINGW32__
  if (options->daemon)
    {
      svz_windoze_daemon_control (NULL);
    }
#endif

  svz_log (SVZ_LOG_NOTICE, "serveez terminating\n");

  /* FIXME: Serveez leaks because of a open logfile handle.  */
  if (options->loghandle != stderr)
    svz_fclose (options->loghandle);
}

#if 0
static int
dump_servertype (const svz_servertype_t *stype, void *closure)
{
  int *num = closure;
  svz_config_prototype_t *prototype = &stype->config_prototype;
  int i;

  printf ("[%d] - %s\n"
          "\t`detect_proto' at %p\n"
          "\t`connect_socket' at %p\n",
          (*num)++, stype->description,
          (void *) stype->detect_proto,
          (void *) stype->connect_socket);

  if (prototype->start != NULL)
    {
      printf ("  configuration prototype %s (%d byte at %p): \n",
              prototype->description, prototype->size, prototype->start);

      for (i = 0; prototype->items[i].type != SVZ_ITEM_END; i++)
        {
          long offset = (char *) prototype->items[i].address -
            (char *) prototype->start;

          printf ("   variable `%s' at offset %ld, %sdefaultable: ",
                  prototype->items[i].name, offset,
                  prototype->items[i].defaultable ? "" : "not ");

          switch (prototype->items[i].type)
            {
            case SVZ_ITEM_BOOL:
              printf ("bool\n");
              break;
            case SVZ_ITEM_INT:
              printf ("int\n");
              break;
            case SVZ_ITEM_INTARRAY:
              printf ("int array\n");
              break;
            case SVZ_ITEM_STR:
              printf ("string\n");
              break;
            case SVZ_ITEM_STRARRAY:
              printf ("string array\n");
              break;
            case SVZ_ITEM_HASH:
              printf ("hash\n");
              break;
            case SVZ_ITEM_PORTCFG:
              printf ("port configuration\n");
              break;
            default:
              printf ("invalid\n");
            }
        }
    }
  else
    {
      printf ("  no configuration option\n");
    }
  return 0;
}
#endif

/*
 * Main entry point.
 */
int
main (int argc, char *argv[])
{
  /* Initialize the the core library.  */
  svz_boot (argv[0]);
  svz_envblock_setup ();

  /* Handle command line arguments.  */
  options = handle_options (argc, argv);

  /* Send all logging messages to the log handle.  */
  if (options->logfile && options->logfile[0])
    options->loghandle = svz_fopen (options->logfile, "w");
  if (!options->loghandle)
    options->loghandle = stderr;
  svz_log_setfile (options->loghandle);

  /* Setup verbosity once.  */
  if (options->verbosity != -1)
    SVZ_RUNPARM_X (VERBOSITY, options->verbosity);

  /* Start as daemon, not as foreground application.  */
  if (options->daemon)
    {
#ifndef __MINGW32__
      int pid;

      if ((pid = fork ()) == -1)
        {
          svz_log_sys_error ("fork");
          exit (EXIT_FAILURE);
        }
      else if (pid != 0)
        {
          exit (EXIT_SUCCESS);
        }
      /* Close the log file if necessary.  */
      if (options->loghandle == stderr)
        svz_log_setfile (NULL);
      /* Close stdin, stdout and stderr.  */
      if (isatty (fileno (stdin)))
        close (fileno (stdin));
      if (isatty (fileno (stdout)))
        close (fileno (stdout));
      if (isatty (fileno (stderr)))
        close (fileno (stderr));
#else /* __MINGW32__ */
      if (svz_windoze_daemon_control (argv[0]) == -1)
        exit (EXIT_FAILURE);
      if (options->loghandle == stderr)
        svz_log_setfile (NULL);
      svz_closehandle (GetStdHandle (STD_INPUT_HANDLE));
      svz_closehandle (GetStdHandle (STD_OUTPUT_HANDLE));
      svz_closehandle (GetStdHandle (STD_ERROR_HANDLE));
#endif /* __MINGW32__ */
    }

  /* Initialize the static server types.  */
  init_server_definitions ();
#if 0
  {
    int num = 0;

    svz_foreach_servertype (dump_servertype, &num);
  }
#endif

  /* Enter the main guile function.  */
  scm_boot_guile (argc, argv, guile_launch_pad, (void *) guile_entry);
  /* Never reached.  */
  return 0;
}
