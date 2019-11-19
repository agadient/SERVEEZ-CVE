/*
 * option.c - getopt function implementation
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
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

#ifdef HAVE_CRYPT_H
# include <crypt.h>
#endif

#include "networking-headers.h"
#include "cfgfile.h"
#include "libserveez.h"
#include "option.h"
#include "unused.h"

#ifndef HAVE_GETOPT
/*
 * Lousy implementation of @code{getopt}.
 * Only good for parsing simple short option command lines on
 * stupid systems like Win32.  No error checking !
 */
char *optarg = NULL;
int optind = 1;
int opterr = 1;
int optopt = 0;

int
getopt (int argc, char * const argv[], const char *optstring)
{
  static int current_arg = 1, current_opt = 0, current_idx = 1;
  int n;
  char *prog = argv[0] + strlen (argv[0]);

  /* parse the programs name */
  while (prog > argv[0] && (*prog != '/' || *prog != '\\'))
    prog--;
  if (*prog == '/' || *prog == '\\')
    prog++;

  while (current_arg < argc)
    {
      if (argv[current_arg][0] == '-')
        {
          if (current_opt == 0)
            current_opt = 1;
          while (argv[current_arg][current_opt] != '\0')
            {
              n = 0;
              /* go through all option characters */
              while (optstring[n])
                {
                  if (optstring[n] == argv[current_arg][current_opt])
                    {
                      current_opt++;
                      if (optstring[n + 1] == ':')
                        {
                          optarg = argv[current_arg + current_idx];
                          current_idx++;
                          if (opterr && optarg == NULL)
                            fprintf (stderr,
                                     "%s: option requires an argument -- %c\n",
                                     prog, optstring[n]);
                        }
                      else
                        optarg = NULL;
                      if (argv[current_arg][current_opt] == '\0')
                        {
                          current_arg += current_idx;
                          current_opt = 0;
                          current_idx = 1;
                        }
                      optind = current_arg + current_idx - 1;
                      return optstring[n];
                    }
                  n++;
                }
              optopt = argv[current_arg][current_opt];
              if (opterr)
                fprintf (stderr, "%s: invalid option -- %c\n", prog, optopt);
              return '?';
            }
          current_opt++;
        }
      current_arg++;
      current_idx = 1;
    }

  current_arg = 1;
  current_opt = 0;
  current_idx = 1;
  return EOF;
}
#endif /* not HAVE_GETOPT */

/*
 * Print program version.
 */
static void
version (void)
{
  fprintf (stdout, "serveez (%s) %s\n"
           "Copyright (C) 2011-2013 Thien-Thi Nguyen\n"
           "Copyright (C) 2000-2009 Martin Grabmueller,"
           " Stefan Jahn, Raimund Jacob, Andreas Rottmann\n"
           "License GPLv3+: GNU GPL version 3 or later"
           " <http://gnu.org/licenses/gpl.html>\n"
           "This is free software: you are free"
           " to change and redistribute it.\n"
           "There is NO WARRANTY, to the extent permitted by law.\n",
           PACKAGE_STRING, PROGRAM_VERSION);
}

#ifdef HAVE_GETOPT_LONG
#define LONGOPTS  1

static struct option serveez_options[];

static const char *
longname (char letter)
{
  struct option *o = serveez_options;

  while (o->val != letter)
    o++;

  return o->name;
}

#else  /* !defined HAVE_GETOPT_LONG */
#define LONGOPTS  0
#endif  /* !deifned HAVE_GETOPT_LONG */

#define AB(blurb,uri)    blurb ": <" uri ">\n"
#define GNU(blurb,rest)  AB (blurb, "http://www.gnu.org/" rest)

#define BUGME                                           \
  ("\n"                                                 \
   AB ("Report bugs to", PACKAGE_BUGREPORT)             \
   GNU ("Serveez home page", "software/serveez/")       \
   GNU ("General help using GNU software", "gethelp/"))

/*
 * Display program command line options.
 * Then @code{exit} with @var{exitval}.
 */
static void
usage (int exitval)
{
  struct human {
    char letter;
    char *arg;
    char *description;
  } all[] = {
    {'h', NULL, "display this help and exit"},
    {'V', NULL, "display version information and exit"},
    {'L', NULL, "display builtin servers and exit"},
    {'i', NULL, "list local network interfaces and exit"},
    {'f', "FILENAME", "file to use as configuration file (serveez.cfg)"},
    {'v', "LEVEL", "set level of verbosity"},
    {'l', "FILENAME", "use FILENAME for logging (default is stderr)"},
#if ENABLE_CONTROL_PROTO
    {'P', "STRING", "set the password for control connections"},
#endif
    {'m', "COUNT", "set the max. number of socket descriptors"},
    {'d', NULL, "start as daemon in background"},
    {'c', NULL, "use standard input as configuration file"},
    {'s', NULL, "don't start any coservers"}
  };
  int const count = sizeof (all) / sizeof (struct human);
  int width = 0;
  int one;
  struct human *h;

  for (h = all; h < all + count; h++)
    {
      one = (LONGOPTS ? strlen (longname (h->letter)) : 0)
        + (h->arg ? strlen (h->arg) : 0);
      if (width < one)
        width = one;
    }
  width += 3;

  printf ("Usage: serveez [OPTION...]\n\n");
  for (h = all; h < all + count; h++)
    {
      one = 0;
      printf ("  -%c", h->letter);
      if (LONGOPTS)
        {
          const char *name = longname (h->letter);

          printf (", --%s", name);
          one += strlen (name);
        }
      if (h->arg)
        {
          /* NB: This doesn't handle the ‘optional_argument’ case,
             which should be displayed as "[=ARG]" or " [ARG]".  */
          printf ("%c%s",
                  LONGOPTS ? '=' : ' ',
                  h->arg);
          one += 1 + strlen (h->arg);
        }
      printf ("%*s%s\n", width - one, "", h->description);
    }
  fprintf (stdout, "%s", BUGME);

  exit (exitval);
}

#if HAVE_GETOPT_LONG
/*
 * Argument array for ‘getopt_long’ system call.
 */
static struct option serveez_options[] = {
  {"help", no_argument, NULL, 'h'},
  {"version", no_argument, NULL, 'V'},
  {"list-servers", no_argument, NULL, 'L'},
  {"iflist", no_argument, NULL, 'i'},
  {"daemon", no_argument, NULL, 'd'},
  {"stdin", no_argument, NULL, 'c'},
  {"verbose", required_argument, NULL, 'v'},
  {"cfg-file", required_argument, NULL, 'f'},
  {"log-file", required_argument, NULL, 'l'},
#if ENABLE_CONTROL_PROTO
  {"password", required_argument, NULL, 'P'},
#endif
  {"max-sockets", required_argument, NULL, 'm'},
  {"solitary", no_argument, NULL, 's'},
  {NULL, 0, NULL, 0}
};
#endif /* HAVE_GETOPT_LONG */

#if ENABLE_CONTROL_PROTO
#define SERVEEZ_OPTIONS "l:hVLiv:f:P:m:dcs"
#else
#define SERVEEZ_OPTIONS "l:hVLiv:f:m:dcs"
#endif

static int
display_ifc (const svz_interface_t *ifc, UNUSED void *closure)
{
  char addr[64];

  SVZ_PP_ADDR (addr, ifc->addr);
  if (ifc->description)
    /* interface with description */
    printf ("%40s: %s\n",
            ifc->description, addr);
  else
    /* interface with interface # only */
    printf ("%31s%09zu: %s\n",
            "interface # ", ifc->index, addr);
  return 0;
}

/*
 * Parse the command line options.  If these have been correct the function
 * either terminates the program successfully or returns an option
 * structure containing information about the command line arguments or it
 * exits the program failurefully if the command line has been wrong.
 */
option_t *
handle_options (int argc, char **argv)
{
  static option_t options;
  static char *cfgfile = "serveez.cfg";
  int arg;
#if HAVE_GETOPT_LONG
  int index;
#endif

  /* initialize command line options */
  options.logfile = NULL;
  options.cfgfile = cfgfile;
  options.verbosity = -1;
  options.sockets = -1;
#if ENABLE_CONTROL_PROTO
  options.pass = NULL;
#endif
  options.daemon = 0;
  options.loghandle = NULL;
  options.coservers = 1;

  /* go through the command line itself */
#if HAVE_GETOPT_LONG
  while ((arg = getopt_long (argc, argv, SERVEEZ_OPTIONS, serveez_options,
                             &index)) != EOF)
#else
  while ((arg = getopt (argc, argv, SERVEEZ_OPTIONS)) != EOF)
#endif
    {
      switch (arg)
        {
        case 'h':
          usage (EXIT_SUCCESS);
          break;

        case 'V':
          version ();
          exit (EXIT_SUCCESS);
          break;

        case 'L':
          print_available_servers ();
          exit (EXIT_SUCCESS);
          break;

        case 'i':
          printf ("--- list of local interfaces"
                  " you can start ip services on ---\n");
          svz_foreach_interface (display_ifc, NULL);
          exit (EXIT_SUCCESS);
          break;

        case 'c':
          if (options.cfgfile != cfgfile)
            usage (EXIT_FAILURE);
          options.cfgfile = NULL;
          break;

        case 'f':
          if (!optarg || options.cfgfile == NULL)
            usage (EXIT_FAILURE);
          options.cfgfile = optarg;
          break;

        case 'v':
          if (optarg)
            {
              options.verbosity = atoi (optarg);
              if (options.verbosity < SVZ_LOG_FATAL)
                options.verbosity = SVZ_LOG_FATAL;
              else if (options.verbosity > SVZ_LOG_DEBUG)
                options.verbosity = SVZ_LOG_DEBUG;
            }
          else
            options.verbosity = SVZ_LOG_DEBUG;
          break;

        case 'l':
          if (!optarg)
            usage (EXIT_FAILURE);
          options.logfile = optarg;
          break;

#if ENABLE_CONTROL_PROTO
        case 'P':
          if (!optarg || strlen (optarg) < 2)
            usage (EXIT_FAILURE);
#if defined HAVE_CRYPT
          options.pass = svz_strdup (crypt (optarg, optarg));
#else
          options.pass = svz_strdup (optarg);
#endif
          break;
#endif  /* ENABLE_CONTROL_PROTO */

        case 'm':
          if (!optarg)
            usage (EXIT_FAILURE);
          options.sockets = atoi (optarg);
          break;

        case 'd':
          options.daemon = 1;
          break;

        case 's':
          options.coservers = -1;
          break;

        default:
          usage (EXIT_FAILURE);
        }
    }

  return &options;
}
