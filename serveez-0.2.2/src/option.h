/*
 * option.h - getopt function interface
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __OPTION_H__
#define __OPTION_H__

#if HAVE_GETOPT_H
# include <getopt.h>
#elif HAVE_UNISTD_H
/* FreeBSD and probably all commercial Un*ces define ‘getopt’
   in this specific file */
# include <unistd.h>
#endif

/*
 * The following structure contains all command line options which might
 * override the settings from the configuration file.
 */
typedef struct
{
  char *logfile;   /* logging file */
  FILE *loghandle; /* logging file handle */
  char *cfgfile;   /* configuration file */
  int verbosity;   /* verbosity level */
  int sockets;     /* maximum amount of open files (sockets) */
#if ENABLE_CONTROL_PROTO
  char *pass;      /* password */
#endif
  int daemon;      /* start as daemon or not */
  int coservers;   /* 1: start coserver instances; -1: do not */
}
option_t;

/*
 * Defining here the struct and #define's for ‘getopt_long’ if it
 * is in libiberty.a but could not be found in getopt.h
 */
#if defined (HAVE_GETOPT_LONG) && !HAVE_DECL_GETOPT_LONG

extern char *optarg;

struct option
{
  const char *name;
  int has_arg;
  int *flag;
  int val;
};

#define no_argument       0
#define required_argument 1
#define optional_argument 2

extern int getopt_long (int argc,
                        char * const argv[],
                        const char *optstring,
                        const struct option *longopts,
                        int *longindex);

#endif  /* defined (HAVE_GETOPT_LONG) && !HAVE_DECL_GETOPT_LONG */

#ifndef HAVE_GETOPT

int getopt (int argc, char * const argv[], const char *optstring);
extern char *optarg;
extern int optind;
extern int opterr;
extern int optopt;

#endif /* not HAVE_GETOPT */

option_t *handle_options (int argc, char **argv);

#endif /* __OPTION_H__ */
