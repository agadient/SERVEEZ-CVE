/* btdt.c --- "been there done that" low-level (C language) test support
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
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
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#if HAVE_FLOSS_H
# include <floss.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
# include <netdb.h>
#else
# define sleep(x) Sleep ((x) * 1000)
# include <io.h>
#endif

#include "networking-headers.h"
#include "o-binary.h"
#include <libserveez.h>
#include "misc-macros.h"

int verbosep;


/*
 * utility functions
 */

/* Check that there are at least N args.  If not,
   display "missing args" message and exit failurefully.  */
void
check_nargs (int argc, int n, char const *need)
{
  if (n >= argc)
    {
      fprintf (stderr, "ERROR: missing args: %s\n", need);
      exit (EXIT_FAILURE);
    }
}

/* Initialize test suite.  */
void
test_init (void)
{
  srand (time (NULL));
}

/* Return a random string.  */
char *
test_string (void)
{
  static char text[0x101];
  int length = (rand () & 0xff) + 1;

  text[length] = '\0';
  while (length--)
    {
      text[length] = (rand () % (128 - ' ')) + ' ';
    }
  return text;
}

/* Return a random number between 0 and NR - 1.  */
unsigned long
test_value (unsigned long nr)
{
  return nr ? (rand () % nr) : 0;
}

/* Print any text.  */
void
test_print (char *text)
{
  if (!verbosep)
    return;
  fprintf (stderr, text);
  fflush (stderr);
}

/* Print an Ok.  */
void
test_ok (void)
{
  if (!verbosep)
    return;
  fprintf (stderr, "ok\n");
  fflush (stderr);
}

/* Print a Failed.  */
void
test_failed (void)
{
  if (!verbosep)
    return;
  fprintf (stderr, "failed\n");
  fflush (stderr);
}

/* Check ‘error’ and display results.  */
#define test(error)  do                         \
    {                                           \
      if (error)                                \
        {                                       \
          test_failed ();                       \
          result++;                             \
        }                                       \
      else                                      \
        test_ok ();                             \
    }                                           \
  while (0)


/*
 * data structure: array
 */

/*
 * Return how often the given value @var{value} is stored in the array
 * @var{array}.  Return zero if there is no such value.
 */
size_t
array_popcount (svz_array_t *array, void *value)
{
  size_t n, size, found;

  if (array == NULL)
    return 0;
  size = svz_array_size (array);
  for (found = n = 0; n < size; n++)
    if (svz_array_get (array, n) == value)
      found++;
  return found;
}

int
array_main (int argc, char **argv)
{
  size_t repeat;
  int result = 0;
  svz_array_t *array;
  size_t n;
  int error;
  void *value;
  size_t cur[2];

  check_nargs (argc, 1, "REPEAT (integer)");
  repeat = atoi (argv[1]);

  test_init ();
  test_print ("array function test suite\n");

#define AGAIN(array)  (array = svz_array_create (0, NULL))
#define CLEAR(array)  (svz_array_destroy (array), AGAIN (array))

  /* array creation */
  error = 0;
  test_print ("    create: ");
  if (AGAIN (array) == NULL)
    error++;
  if (svz_array_size (array) != 0)
    error++;
  test (error);

  /* add and get functions */
  test_print ("       add: ");
  for (n = 0; n < repeat; n++)
    svz_array_add (array, (void *) (n + 1));
  test (repeat != svz_array_size (array));

  test_print ("       get: ");
  for (error = n = 0; n < repeat; n++)
    if (svz_array_get (array, n) != (void *) (n + 1))
      error++;
  if (svz_array_get (array, n) != NULL || svz_array_get (array, -1) != NULL)
    error++;
  test (error);

  /* array iteration */
  error = 0;
  test_print ("   iterate: ");
  svz_array_foreach (array, value, n)
    if (value != (void *) (n + 1))
      error++;
  if (n != repeat || n != svz_array_size (array))
    error++;
  test (error);

  /* set function */
  test_print ("       set: ");
  for (error = n = 0; n < repeat; n++)
    if (svz_array_set (array, n, (void *) (repeat - n)) != (void *) (n + 1))
      error++;
  test (error);

  /* delete function */
  test_print ("    delete: ");
  for (error = n = 0; n < repeat; n++)
    if (svz_array_del (array, 0) != (void *) (repeat - n))
      error++;
  if (svz_array_size (array) != 0)
    error++;

  if (svz_array_del (array, -1) != NULL || svz_array_del (array, n) != NULL)
    error++;
  for (n = 0; n < repeat; n++)
    svz_array_add (array, (void *) n);
  for (n = repeat; n--;)
    if (svz_array_del (array, n) != (void *) n)
      error++;
  if (svz_array_size (array) != 0)
    error++;
  test (error);

  /* check the `contains' function */
  test_print ("  contains: ");
  error = 0;
  for (n = 0; n < repeat; n++)
    if (array_popcount (array, (void *) n))
      error++;
  for (n = 0; n < repeat; n++)
    {
      svz_array_add (array, (void *) n);
      if (array_popcount (array, (void *) n) != 1)
        error++;
    }
  for (n = 0; n < repeat; n++)
    {
      svz_array_set (array, n, (void *) 0);
      if (array_popcount (array, (void *) 0) != n + 1)
        error++;
    }
  CLEAR (array);
  if (array_popcount (array, (void *) 0) != 0)
    error++;
  test (error);

  /* destroy function */
  test_print ("   destroy: ");
  svz_array_destroy (array);
  test_ok ();

  /* is heap ok?  */
  test_print ("      heap: ");
  svz_get_curalloc (cur);
  test (cur[0] || cur[1]);

#undef CLEAR
#undef AGAIN

  return result;
}


/*
 * data structure: hash table
 */

struct it_test
{
  long k_count;
  long v_acc;
};

void
hash_count (void *k, void *v, void *closure)
{
  struct it_test *x = closure;

  if (k)
    x->k_count++;
  x->v_acc += (long) v;
}

void
hash_clear (svz_hash_t **hash)
{
  svz_hash_destroy (*hash);
  *hash = svz_hash_create (4, NULL);
}

struct hash_pair
{
  char *key;
  long value;
};

void
hash_accumulate (void *k, void *v, void *closure)
{
  char *key = k;
  long value = SVZ_PTR2NUM (v);
  svz_array_t *array = closure;
  struct hash_pair *pair = svz_malloc (sizeof (struct hash_pair));

  pair->key = key;
  pair->value = value;
  svz_array_add (array, pair);
}

int
hash_main (int argc, char **argv)
{
  size_t repeat;
  int result = 0;
  svz_hash_t *hash;
  size_t n;
  long error;
  char *text;
  size_t cur[2];

  check_nargs (argc, 1, "REPEAT (integer)");
  repeat = atoi (argv[1]);

  test_print ("hash function test suite\n");

  /* hash creation */
  test_print ("             create: ");
  test ((hash = svz_hash_create (4, NULL)) == NULL);

  /* hash put and get */
  test_print (" put/get and exists: ");
  for (error = n = 0; n < repeat; n++)
    {
      text = svz_strdup (test_string ());
      svz_hash_put (hash, text, (void *) 0xdeadbeef);
      if (((void *) 0xdeadbeef != svz_hash_get (hash, text)))
        error++;
      if (svz_hash_exists (hash, text) == 0)
        error++;
      svz_free (text);
    }
  test (error);

  /* hash containing a certain value */
  test_print ("           contains: ");
  error = 0;
  if (svz_hash_contains (hash, NULL))
    error++;
  if (svz_hash_contains (hash, (void *) 0xeabceabc))
    error++;
  svz_hash_put (hash, "1234567890", (void *) 0xeabceabc);
  if (strcmp ("1234567890", svz_hash_contains (hash, (void *) 0xeabceabc)))
    error++;
  test (error);

  /* hash key deletion */
  test_print ("             delete: ");
  error = 0;
  n = svz_hash_size (hash);
  if ((void *) 0xeabceabc != svz_hash_delete (hash, "1234567890"))
    error++;
  if (n - 1 != svz_hash_size (hash))
    error++;
  test (error);

  /* keys and values */
  test_print ("    keys and values: ");
  hash_clear (&hash);
  error = 0;
  text = svz_malloc (16);
  for (n = 0; n < repeat; n++)
    {
      sprintf (text, "%015lu", (unsigned long) n);
      svz_hash_put (hash, text, (void *) n);
      if (svz_hash_get (hash, text) != (void *) n)
        error++;
    }
  svz_free (text);
  if (n != svz_hash_size (hash))
    error++;
  {
    svz_array_t *array = svz_array_create (repeat, svz_free);
    struct hash_pair *p;

    svz_hash_foreach (hash_accumulate, hash, array);
    if (svz_array_size (array))
      svz_array_foreach (array, p, n)
        {
          if (atol (p->key) != p->value)
            error++;
          if (svz_hash_get (hash, p->key) != SVZ_NUM2PTR (p->value))
            error++;
          if (svz_hash_contains (hash, SVZ_NUM2PTR (p->value)) != p->key)
            error++;
        }
    else
      error++;
    svz_array_destroy (array);
  }
  if (svz_hash_size (hash) != repeat)
    error++;
  test (error);

  /* hash clear */
  test_print ("              clear: ");
  hash_clear (&hash);
  test (svz_hash_size (hash));

  /* hash destruction */
  test_print ("            destroy: ");
  svz_hash_destroy (hash);
  test_ok ();

  /* hash iteration */
  hash = svz_hash_create (4, NULL);

  svz_hash_put (hash, "1234567890", (void *) 1);
  svz_hash_put (hash, "1234567891", (void *) 2);
  svz_hash_put (hash, "1234567892", (void *) 3);

  test_print ("          iteration: ");
  {
    struct it_test x = { 0L, 0L };

    svz_hash_foreach (hash_count, hash, &x);
    test (x.k_count != 3 || x.v_acc != 6);
  }

  svz_hash_destroy (hash);

  /* is heap ok ? */
  test_print ("               heap: ");
  svz_get_curalloc (cur);
  test (cur[0] || cur[1]);

  return result;
}


/*
 * codec
 */

/*
 * Stdin reader for the codec test.  Read as much data as
 * available and set the socket flags to ‘SVZ_SOFLG_FLUSH’ if
 * ready.  Invoke the ‘check_request’ callback each time some
 * data has been received.  Very likely any other ‘read_socket’
 * callback.  [???  Incomplete sentence. --ttn]
 */
int
codec_recv (svz_socket_t *sock)
{
  int num_read, do_read;

  if ((do_read = sock->recv_buffer_size - sock->recv_buffer_fill) <= 0)
    return 0;
  num_read = read ((int) sock->pipe_desc[SVZ_READ],
                   sock->recv_buffer + sock->recv_buffer_fill, do_read);
#ifndef __MINGW32__
  if (num_read < 0 && errno == EAGAIN)
    return 0;
#endif
  if (num_read <= 0)
    {
      sock->flags |= SVZ_SOFLG_FLUSH;
      close ((int) sock->pipe_desc[SVZ_READ]);
      num_read = 0;
    }
  sock->recv_buffer_fill += num_read;
  return sock->check_request (sock);
}

/*
 * Stdout writer.  Write as much data as possible to stdout,
 * removing written bytes from the send buffer.  Very likely any
 * other ‘write_socket’ callback.  [??? Incomplete sentence. --ttn]
 */
int
codec_send (svz_socket_t *sock)
{
  int num_written, do_write;

  if ((do_write = sock->send_buffer_fill) <= 0)
    return 0;
  num_written = write ((int) sock->pipe_desc[SVZ_WRITE],
                       sock->send_buffer, do_write);
#ifndef __MINGW32__
  if (num_written < 0 && errno == EAGAIN)
    return 0;
#endif
  if (num_written <= 0)
    return -1;
  if (num_written < do_write)
    memmove (sock->send_buffer, sock->send_buffer + num_written,
             do_write - num_written);
  sock->send_buffer_fill -= num_written;
  return 0;
}

/* Most simple ‘check_request’ callback I could think of.
   Simply copy the receive buffer into the send buffer.  */
int
codec_check (svz_socket_t *sock)
{
  if (svz_sock_write (sock, sock->recv_buffer, sock->recv_buffer_fill))
    return -1;
  sock->recv_buffer_fill = 0;
  return 0;
}

struct codec_list_closure
{
  int type;
  FILE *to;
};

int
codec_list_internal (const svz_codec_t *codec, void *closure)
{
  struct codec_list_closure *x = closure;

  if (codec->type == x->type)
    fprintf (x->to, " %s", codec->description);
  return 0;
}

void
codec_list (FILE *to)
{
  struct codec_list_closure x;

  x.to = to;
  fprintf (to, "--- list of available codecs ---");

  /* Print encoder list.  */
  fprintf (to, "\n\tencoder:");
  x.type = SVZ_CODEC_ENCODER;
  svz_foreach_codec (codec_list_internal, &x);

  /* Print decoder list.  */
  fprintf (to, "\n\tdecoder:");
  x.type = SVZ_CODEC_DECODER;
  svz_foreach_codec (codec_list_internal, &x);

  fprintf (to, "\n");
}

/*
 * Main entry point for codec tests.
 */
int
codec_main (int argc, char **argv)
{
  int result = 1, id, version;
  svz_socket_t *sock;
  svz_codec_t *codec;
  char *desc;
  size_t cur[2];

  check_nargs (argc, 1, "CODEC < INFILE > OUTFILE");

  /* Setup serveez core library.  */
  svz_boot ("codec");
#if ENABLE_DEBUG
  SVZ_RUNPARM_X (VERBOSITY, 9);
  svz_log_setfile (stderr);
#endif

#ifdef __MINGW32__
  setmode (fileno (stdin), O_BINARY);
  setmode (fileno (stdout), O_BINARY);
#endif

  /* Create single pipe socket for stdin and stdout.  */
  if ((sock = svz_pipe_create ((svz_t_handle) fileno (stdin),
                               (svz_t_handle) fileno (stdout))) == NULL)
    return result;
  sock->read_socket = codec_recv;
  sock->write_socket = codec_send;
  sock->check_request = codec_check;
  if (svz_sock_enqueue (sock))
    return result;
  id = sock->id;
  version = sock->version;

  /* Setup codecs.  */
  desc = argv[1];
  if ((codec = svz_codec_get (desc, SVZ_CODEC_ENCODER)) == NULL)
    {
      codec_list (stderr);
      return result;
    }
  if (svz_codec_sock_receive_setup (sock, codec))
    return result;
  if ((codec = svz_codec_get (desc, SVZ_CODEC_DECODER)) == NULL)
    {
      codec_list (stderr);
      return result;
    }
  if (svz_codec_sock_send_setup (sock, codec))
    return result;

  /* Run server loop.  */
  svz_loop_pre ();
  do
    {
      svz_loop_one ();
    }
  while (svz_sock_find (id, version) && !svz_shutting_down_p ());
  svz_loop_post ();

  /* Finalize the core API.  */
  svz_halt ();

  svz_get_curalloc (cur);
  if (cur[0] || cur[1])
    return 1;

  return EXIT_SUCCESS;
}


/*
 * program passthrough
 */

int
spew_main (int argc, char **argv)
{
  int s;
  struct sockaddr_in addr;
  socklen_t len = sizeof (struct sockaddr_in);
  char *buf1 = "write: Hello\r\n";
  char *buf2 = "send: Hello\r\n";

  check_nargs (argc, 1, "SLEEP (seconds, integer)");
#ifdef __MINGW32__
  WSADATA WSAData;
  WSAStartup (0x0202, &WSAData);
#endif /* __MINGW32__ */

  if (verbosep)
    {
      int i;

      fprintf (stderr, "start...\r\n");
      for (i = 0; i < argc; i++)
        fprintf (stderr, "  argv[%d] = \"%s\"\r\n", i, argv[i]);
      fflush (stderr);
    }

  /* Obtain output descriptor.  */
#ifdef __MINGW32__
  if (getenv ("SEND_HANDLE") != NULL)
    s = atoi (getenv ("SEND_HANDLE"));
  else
    s = fileno (stdout);
#else
  s = fileno (stdout);
#endif

  /* Determine remote connection.  */
  if (getpeername ((svz_t_socket) s, (struct sockaddr *) &addr, &len) < 0)
    {
      fprintf (stderr, "getpeername: %s\n", strerror (errno));
      fflush (stderr);
    }
  else
    {
      /* Try using ‘fprintf’.  */
      fprintf (stdout, "fprintf: %s:%d\r\n",
               inet_ntoa ((* ((struct in_addr *) &addr.sin_addr))),
               ntohs (addr.sin_port));
      fflush (stdout);
    }

  /* Try using ‘write’.  */
  if (write (s, buf1, strlen (buf1)) < 0)
    {
      fprintf (stderr, "write: %s\n", strerror (errno));
      fflush (stderr);
    }
  /* Try using ‘send’.  */
  if (send (s, buf2, strlen (buf2), 0) < 0)
    {
      fprintf (stderr, "send: %s\n", strerror (errno));
      fflush (stderr);
    }

  fflush (stdout);
  sleep (atoi (argv[1]));

#ifdef __MINGW32__
  shutdown (s, 2);
  svz_closesocket (s);
  WSACleanup();
#endif /* __MINGW32__ */

  fprintf (stderr, "...end\r\n");
  return EXIT_SUCCESS;
}


/*
 * dispatch
 */

struct avail
{
  char const *name;
  int (*sub) (int argc, char **argv);
};

#define SUB(x)  { #x, x ## _main }

struct avail avail[] =
  {
    SUB (array),
    SUB (hash),
    SUB (codec),
    SUB (spew),
    { NULL, NULL }
  };

int
main (int argc, char **argv)
{
  struct avail *a = avail;

  if (1 > argc)
    return EXIT_FAILURE;

  {
    char *v = getenv ("VERBOSE");

    if (v && !strcmp ("1", v))
      verbosep = 1;
  }

  for (a = avail; a->name; a++)
    if (!strcmp (argv[1], a->name))
      return a->sub (argc - 1, argv + 1);

  return EXIT_FAILURE;
}
