/*
 * guile-server.c - guile server modules
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001, 2002, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
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

#if ENABLE_GUILE_SERVER

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#if HAVE_FLOSS_H
# include <floss.h>
#endif
#include <libguile.h>

#include "networking-headers.h"
#include "action.h"
#include "libserveez.h"
#include "misc-macros.h"
#include "gi.h"
#include "guile-api.h"
#include "guile.h"
#include "guile-bin.h"
#include "guile-server.h"
#include "unused.h"

#define SVZ_PTR2SCM(x)  ((SCM) SVZ_PTR2NUM (x))

typedef union {
  SCM          sym;
  char * const str;
} symstr_t;

#include "gsk.c"                        /* (find-file "gsk-make") */

static void
init_symbolset (size_t count, symstr_t set[count])
{
  size_t i;
  symstr_t *ss;

  for (i = 0; i < count; i++)
    {
      ss = set + i;
      ss->sym = gi_gc_protect (gi_symbol2scm (ss->str));
    }
}

#define INIT_SYMBOLSET(set)  init_symbolset (set ## _count, set)

static SCM
protected_ht (size_t size)
{
  return gi_gc_protect (gi_make_hash_table (size));
}

static void
ht_zonk_out (SCM *table)
{
  gi_gc_unprotect (*table);
  gi_hash_clear_x (*table);
  *table = SCM_BOOL_F;
}

#define _CTYPE(ctype,x)     guile_svz_ ## ctype ## _ ## x
#define NAME_TAG(ctype)     _CTYPE (ctype, tag)
#define NAME_PRINT(ctype)   _CTYPE (ctype, print)

#define integer_else(obj, def)                  \
  (gi_exactp (obj)                              \
   ? gi_scm2int (obj)                           \
   : def)

/* The guile server type hash.  */
static SCM all_servertypes;

/* The guile socket hash.  */
static SCM all_sockets;

/* If set to zero exception handling is disabled.  */
static int guile_use_exceptions = 1;

/*
 * Creates a Guile SMOB (small object).  The @var{ctype} specifies a base
 * name for all defined vars and functions:
 * a) tag     - The new scheme tag used to identify a SMOB.
 * b) printer - Used when applying (display . args) in Guile.
 */
#define MAKE_SMOB_DEFINITION(ctype)             \
static svz_smob_tag_t NAME_TAG (ctype);         \
static int NAME_PRINT (ctype)                   \
     (SCM smob, SCM port,                       \
      UNUSED scm_print_state *state)            \
{                                               \
  static char txt[256];                         \
                                                \
  snprintf (txt, 256, "#<svz-%s %p>", #ctype,   \
            gi_smob_data (smob));               \
  scm_puts (txt, port);                         \
  return 1;                                     \
}

/* Initializer macro for a new smob type.  */
#define INIT_SMOB(ctype)                        \
  NAME_TAG (ctype) = gi_make_tag                \
    ("svz-" #ctype,                             \
     sizeof (svz_ ## ctype ## _t *),            \
     NULL,                                      \
     NAME_PRINT (ctype),                        \
     NULL)

/* Instantiating macro for a smob type.  */
#define MAKE_SMOB(ctype, data)  gi_make_smob (NAME_TAG (ctype), data)

/* Checks if the given scheme cell is a smob or not.  */
#define CHECK_SMOB(ctype, smob)  gi_smob_tagged_p (smob, NAME_TAG (ctype))

/* Checks if the scheme cell @var{smob} is a smob and throws an error if
   not.  Otherwise the variable @var{var} receives the smob data.  */
#define CHECK_SMOB_ARG(ctype, smob, arg, description, var) do { \
  if (!CHECK_SMOB (ctype, smob))                                \
    scm_wrong_type_arg_msg (FUNC_NAME, arg, smob, description); \
  else                                                          \
    var = gi_smob_data (smob);                                  \
  } while (0)

/* Finally: With the help of the above macros we create smob types for
   Serveez socket structures, servers and server types.  */
MAKE_SMOB_DEFINITION (socket)
MAKE_SMOB_DEFINITION (server)
MAKE_SMOB_DEFINITION (servertype)


/* A hash of smobs, keyed by the wrapped (C lang) object, a pointer.
   This is consulted by ‘valid_smob’ and modified destructively by
   ‘invalidate_smob’.  */
static SCM goodstuff;

#define GOODSTUFF(key)  scm_hashq_ref (goodstuff, key, SCM_BOOL_F)

static SCM
valid_smob (svz_smob_tag_t tag, void *orig)
{
  SCM key = gi_gc_protect (PACK_POINTER (orig));
  SCM val = GOODSTUFF (key);

  if (! gi_nfalsep (val))
    {
      val = gi_gc_protect (gi_make_smob (tag, orig));
      scm_hashq_set_x (goodstuff, key, val);
      gi_gc_unprotect (val);
    }
  gi_gc_unprotect (key);
  return val;
}

#define VALID_SMOB(ctype,orig)  valid_smob (NAME_TAG (ctype), orig)

static void
invalidate_smob (const void *orig)
{
  SCM key = PACK_POINTER (orig);
  SCM smob = GOODSTUFF (key);

  if (gi_nfalsep (smob))
    {
      SCM_SET_SMOB_DATA (smob, NULL);
      scm_hashq_remove_x (goodstuff, key);
    }
}

static void
invalidate_socket_smob (const svz_socket_t *sock)
{
  invalidate_smob (sock);
}

static SCM
socket_smob (svz_socket_t *orig)
{
  return VALID_SMOB (socket, orig);
}

static SCM
server_smob (svz_server_t *orig)
{
  return VALID_SMOB (server, orig);
}

static SCM
servertype_smob (svz_servertype_t *orig)
{
  return VALID_SMOB (servertype, orig);
}

/*
 * Extract a guile procedure from an option hash.  Return zero on success.
 */
static int
optionhash_extract_proc (svz_hash_t *hash,
                         enum guile_functions_ix kidx, /* the key to find       */
                         SCM *target,      /* where to put it       */
                         char *txt)        /* appended to error     */
{
  char key[32];
  SCM proc, hvalue;
  int err = 0;

#define BADNESS(fmtstr, ...)  do                \
    {                                           \
      err = 1;                                  \
      guile_error (fmtstr, __VA_ARGS__);        \
    }                                           \
  while (0)

  GI_GET_XREP (key, guile_functions[kidx].sym);
  hvalue = optionhash_get (hash, key);

  /* Is there such a string in the option-hash?  */
  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      /* Nothing in hash, use default.  */
      *target = SCM_UNDEFINED;
      return err;
    }

  /* Is that guile procedure?  */
  if (SCM_PROCEDUREP (hvalue))
    {
      *target = hvalue;
    }
  else if (STRING_OR_SYMBOL_P (hvalue))
    {
      char str[128];

      GI_GET_XREP (str, hvalue);
      proc = gi_lookup (str);
      if (BOUNDP (proc) && SCM_PROCEDUREP (proc))
        *target = proc;
      else
        BADNESS ("No such procedure `%s' for `%s' %s", str, key, txt);
    }
  else
    BADNESS ("Invalid procedure for `%s' %s", key, txt);
  return err;

#undef BADNESS
}

/*
 * Return the procedure associated w/ the @var{fidx} entry
 * in @code{guile_functions} for @var{stype},
 * or ‘SCM_UNDEFINED’ on lookup failure.
 */
static SCM
servertype_getfunction (svz_servertype_t *stype,
                        enum guile_functions_ix fidx)
{
  SCM servertype, functions;

  if (stype == NULL)
    return SCM_UNDEFINED;

  servertype = servertype_smob (stype);
  functions = scm_hashq_ref (all_servertypes, servertype, SCM_BOOL_F);
  if (! gi_nfalsep (functions))
    return SCM_UNDEFINED;

  return scm_hashq_ref (functions, guile_functions[fidx].sym, SCM_UNDEFINED);
}

/*
 * Do ‘servertype_getfunction’ for @var{server}.
 */
static SCM
server_getfunction (svz_server_t *server, enum guile_functions_ix fidx)
{
  return servertype_getfunction (server->type, fidx);
}

/*
 * Return the procedure associated w/ the @var{ix} entry
 * in @code{guile_sock_fns} for @var{sock},
 * or ‘SCM_UNDEFINED’ on lookup failure.
 */
static SCM
guile_sock_getfunction (svz_socket_t *sock, enum guile_sock_fns_ix ix)
{
  SCM ssock, functions;

  if (sock == NULL)
    return SCM_UNDEFINED;

  if (svz_sock_find (sock->id, sock->version) != sock)
    return SCM_UNDEFINED;

  ssock = socket_smob (sock);
  functions = scm_hashq_ref (all_sockets, ssock, SCM_BOOL_F);
  if (! gi_nfalsep (functions))
    return SCM_UNDEFINED;

  return scm_hashq_ref (functions, guile_sock_fns[ix].sym, SCM_UNDEFINED);
}

extern int global_exit_value;

SCM_DEFINE
(guile_nuke_happened,
 "serveez-nuke", 0, 1, 0,
 (SCM exit_value),
 doc: /***********
Shutdown all network connections and terminate after the next event
loop.  You should use this instead of calling @code{quit}.
Optional arg @var{exit-value} specifies an exit value for the
serveez program.  It is mapped to a number via @code{scm_exit_value}.  */)
{
#define FUNC_NAME s_guile_nuke_happened
  if (BOUNDP (exit_value))
    /* The ‘cons’ avoids a segfault in Guile 1.8.7.  */
    global_exit_value = scm_exit_status (scm_cons (exit_value, SCM_EOL));

  raise (SIGQUIT);
  return SCM_UNSPECIFIED;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_access_exceptions,
 "serveez-exceptions", 0, 1, 0,
 (SCM enable),
 doc: /***********
Control the use of exception handlers for the Guile procedure calls of
Guile server callbacks.  If the optional argument @var{enable} is
@code{#t}, enable exception handling; if @code{#f}, disable it.
Return the current (boolean) setting.  */)
{
#define FUNC_NAME s_guile_access_exceptions
  SCM value = SCM_BOOL (guile_use_exceptions);
  int n;

  if (BOUNDP (enable))
    {
      if (guile_to_boolean (enable, &n))
        guile_error ("%s: Invalid boolean value", FUNC_NAME);
      else
        guile_use_exceptions = n;
    }
  return value;
#undef FUNC_NAME
}

/*
 * The @code{guile_call} function puts the procedure to call including
 * the arguments to it into a single scheme cell passed to this function
 * in @var{data}.  The functions unpacks this cell and applies it to
 * @code{scm_apply}.
 * By convention the @var{data} argument cell consists of three items chained
 * like this: @code{(procedure first-argument (remaining-argument-list))}
 */
static SCM
guile_call_body (void *data)
{
  SCM ls = SVZ_PTR2SCM (data);

  return scm_apply (SCM_CAR (ls), SCM_CADR (ls), SCM_CDDR (ls));
}

/*
 * This is the exception handler for calls by @code{guile_call}.  Prints
 * the procedure (passed in @var{data}), the name of the exception and the
 * error message if possible.
 */
static SCM
guile_call_handler (void *data, SCM tag, SCM args)
{
  SCM procname = SVZ_PTR2SCM (data);
  SCM ep = scm_current_error_port ();

  scm_puts ("exception in ", ep);
  scm_display (procname, ep);
  scm_puts (" due to `", ep);
  scm_display (tag, ep);
  scm_puts ("'\n", ep);
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
  scm_display_error_message (SCM_CAR (SCM_CDR (args)),
                             SCM_CAR (SCM_CDR (SCM_CDR (args))),
                             ep);
  return SCM_BOOL_F;
}

/*
 * The following function takes an arbitrary number of arguments (specified
 * in @var{args}) passed to @code{scm_apply} calling the guile procedure
 * @var{code}.  The function catches exceptions occurring in the procedure
 * @var{code}.  On success (no exception) the routine returns the value
 * returned by @code{scm_apply} otherwise @code{SCM_BOOL_F}.
 */
static SCM
guile_call (SCM code, int args, ...)
{
  va_list list;
  void *body_data, *handler_data;
  SCM arg = SCM_EOL, arglist = SCM_EOL, ret;

  /* Setup arg and arglist correctly for use with ‘scm_apply’.  */
  va_start (list, args);
  if (args > 0)
    {
      arg = va_arg (list, SCM);
      while (--args)
        arglist = scm_cons (va_arg (list, SCM), arglist);
      arglist = scm_cons (scm_reverse (arglist), SCM_EOL);
    }
  va_end (list);

  /* Put both arguments and the procedure together into a single argument
     for the catch body.  */
  body_data = SVZ_NUM2PTR (scm_cons (code, scm_cons (arg, arglist)));
  handler_data = SVZ_NUM2PTR (code);

  /* Use exception handling if requested.  */
  if (guile_use_exceptions)
    {
      ret = scm_internal_catch (SCM_BOOL_T,
                                guile_call_body, body_data,
                                guile_call_handler, handler_data);
    }
  else
    {
      ret = guile_call_body (body_data);
    }

  return ret;
}

/* Wrapper function for the global initialization of a server type.  */
static int
guile_func_global_init (svz_servertype_t *stype)
{
#define FUNC_NAME __func__
  SCM global_init = servertype_getfunction (stype, fn_global_init);
  SCM ret;

  if (BOUNDP (global_init))
    {
      ret = guile_call (global_init, 1, servertype_smob (stype));
      return integer_else (ret, -1);
    }
  return 0;
#undef FUNC_NAME
}


/* Wrapper function for the initialization of a server instance.  */
static int
guile_func_init (svz_server_t *server)
{
#define FUNC_NAME __func__
  SCM init = server_getfunction (server, fn_init);
  SCM ret;

  if (BOUNDP (init))
    {
      ret = guile_call (init, 1, server_smob (server));
      return integer_else (ret, -1);
    }
  return 0;
#undef FUNC_NAME

}
/* Wrapper routine for protocol detection of a server instance.  */
static int
guile_func_detect_proto (svz_server_t *server, svz_socket_t *sock)
{
#define FUNC_NAME __func__
  SCM detect_proto = server_getfunction (server, fn_detect_proto);
  SCM ret;

  if (BOUNDP (detect_proto))
    {
      ret = guile_call (detect_proto, 2, server_smob (server),
                        socket_smob (sock));
      return integer_else (ret, 0);
    }
  return 0;
}
#undef FUNC_NAME

/* Free the socket boundary if set by guile.  */
static void
guile_sock_clear_boundary (svz_socket_t *sock)
{
  if (sock->boundary)
    svz_free_and_zero (sock->boundary);
  sock->boundary_size = 0;
}

/* Wrapper for the socket disconnected callback.  Used here in order to
   delete the additional guile callbacks associated with the disconnected
   socket structure.  */
static int
guile_func_disconnected_socket (svz_socket_t *sock)
{
#define FUNC_NAME __func__
  SCM ssock = socket_smob (sock);
  SCM ret, disconnected = guile_sock_getfunction (sock, sfn_disconnected);
  int retval = -1;

  /* First call the guile callback if necessary.  */
  if (BOUNDP (disconnected))
    {
      ret = guile_call (disconnected, 1, ssock);
      retval = integer_else (ret, -1);
    }

  /* Delete all the associated guile callbacks.  */
  scm_hashq_remove_x (all_sockets, ssock);

  /* Free the socket boundary if set by guile.  */
  guile_sock_clear_boundary (sock);

  return retval;
#undef FUNC_NAME

}
/* Wrapper for the kicked socket callback.  */
static int
guile_func_kicked_socket (svz_socket_t *sock, int reason)
{
#define FUNC_NAME __func__
  SCM ret, kicked = guile_sock_getfunction (sock, sfn_kicked);

  if (BOUNDP (kicked))
    {
      ret = guile_call (kicked, 2, socket_smob (sock),
                        gi_integer2scm (reason));
      return integer_else (ret, -1);
    }
  return 0;
}
#undef FUNC_NAME

/* Wrapper function for the socket connection after successful detection.  */
static int
guile_func_connect_socket (svz_server_t *server, svz_socket_t *sock)
{
#define FUNC_NAME __func__
  SCM connect_socket = server_getfunction (server, fn_connect_socket);
  SCM ret;

  /* Setup this function for later use.  */
  sock->disconnected_socket = guile_func_disconnected_socket;

  if (BOUNDP (connect_socket))
    {
      ret = guile_call (connect_socket, 2, server_smob (server),
                        socket_smob (sock));
      return integer_else (ret, 0);
    }
  return 0;
#undef FUNC_NAME

}
/* Wrapper for the finalization of a server instance.  */
static int
guile_func_finalize (svz_server_t *server)
{
#define FUNC_NAME __func__
  SCM ret, finalize = server_getfunction (server, fn_finalize);
  int retval = 0;

  if (BOUNDP (finalize))
    {
      ret = guile_call (finalize, 1, server_smob (server));
      retval = integer_else (ret, -1);
    }

  invalidate_smob (server);
  return retval;
}
#undef FUNC_NAME

/* Wrapper routine for the global finalization of a server type.  */
static int
guile_func_global_finalize (svz_servertype_t *stype)
{
#define FUNC_NAME __func__
  svz_config_prototype_t *prototype = &stype->config_prototype;
  SCM servertype = servertype_smob (stype);
  SCM ret, global_finalize;
  int i, retval = 0;

  global_finalize = servertype_getfunction (stype, fn_global_finalize);

  if (BOUNDP (global_finalize))
    {
      ret = guile_call (global_finalize, 1, servertype);
      retval = integer_else (ret, -1);
    }

  scm_hashq_remove_x (all_servertypes, servertype);
  invalidate_smob (stype);
  svz_free (stype->prefix);
  svz_free (stype->description);
  for (i = 0; prototype->items[i].type != SVZ_ITEM_END; i++)
    svz_free (prototype->items[i].name);
  svz_config_free (prototype, prototype->start);
  svz_free (prototype->items);
  svz_free (stype);
  return retval;
#undef FUNC_NAME
}

/* Wrapper for the client info callback.  */
static char *
guile_func_info_client (svz_server_t *server, svz_socket_t *sock)
{
#define FUNC_NAME __func__
  SCM info_client = server_getfunction (server, fn_info_client);
  SCM ret;
  static char text[1024];

  if (BOUNDP (info_client))
    {
      ret = guile_call (info_client, 2, server_smob (server),
                        socket_smob (sock));
      if (GI_GET_XREP_MAYBE (text, ret))
        return text;
    }
  return NULL;
}
#undef FUNC_NAME

/* Wrapper for the server info callback.  */
static char *
guile_func_info_server (svz_server_t *server)
{
#define FUNC_NAME __func__
  SCM info_server = server_getfunction (server, fn_info_server);
  SCM ret;
  static char text[1024];

  if (BOUNDP (info_server))
    {
      ret = guile_call (info_server, 1, server_smob (server));
      if (GI_GET_XREP_MAYBE (text, ret))
        return text;
    }
  return NULL;
#undef FUNC_NAME

}
/* Wrapper for the server notifier callback.  */
static int
guile_func_notify (svz_server_t *server)
{
#define FUNC_NAME __func__
  SCM ret, notify = server_getfunction (server, fn_notify);

  if (BOUNDP (notify))
    {
      ret = guile_call (notify, 1, server_smob (server));
      return integer_else (ret, -1);
    }
  return -1;
}
#undef FUNC_NAME

/* Wrapper for the server reset callback.  */
static int
guile_func_reset (svz_server_t *server)
{
#define FUNC_NAME __func__
  SCM ret, reset = server_getfunction (server, fn_reset);

  if (BOUNDP (reset))
    {
      ret = guile_call (reset, 1, server_smob (server));
      return integer_else (ret, -1);
    }
  return -1;
#undef FUNC_NAME

}
/* Wrapper for the socket check request callback.  */
static int
guile_func_check_request (svz_socket_t *sock)
{
#define FUNC_NAME __func__
  SCM ret, check_request;
  check_request = guile_sock_getfunction (sock, sfn_check_request);

  if (BOUNDP (check_request))
    {
      ret = guile_call (check_request, 1, socket_smob (sock));
      return integer_else (ret, -1);
    }
  return -1;
}
#undef FUNC_NAME

/* Wrapper for the socket handle request callback.  The function searches for
   both the servertype specific and socket specific procedure.  */
static int
guile_func_handle_request (svz_socket_t *sock, char *request, int len)
{
#define FUNC_NAME __func__
  svz_server_t *server;
  SCM ret, handle_request;
  handle_request = guile_sock_getfunction (sock, sfn_handle_request);

  if (!BOUNDP (handle_request))
    {
      server = svz_server_find (sock->cfg);
      handle_request = server_getfunction (server, fn_handle_request);
    }

  if (BOUNDP (handle_request))
    {
      ret = guile_call (handle_request, 3, socket_smob (sock),
                        guile_data_to_bin (request, len), gi_integer2scm (len));
      return integer_else (ret, -1);
    }
  return -1;
#undef FUNC_NAME

}
/* Wrapper for the socket idle func callback.  */
static int
guile_func_idle_func (svz_socket_t *sock)
{
#define FUNC_NAME __func__
  SCM ret, idle_func = guile_sock_getfunction (sock, sfn_idle);

  if (BOUNDP (idle_func))
    {
      ret = guile_call (idle_func, 1, socket_smob (sock));
      return integer_else (ret, -1);
    }
  return 0;
}
#undef FUNC_NAME

/* Wrapper for the socket trigger condition func callback.  */
static int
guile_func_trigger_cond (svz_socket_t *sock)
{
#define FUNC_NAME __func__
  SCM ret, trigger_cond = guile_sock_getfunction (sock, sfn_trigger_condition);

  if (BOUNDP (trigger_cond))
    {
      ret = guile_call (trigger_cond, 1, socket_smob (sock));
      return gi_nfalsep (ret);
    }
  return 0;
#undef FUNC_NAME

}
/* Wrapper for the socket trigger func callback.  */
static int
guile_func_trigger_func (svz_socket_t *sock)
{
#define FUNC_NAME __func__
  SCM ret, trigger_func = guile_sock_getfunction (sock, sfn_trigger);

  if (BOUNDP (trigger_func))
    {
      ret = guile_call (trigger_func, 1, socket_smob (sock));
      return integer_else (ret, -1);
    }
  return 0;
}
#undef FUNC_NAME

/* Wrapper for the socket check oob request callback.  */
static int
guile_func_check_request_oob (svz_socket_t *sock)
{
#define FUNC_NAME __func__
  SCM ret, check_request_oob;
  check_request_oob = guile_sock_getfunction (sock, sfn_check_oob_request);

  if (BOUNDP (check_request_oob))
    {
      ret = guile_call (check_request_oob, 2, socket_smob (sock),
                        gi_integer2scm (sock->oob));
      return integer_else (ret, -1);
    }
  return -1;
#undef FUNC_NAME
}


/*
 * sock callbacks: {handle,check}-request
 * (and others)
 */

#define CHECK_SOCK_SMOB_ARG(svar,cvar)                          \
  CHECK_SMOB_ARG (socket, svar, SCM_ARG1, "svz-socket", cvar)

typedef int (* getset_fn_t) ();

struct sock_callback_details {
  const char  *who;
  getset_fn_t  getset;
  size_t       place;                   /* ‘svz_socket_t’ offset */
  enum guile_sock_fns_ix assoc;
};

static SCM
sock_callback_body (const struct sock_callback_details *d,
                    SCM sock, SCM proc)
{
  const char *FUNC_NAME = d->who;
  svz_socket_t *xsock;

  CHECK_SOCK_SMOB_ARG (sock, xsock);
  if (BOUNDP (proc))
    {
      SCM func = guile_sock_fns[d->assoc].sym;
      getset_fn_t *place = (void *) xsock + d->place;
      SCM functions;
      SCM oldproc;

      SCM_ASSERT_TYPE (SCM_PROCEDUREP (proc), proc, SCM_ARG2,
                       FUNC_NAME, "procedure");
      *place = d->getset;

      functions = scm_hashq_ref (all_sockets, sock, SCM_BOOL_F);
      if (! gi_nfalsep (functions))
        {
          functions = protected_ht (3);
          scm_hashq_set_x (all_sockets, sock, functions);
          gi_gc_unprotect (functions);
        }

      /* Put the procedure into the socket's hash.
         If there was one previously set, return that.  */
      oldproc = scm_hashq_ref (functions, func, SCM_BOOL_F);
      if (! gi_nfalsep (oldproc))
        oldproc = SCM_UNDEFINED;
      scm_hashq_set_x (functions, func, proc);
      return oldproc;
    }
  return guile_sock_getfunction (xsock, d->assoc);
}

/* This macro creates a the body of socket callback getter/setter for
   use from Scheme code.  The procedure returns any previously set
   callback or an undefined value.  */
#define SOCK_CALLBACK_BODY(FUNC,ASSOC)          \
  const struct sock_callback_details d = {      \
    .who    = s_guile_sock_ ## FUNC,            \
    .getset = guile_func_ ## FUNC,              \
    .place  = offsetof (svz_socket_t, FUNC),    \
    .assoc  = ASSOC                             \
  };                                            \
  return sock_callback_body (&d, sock, proc)

SCM_DEFINE
(guile_sock_handle_request,
 "svz:sock:handle-request", 1, 1, 0,
 (SCM sock, SCM proc),
 doc: /***********
Set the @code{handle-request} member of the socket structure @var{sock}
to @var{proc}.  Return the previously set handler if there is any.  */)
{
  SOCK_CALLBACK_BODY (handle_request, sfn_handle_request);
}

SCM_DEFINE
(guile_sock_check_request,
 "svz:sock:check-request", 1, 1, 0,
 (SCM sock, SCM proc),
 doc: /***********
Set the @code{check-request} member of the socket structure @var{sock}
to @var{proc}.  Return the previously handler if there is any.  */)
{
  SOCK_CALLBACK_BODY (check_request, sfn_check_request);
}


SCM_DEFINE
(guile_sock_boundary,
 "svz:sock:boundary", 2, 0, 0,
 (SCM sock, SCM boundary),
 doc: /***********
Setup the packet boundary of the socket @var{sock}.  The given string
value @var{boundary} can contain any kind of data.  If @var{boundary}
is an exact number, set up the socket to parse fixed sized packets.
More precisely, set the @code{check-request} callback of the given
socket structure @var{sock} to an internal routine which runs the
socket's @code{handle-request} callback when it detects a
complete packet specified by @var{boundary}.

For instance, you can arrange for Serveez to pass the
@code{handle-request} procedure lines of text by calling
@code{(svz:sock:boundary sock "\n")}.  */)
{
#define FUNC_NAME s_guile_sock_boundary
  svz_socket_t *xsock;

  CHECK_SOCK_SMOB_ARG (sock, xsock);
  SCM_ASSERT_TYPE (gi_exactp (boundary) || gi_stringp (boundary),
                   boundary, SCM_ARG2, FUNC_NAME, "string or exact");

  /* Release previously set boundaries.  */
  guile_sock_clear_boundary (xsock);

  /* Setup for fixed sized packets.  */
  if (gi_exactp (boundary))
    {
      xsock->boundary = NULL;
      xsock->boundary_size = gi_scm2int (boundary);
    }
  /* Handle packet delimiters.  */
  else
    {
      char buf[512];

      xsock->boundary_size = GI_GET_XREP (buf, boundary);
      xsock->boundary = svz_strdup (buf);
    }

  /* Only assign this callback for connection oriented protocols.  */
  if (xsock->proto & (SVZ_PROTO_TCP | SVZ_PROTO_PIPE))
    xsock->check_request = svz_sock_check_request;

  return SCM_BOOL_T;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_floodprotect,
 "svz:sock:floodprotect", 1, 1, 0,
 (SCM sock, SCM flag),
 doc: /***********
Set or unset the flood protection bit of the given socket @var{sock}.
Return the previous value of this bit (@code{#t} or @code{#f}).  The
@var{flag} argument must be either boolean or an exact number and is
optional.  */)
{
#define FUNC_NAME s_guile_sock_floodprotect
  svz_socket_t *xsock;
  int flags;

  CHECK_SOCK_SMOB_ARG (sock, xsock);
  flags = xsock->flags;
  if (BOUNDP (flag))
    {
      SCM_ASSERT_TYPE (SCM_BOOLP (flag) || gi_exactp (flag),
                       flag, SCM_ARG2, FUNC_NAME, "boolean or exact");
      if ((SCM_BOOLP (flag) && gi_nfalsep (flag) != 0) ||
          (gi_exactp (flag) && gi_scm2int (flag) != 0))
        xsock->flags &= ~SVZ_SOFLG_NOFLOOD;
      else
        xsock->flags |= SVZ_SOFLG_NOFLOOD;
    }
  return (flags & SVZ_SOFLG_NOFLOOD) ? SCM_BOOL_F : SCM_BOOL_T;
#undef FUNC_NAME
}

SCM_DEFINE
(guile_sock_print,
 "svz:sock:print", 2, 0, 0,
 (SCM sock, SCM buffer),
 doc: /***********
Write @var{buffer} (string or binary smob) to the socket @var{sock}.
Return @code{#t} on success and @code{#f} on failure.  */)
{
#define FUNC_NAME s_guile_sock_print
  svz_socket_t *xsock;
  char tem[8192];
  char *buf;
  int len, ret = -1;

  CHECK_SOCK_SMOB_ARG (sock, xsock);
  SCM_ASSERT_TYPE (gi_stringp (buffer) || guile_bin_check (buffer),
                   buffer, SCM_ARG2, FUNC_NAME, "string or binary");

  if (gi_stringp (buffer))
    {
      len = GI_GET_XREP (tem, buffer);
      buf = tem;
    }
  else
    {
      buf = guile_bin_to_data (buffer, &len);
    }

  /* Depending on the protocol type use different kind of senders.  */
  if (xsock->proto & (SVZ_PROTO_TCP | SVZ_PROTO_PIPE))
    ret = svz_sock_write (xsock, buf, len);
  else if (xsock->proto & SVZ_PROTO_UDP)
    ret = svz_udp_write (xsock, buf, len);
  else if (xsock->proto & SVZ_PROTO_ICMP)
    ret = svz_icmp_write (xsock, buf, len);

  if (ret == -1)
    {
      svz_sock_schedule_for_shutdown (xsock);
      return SCM_BOOL_F;
    }
  return SCM_BOOL_T;
#undef FUNC_NAME
}

/*
 * Convert the given value at @var{address} of the the type @var{type} into
 * a scheme cell.
 */
static SCM
guile_config_convert (void *address, int type)
{
  svz_portcfg_t *port;
  SCM ret = SCM_EOL;

  switch (type)
    {
    case SVZ_ITEM_INT:
      ret = gi_integer2scm (*(int *) address);
      break;
    case SVZ_ITEM_INTARRAY:
      ret = guile_intarray_to_guile (*(svz_array_t **) address);
      break;
    case SVZ_ITEM_STR:
      ret = gi_string2scm (*(char **) address);
      break;
    case SVZ_ITEM_STRARRAY:
      ret = guile_strarray_to_guile (*(svz_array_t **) address);
      break;
    case SVZ_ITEM_HASH:
      ret = guile_hash_to_guile (*(svz_hash_t **) address);
      break;
    case SVZ_ITEM_PORTCFG:
      if ((port = *(svz_portcfg_t **) address) != NULL)
        ret = gi_string2scm (port->name);
      break;
    case SVZ_ITEM_BOOL:
      ret = SCM_BOOL (*(int *) address);
      break;
    }
  return ret;
}

/* Checks if the given Guile object @var{smob} in position @var{arg} is a
   server or socket and throws an exception if not.  Otherwise it saves the
   server in the variable @var{var}.  */
#define CHECK_SERVER_SMOB_ARG(smob, arg, var)                                \
  do {                                                                       \
    SCM_ASSERT_TYPE (CHECK_SMOB (server, smob) ||                            \
                     CHECK_SMOB (socket, smob), smob, arg,                   \
                     FUNC_NAME, "svz-server or svz-socket");                 \
    var = CHECK_SMOB (server, smob) ? gi_smob_data (smob) :                  \
      svz_server_find (((svz_socket_t *) gi_smob_data (smob))->cfg);         \
  } while (0)

SCM_DEFINE
(guile_server_config_ref,
 "svz:server:config-ref", 2, 0, 0,
 (SCM server, SCM key),
 doc: /***********
Return the configuration item specified by @var{key} of the given server
instance @var{server}.  You can pass this procedure a socket, too, in
which case the appropriate server instance is looked up.  If the given
string @var{key} is invalid (not defined in the configuration alist in
@code{define-servertype!}), then return an empty list.  */)
{
#define FUNC_NAME s_guile_server_config_ref
  SCM ret = SCM_EOL;
  svz_server_t *xserver;
  int i;
  void *cfg, *address;
  char str[64];
  svz_config_prototype_t *prototype;

  CHECK_SERVER_SMOB_ARG (server, SCM_ARG1, xserver);
  ASSERT_STRING (2, key);

  GI_GET_XREP (str, key);
  cfg = xserver->cfg;
  prototype = &xserver->type->config_prototype;

  for (i = 0; prototype->items[i].type != SVZ_ITEM_END; i++)
    {
      if (strcmp (prototype->items[i].name, str) == 0)
        {
          address = (void *) ((unsigned long) cfg +
                              (unsigned long) prototype->items[i].address -
                              (unsigned long) prototype->start);
          ret = guile_config_convert (address, prototype->items[i].type);
          break;
        }
    }
  return ret;
#undef FUNC_NAME
}

/*
 * Returns the length of a configuration item type, updates the configuration
 * item structure @var{item} and increases the @var{size} value if the
 * text representation @var{str} fits one of the item types understood by
 * Serveez.  Returns zero if there is no such type.
 */
static int
guile_servertype_config_type (char *str, svz_key_value_pair_t *item, int *size)
{
  int n;
  struct {
    char *key;
    int size;
    int type;
  }
  config_types[] = {
    { "integer", sizeof (int), SVZ_ITEM_INT },
    { "intarray", sizeof (svz_array_t *), SVZ_ITEM_INTARRAY },
    { "string", sizeof (char *), SVZ_ITEM_STR },
    { "strarray", sizeof (svz_array_t *), SVZ_ITEM_STRARRAY },
    { "hash", sizeof (svz_hash_t *), SVZ_ITEM_HASH },
    { "portcfg", sizeof (svz_portcfg_t *), SVZ_ITEM_PORTCFG },
    { "boolean", sizeof (int), SVZ_ITEM_BOOL },
    { NULL, 0, -1 }
  };

  for (n = 0; config_types[n].key != NULL; n++)
    {
      if (strcmp (str, config_types[n].key) == 0)
        {
          item->type = config_types[n].type;
          *size += config_types[n].size;
          return config_types[n].size;
        }
    }
  return 0;
}

/*
 * Obtain a default value from the scheme cell @var{value}.  The configuration
 * item type is specified by @var{type}.  The default value is stored then at
 * @var{address}.  Returns zero on success.
 */
static int
guile_servertype_config_default (svz_servertype_t *stype, SCM value,
                                 void *address, int len, int type, char *key)
{
  int err = 0, n;
  char str[2048], *txt;
  svz_array_t *array;
  svz_hash_t *hash;
  svz_portcfg_t *port, *dup;

  switch (type)
    {
      /* Integer.  */
    case SVZ_ITEM_INT:
      if (guile_to_integer (value, &n) != 0)
        {
          guile_error ("%s: Invalid integer value for `%s'",
                       stype->prefix, key);
          err = -1;
        }
      else
        memcpy (address, &n, len);
      break;

      /* Array of integers.  */
    case SVZ_ITEM_INTARRAY:
      if ((array = guile_to_intarray (value, key)) == NULL)
        err = -1;
      else
        memcpy (address, &array, len);
      break;

      /* Character string.  */
    case SVZ_ITEM_STR:
      if (! GI_GET_XREP_MAYBE (str, value))
        {
          guile_error ("%s: Invalid string value for `%s'",
                       stype->prefix, key);
          err = -1;
        }
      else
        {
          txt = svz_strdup (str);
          memcpy (address, &txt, len);
        }
      break;

      /* Array of character strings.  */
    case SVZ_ITEM_STRARRAY:
      if ((array = guile_to_strarray (value, key)) == NULL)
        err = -1;
      else
        memcpy (address, &array, len);
      break;

      /* Hash.  */
    case SVZ_ITEM_HASH:
      if ((hash = guile_to_hash (value, key)) == NULL)
        err = -1;
      else
        memcpy (address, &hash, len);
      break;

      /* Port configuration.  */
    case SVZ_ITEM_PORTCFG:
      if (! GI_GET_XREP_MAYBE (str, value))
        {
          guile_error ("%s: Invalid string value for `%s'",
                       stype->prefix, key);
          err = -1;
        }
      else if ((port = svz_portcfg_get (str)) == NULL)
        {
          guile_error ("%s: No such port configuration: `%s'",
                       stype->prefix, str);
          err = -1;
        }
      else
        {
          dup = svz_portcfg_dup (port);
          memcpy (address, &dup, len);
        }
      break;

      /* Boolean value.  */
    case SVZ_ITEM_BOOL:
      if (guile_to_boolean (value, &n) != 0)
        {
          guile_error ("%s: Invalid boolean value for `%s'",
                       stype->prefix, key);
          err = -1;
        }
      else
        memcpy (address, &n, sizeof (int));
      break;

      /* Invalid type.  */
    default:
      err = -1;
    }
  return err;
}

static void
items_append (svz_key_value_pair_t **all, unsigned int i,
              svz_key_value_pair_t *one)
{
  *all = svz_realloc (*all, (1 + i) * sizeof (*one));
  /* Is this equivalent to:  *((*all)[n]) = *one;  ???  */
  memcpy (&((*all)[i]), one, sizeof (*one));
}

struct servertype_config_closure
{
  svz_servertype_t *stype;
  char *action;
  svz_hash_t *options;
  int error;
  int size;
  unsigned int count;
  svz_key_value_pair_t *items;
  char *prototype;
};

static void
servertype_config_internal (void *k, UNUSED void *v, void *closure)
{
  struct servertype_config_closure *x = closure;
  char *key = k;
  SCM list = optionhash_get (x->options, key);
  svz_key_value_pair_t item;
  SCM value;
  char str[64];
  int len, def;

  /* Each configuration item must be a scheme list with three elements.  */
  if (!SCM_LISTP (list) ||
      gi_scm2ulong (scm_length (list)) != 3)
    {
      guile_error ("Invalid definition for `%s' %s", key, x->action);
      x->error = -1;
      return;
    }

  /* Assign address offset.  */
  item.address = SVZ_NUM2PTR (x->size);

  /* First appears the type of item.  */
  value = SCM_CAR (list);
  if (! GI_GET_XREP_MAYBE (str, value))
    {
      guile_error ("Invalid type definition for `%s' %s", key, x->action);
      x->error = -1;
      return;
    }
  len = guile_servertype_config_type (str, &item, &x->size);
  if (len == 0)
    {
      guile_error ("Invalid type for `%s' %s", key, x->action);
      x->error = -1;
      return;
    }

  /* Then appears a boolean value specifying if the configuration
     item is defaultable or not.  */
  list = SCM_CDR (list);
  value = SCM_CAR (list);
  if (guile_to_boolean (value, &def) != 0)
    {
      guile_error ("Invalid defaultable value for `%s' %s", key, x->action);
      x->error = -1;
      return;
    }
  item.defaultable = def
    ? SVZ_ITEM_DEFAULTABLE
    : SVZ_ITEM_NOTDEFAULTABLE;

  /* Finally the default value itself.  */
  list = SCM_CDR (list);
  value = SCM_CAR (list);
  x->prototype = svz_realloc (x->prototype, x->size);
  memset (x->prototype + x->size - len, 0, len);
  if (item.defaultable == SVZ_ITEM_DEFAULTABLE)
    {
      x->error |= guile_servertype_config_default
        (x->stype, value, x->prototype + x->size - len,
         len, item.type, key);
    }

  /* Increase the number of configuration items.  */
  item.name = svz_strdup (key);
  items_append (&x->items, x->count++, &item);
}

/*
 * Parse the configuration of the server type @var{stype} stored in the
 * scheme cell @var{cfg}.
 */
static int
guile_servertype_config (svz_servertype_t *stype, SCM cfg)
{
#define FUNC_NAME __func__
  unsigned int n;
  svz_key_value_pair_t item;
  char action[ACTIONBUFSIZE];
  struct servertype_config_closure x =
    {
      stype, action,
      NULL,                             /* .options */
      0,                                /* .error */
      0,                                /* .size */
      0,                                /* .count */
      NULL,                             /* .items */
      NULL                              /* .prototype */
    };

#define err      (x.error)              /* Keep things tidy.  */
#define items    (x.items)
#define options  (x.options)

  DOING ("parsing configuration of `%s'", stype->prefix);

  /* Check if the configuration alist is given or not.  */
  if (SCM_EQ_P (cfg, SCM_UNSPECIFIED))
    {
      guile_error ("Missing servertype `configuration' for `%s'",
                   stype->prefix);
      FAIL ();
    }

  /* Try parsing this alist is valid.  */
  if (NULL == (options = guile_to_optionhash (cfg, action, 0)))
    FAIL ();                    /* Message already emitted.  */

  /* Check the servertype configuration definition for duplicates.  */
  err |= optionhash_validate (options, 1, "configuration", stype->prefix);

  /* Now check all configuration items.  */
  svz_hash_foreach (servertype_config_internal, options, &x);

  /* Append the last configuration item identifying the end of the
     configuration item list.  */
  item.type = SVZ_ITEM_END;
  item.address = NULL;
  item.defaultable = 0;
  item.name = NULL;
  items_append (&items, x.count++, &item);

  /* Adjust the address values of the configuration items and assign
     all gathered information to the given servertype.  */
  for (n = 0; n < x.count; n++)
    items[n].address = (void *) ((unsigned long) items[n].address +
      (unsigned long) x.prototype);
  stype->config_prototype.start = x.prototype;
  stype->config_prototype.size = x.size;
#undef items        /* Unfortunately, tidy is incorrect for next line LHS.  */
  stype->config_prototype.items = x.items;

 out:
  optionhash_destroy (options);
  return err;

#undef options
#undef err
#undef FUNC_NAME
}

SCM_DEFINE
(guile_define_servertype,
 "define-servertype!", 1, 0, 0,
 (SCM args),
 doc: /***********
Define a new server type based on @var{args}.  (If everything
works fine you have a freshly registered server type afterwards.)
Return @code{#t} on success.  */)
{
#define FUNC_NAME s_guile_define_servertype
  enum guile_functions_ix n;
  int err = 0;
  svz_hash_t *options;
  SCM proc, functions;
  svz_servertype_t *stype;
  char action[ACTIONBUFSIZE];

  stype = svz_calloc (sizeof (svz_servertype_t));
  DEFINING ("%s", "servertype");

  if (NULL == (options = guile_to_optionhash (args, action, 0)))
    FAIL ();                    /* Message already emitted.  */

  /* Obtain the servertype prefix variable (Mandatory).  */
  if (optionhash_extract_string (options, "prefix", 0, NULL,
                                 &stype->prefix, action) != 0)
    FAIL ();
  DEFINING ("servertype `%s'", stype->prefix);

  /* Check the servertype definition once.  */
  err |= optionhash_validate (options, 1, "servertype", stype->prefix);

  /* Get the description of the server type.  */
  err |= optionhash_extract_string (options, "description", 0, NULL,
                                    &stype->description, action);

  /* Set the procedures.  */
  functions = protected_ht (3);
  for (n = 0; n < guile_functions_count; n++)
    {
      err |= optionhash_extract_proc (options, n, &proc, action);
      scm_hashq_set_x (functions, guile_functions[n].sym, proc);
    }

  /* Check duplicate server types.  */
  if (svz_servertype_get (stype->prefix, 0) != NULL)
    {
      guile_error ("Duplicate servertype definition: `%s'", stype->prefix);
      err = -1;
    }
  else
    {
      /* Check the configuration items for this servertype.  */
      err |= guile_servertype_config (stype,
                                      optionhash_get (options,
                                                      "configuration"));
    }

  if (!err)
    {
      SCM servertype = servertype_smob (stype);

      stype->global_init = guile_func_global_init;
      stype->init = guile_func_init;
      stype->detect_proto = guile_func_detect_proto;
      stype->connect_socket = guile_func_connect_socket;
      stype->finalize = guile_func_finalize;
      stype->global_finalize = guile_func_global_finalize;
      stype->info_client = guile_func_info_client;
      stype->info_server = guile_func_info_server;
      stype->notify = guile_func_notify;
      stype->reset = guile_func_reset;
      stype->handle_request = guile_func_handle_request;

      /* Hook it all up.  */
      scm_hashq_set_x (all_servertypes, servertype, functions);
      gi_gc_unprotect (functions);
      svz_servertype_add (stype);
    }
  else
    {
      svz_free (stype->prefix);
      if (stype->description)
        svz_free (stype->description);
      svz_free (stype);
      ht_zonk_out (&functions);
    }

 out:
  optionhash_destroy (options);
  guile_global_error |= err;
  return err ? SCM_BOOL_F : SCM_BOOL_T;
#undef FUNC_NAME
}

#include "guile-api.c"

/*
 * Initialization of the guile server module.  Should be run before calling
 * @code{guile_eval_file}.  It registers some new guile procedures and
 * creates some static data.
 */
void
guile_server_init (void)
{
  INIT_SYMBOLSET (guile_functions);
  INIT_SYMBOLSET (guile_sock_fns);

  /* Initialize the guile SMOB things.  Previously defined via
     MAKE_SMOB_DEFINITION ().  */
  INIT_SMOB (socket);
  INIT_SMOB (server);
  INIT_SMOB (servertype);

  svz_sock_prefree (1, invalidate_socket_smob);
  goodstuff = protected_ht (11);
  all_servertypes = protected_ht (3);
  all_sockets = protected_ht (23);

#include "guile-server.x"

  guile_bin_init ();
  guile_api_init ();
}

/*
 * This function should be called before shutting down the core library in
 * order to avoid memory leaks.  It releases the server types defined with
 * guile.
 */
void
guile_server_finalize (void)
{
  guile_api_finalize ();

  ht_zonk_out (&all_sockets);
  ht_zonk_out (&all_servertypes);
  ht_zonk_out (&goodstuff);
  svz_sock_prefree (0, invalidate_socket_smob);
}

#else /* not ENABLE_GUILE_SERVER */

static int have_guile_server = 0;

#endif /* ENABLE_GUILE_SERVER */
