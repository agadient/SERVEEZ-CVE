/*
 * server.c - server object functions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
#endif
#include "networking-headers.h"
#include "unused.h"
#include "libserveez/alloc.h"
#include "libserveez/hash.h"
#include "libserveez/util.h"
#include "libserveez/core.h"
#include "libserveez/array.h"
#include "libserveez/socket.h"
#include "libserveez/server-core.h"
#include "libserveez/server.h"
#include "libserveez/binding.h"
#include "libserveez/dynload.h"

/*
 * The list of registered servers.  Feel free to add yours.
 */
static svz_array_t *servertypes = NULL;

/*
 * This is the list of actually instantiated servers.  The hash table
 * associates the servers' names with the server instances.
 */
static svz_hash_t *servers = NULL;

/**
 * Call @var{func} for each servertype, passing additionally the second arg
 * @var{closure}.  If @var{func} returns a negative value, return immediately
 * with that value (breaking out of the loop), otherwise, return 0.
 */
int
svz_foreach_servertype (svz_servertype_do_t *func, void *closure)
{
  size_t i;
  int rv;
  svz_servertype_t *stype;

  svz_array_foreach (servertypes, stype, i)
    {
      if (0 > (rv = func (stype, closure)))
        return rv;
    }
  return 0;
}

struct foreach_server_closure
{
  svz_server_do_t *func;
  void *closure;
};

static void
foreach_server_internal (UNUSED void *k, void *v, void *closure)
{
  struct foreach_server_closure *x = closure;

  x->func (v, x->closure);
}

/**
 * Call @var{func} for each server, passing additionally the second arg
 * @var{closure}.
 */
void
svz_foreach_server (svz_server_do_t *func, void *closure)
{
  if (servers)
    {
      struct foreach_server_closure x = { func, closure };

      svz_hash_foreach (foreach_server_internal, servers, &x);
    }
}

/**
 * Add the server type @var{server} to the currently registered servers.
 */
void
svz_servertype_add (svz_servertype_t *server)
{
  size_t n;
  svz_servertype_t *stype;

  /* Check if the server definition is valid.  */
  if (!server || !server->prefix || !server->description)
    {
      svz_log (SVZ_LOG_ERROR, "invalid server type\n");
      return;
    }

  /* Check if the server is already registered.  */
  svz_array_foreach (servertypes, stype, n)
    {
      if (!strcmp (server->prefix, stype->prefix))
        {
          svz_log (SVZ_LOG_ERROR, "server type `%s' already registered\n",
                   server->description);
          return;
        }
    }

  /* Run the global server type initializer.  */
  if (server->global_init != NULL)
    if (server->global_init (server) < 0)
      {
        svz_log (SVZ_LOG_ERROR, "error running global init for `%s'\n",
                 server->description);
        return;
      }

  /* Add this definition to the registered servers.  */
  if (servertypes == NULL)
    if ((servertypes = svz_array_create (1, NULL)) == NULL)
      return;
  svz_array_add (servertypes, server);
}

/*
 * Completely destroy the given server instance @var{server}.  This
 * especially means to go through each item of the server instances
 * configuration.
 */
static void
server_free (svz_server_t *server)
{
  svz_config_free (&server->type->config_prototype, server->cfg);
  svz_free (server->name);
  svz_free (server);
}

/*
 * Remove the given server instance @var{server} entirely from the list
 * of enqueued sockets.  This means to delete it from each server socket on
 * the one hand and to shutdown every child client spawned from this server
 * on the other hand.
 */
static void
server_unbind (svz_server_t *server)
{
  svz_socket_t *sock, *parent;

  /* Go through all enqueued sockets.  */
  svz_sock_foreach (sock)
    {
      /* Client structures.  */
      if (!(sock->flags & SVZ_SOFLG_LISTENING) &&
          (parent = svz_sock_getparent (sock)) != NULL)
        {
          /* If the parent of a client is the given servers child
             then also shutdown this client.  */
          if (parent->flags & SVZ_SOFLG_LISTENING && parent->port &&
              /* Is this check necessary?  Perhaps it is part of
                 ‘svz_binding_contains_server’?  */
              svz_sock_bindings (parent) &&
              svz_binding_contains_server (parent, server))
            svz_sock_schedule_for_shutdown (sock);
        }
    }

  /* Go through all enqueued sockets once more.  */
  svz_sock_foreach_listener (sock)
    {
      /* Delete the server and shutdown the socket structure if
         there are no more servers left.  */
      if (svz_sock_bindings_zonk_server (sock, server) == 0)
        svz_sock_schedule_for_shutdown (sock);
    }
}

/**
 * Find a servertype definition by its short name.  If @var{dynamic} is
 * set to non-zero, try to load a shared library that provides that
 * servertype.  Return @code{NULL} if no server with the given variable
 * prefix @var{name} has been found.
 */
svz_servertype_t *
svz_servertype_get (char *name, int dynamic)
{
  svz_servertype_t *stype;
  size_t n;

  /* first, try with already loaded ones */
  svz_array_foreach (servertypes, stype, n)
    {
      if (!strcmp (name, stype->prefix))
        return stype;
    }

  /* now, try dynamically */
  if (dynamic)
    {
      if (NULL != (stype = svz_servertype_load (name)))
        {
          svz_servertype_add (stype);
          return stype;
        }
    }

  return NULL;
}

/*
 * Run the global finalizers of each server type and delete all server
 * types.
 */
static void
servertype_finalize (void)
{
  size_t i;
  svz_servertype_t *stype;

  svz_log (SVZ_LOG_NOTICE, "running global server type finalizers\n");
  svz_array_foreach (servertypes, stype, i)
    {
      if (stype->global_finalize != NULL)
        if (stype->global_finalize (stype) < 0)
          svz_log (SVZ_LOG_ERROR, "error running global finalizer for `%s'\n",
                   stype->description);
    }
  if (servertypes != NULL)
    {
      svz_array_destroy (servertypes);
      servertypes = NULL;
    }
}

/**
 * Find a given server instances @var{server} server type.  Return
 * @code{NULL} if there is no such server type (which should never occur
 * since a server is a child of a server type).
 */
svz_servertype_t *
svz_servertype_find (svz_server_t *server)
{
  return server ? server->type : NULL;
}

struct find_closure
{
  void *cfg;
  svz_server_t *match;
};

static void
find_internal (svz_server_t *server, void *closure)
{
  struct find_closure *x = closure;

  if (x->cfg == server->cfg)
    x->match = server;
}

/**
 * Find a server instance by the given configuration structure @var{cfg}.
 * Return @code{NULL} if there is no such configuration in any server
 * instance.
 */
svz_server_t *
svz_server_find (void *cfg)
{
  struct find_closure x = { cfg, NULL };

  svz_foreach_server (find_internal, &x);
  return x.match;
}

/**
 * Return a list of clients (socket structures) which are associated
 * with the given server instance @var{server}.  If there is no such
 * socket, return @code{NULL}.  Caller should @code{svz_array_destroy}
 * the returned array.
 */
svz_array_t *
svz_server_clients (svz_server_t *server)
{
  svz_array_t *clients = svz_array_create (1, NULL);
  svz_socket_t *sock;

  /* go through all the socket list */
  svz_sock_foreach (sock)
    {
      /* and find clients of the server */
      if (!(sock->flags & SVZ_SOFLG_LISTENING))
        if (server->cfg == sock->cfg)
          svz_array_add (clients, sock);
    }
  return svz_array_destroy_zero (clients);
}

/**
 * Get the server instance with the given instance name @var{name}.
 * Return @code{NULL} if there is no such server yet.
 */
svz_server_t *
svz_server_get (char *name)
{
  if (servers == NULL || name == NULL)
    return NULL;
  return svz_hash_get (servers, name);
}

/*
 * Create a new server instance of the server type @var{stype} with the
 * instance name @var{name}.
 */
static svz_server_t *
instantiate_server (svz_servertype_t *stype, char *name)
{
  svz_server_t *server;

  /* Create server instance itself.  */
  server = (svz_server_t *) svz_malloc (sizeof (svz_server_t));
  server->name = svz_strdup (name);
  server->type = stype;
  server->data = NULL;

  /* Transfer callbacks.  */
  server->detect_proto = stype->detect_proto;
  server->connect_socket = stype->connect_socket;
  server->handle_request = stype->handle_request;
  server->init = stype->init;
  server->finalize = stype->finalize;
  server->info_client = stype->info_client;
  server->info_server = stype->info_server;
  server->notify = stype->notify;
  server->reset = stype->reset;
  server->description = stype->description;

  return server;
}

/*
 * This function configures a server instance by modifying its default
 * configuration by the @var{configure} callbacks.  Therefore you need
 * to pass the type of server in @var{server}, the @var{name} of the
 * server instance and the (optional) modifier callback structure
 * @var{configure}.  The @var{arg} argument is passed to each of the
 * callbacks (e.g. specifying a scheme cell).  The function returns
 * either a valid server instance configuration or @code{NULL} on
 * errors.
 */
static void *
configure_server (svz_servertype_t *server, char *name, void *arg,
                  svz_config_accessor_t *configure)
{
  void *cfg;

  cfg = svz_config_instantiate (&server->config_prototype, name, arg,
                                configure);
  return cfg;
}

/*
 * This function runs the server initilizer of the given server instance
 * @var{server} and returns zero on success.  Otherwise it emits an error
 * message and returns non-zero.
 */
static int
init_server (svz_server_t *server)
{
  if (server)
    if (server->init != NULL)
      if (server->init (server) < 0)
        {
          svz_log (SVZ_LOG_ERROR, "error initializing `%s'\n", server->name);
          return -1;
        }
  return 0;
}

static void
init_all_internal (svz_server_t *server, void *closure)
{
  int *errneous = closure;

  if (init_server (server) < 0)
    *errneous = -1;
}

/*
 * Run the initializers of all servers, return -1 if some server did not
 * think it is a good idea to run.
 */
static int
init_all (void)
{
  int errneous = 0;

  svz_log (SVZ_LOG_NOTICE, "initializing all server instances\n");
  svz_foreach_server (init_all_internal, &errneous);
  return errneous;
}

/*
 * This function runs the finalizer callback for the given server instance
 * @var{server}, removes all bindings and frees all resources allocated by
 * the server instance.
 */
static void
finalize_server (svz_server_t *server)
{
  if (server)
    {
      if (server->finalize != NULL)
        if (server->finalize (server) < 0)
          svz_log (SVZ_LOG_ERROR, "error finalizing `%s'\n", server->name);
      server_unbind (server);
      server_free (server);
    }
}

/*
 * Run the local finalizers for all server instances.
 */
static int
finalize_all (void)
{
  svz_log (SVZ_LOG_NOTICE, "running all server finalizers\n");
  svz_hash_destroy (servers);
  servers = NULL;
  servertype_finalize ();
  return 0;
}

/**
 * If @var{direction} is non-zero, run the initializers of all
 * servers, returning -1 if some server did not think it is a good
 * idea to run.  Otherwise, run the local finalizers for all
 * server instances.
 */
int
svz_updn_all_servers (int direction)
{
  return (direction
          ? init_all
          : finalize_all)
    ();
}

/*
 * This routine is the instantiating callback for a servertype as a
 * configurable type named @var{type}.  The @var{name} argument will
 * be the new servers instance name.  The @var{accessor} and
 * @var{options} arguments are passed to @code{configure_server}
 * without modifications.
 */
static int
instantiate_servertype (char *type, char *name, void *options,
                        svz_config_accessor_t *accessor,
                        size_t ebufsz, char *ebuf)
{
  svz_servertype_t *stype;
  svz_server_t *server;

  /* Find the definition by lookup with dynamic loading.  */
  if (NULL == (stype = svz_servertype_get (type, 1)))
    {
      snprintf (ebuf, ebufsz, "No such server type: `%s'", type);
      return -1;
    }

  /* Instantiate and configure this server.  */
  server = instantiate_server (stype, name);
  server->cfg = configure_server (stype, name, options, accessor);
  if (NULL == server->cfg && stype->config_prototype.size)
    {
      server_free (server);
      return -1;                /* Messages emitted by callbacks.  */
    }

  /* Add server configuration.  */
  if (svz_server_get (name) != NULL)
    {
      snprintf (ebuf, ebufsz, "Duplicate server definition: `%s'", name);
      server_free (server);
      return -1;
    }
  if (servers == NULL)
    servers = svz_hash_create (4, (svz_free_func_t) finalize_server);
  svz_hash_put (servers, server->name, server);

  return 0;
}

/*
 * The 'server' configurable type.
 */
svz_config_type_t svz_servertype_definition =
{
  "server",
  instantiate_servertype
};
