/*
 * portcfg.c - port configuration implementation
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
#include "timidity.h"
#include <string.h>
#include <sys/types.h>

#ifndef __MINGW32__
# include <sys/socket.h>
#endif

#include "networking-headers.h"
#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/core.h"
#include "libserveez/array.h"
#include "libserveez/portcfg.h"
#include "libserveez/udp-socket.h"
#include "libserveez/icmp-socket.h"
#include "libserveez/pipe-socket.h"
#include "libserveez/interface.h"
#include "misc-macros.h"

/* How much data is accepted before valid detection.  */
#define SOCK_MAX_DETECTION_FILL 16
/* How much time is accepted before valid detection.  */
#define SOCK_MAX_DETECTION_WAIT 30

static int
any_p (const char *addr)
{
  return !strcmp (SVZ_PORTCFG_ANY, addr);
}

static int
no_ip_p (const char *addr)
{
  return !strcmp (SVZ_PORTCFG_NOIP, addr);
}

/*
 * This hash holds all port configurations created by the configuration
 * file.
 */
static svz_hash_t *portcfgs = NULL;

/**
 * Return the pointer of the @code{sockaddr_in} structure of the given
 * port configuration @var{port} if it is a network port configuration.
 * Otherwise return @code{NULL}.
 */
struct sockaddr_in *
svz_portcfg_addr (svz_portcfg_t *port)
{
#define SIMPLE(up,dn)                                           \
  case SVZ_PROTO_ ## up :  return &port->protocol. dn .addr

  switch (port->proto)
    {
      SIMPLE (TCP, tcp);
      SIMPLE (UDP, udp);
      SIMPLE (ICMP, icmp);
      SIMPLE (RAW, raw);
    default: return NULL;
    }
#undef SIMPLE
}

/**
 * Return the pointer to the ip address @code{ipaddr} of the given
 * port configuration @var{port} if it is a network port configuration.
 * Otherwise return @code{NULL}.
 */
char *
svz_portcfg_ipaddr (svz_portcfg_t *port)
{
#define SIMPLE(up,dn)                                           \
  case SVZ_PROTO_ ## up :  return port->protocol. dn .ipaddr

  switch (port->proto)
    {
      SIMPLE (TCP, tcp);
      SIMPLE (UDP, udp);
      SIMPLE (ICMP, icmp);
      SIMPLE (RAW, raw);
    default: return NULL;
    }
#undef SIMPLE
}

/**
 * Return the network device name stored in the given port
 * configuration @var{port} if it is a network port configuration.
 * Return @code{NULL} if there is no such device set
 * or if the port configuration is not a network port configuration.
 */
char *
svz_portcfg_device (svz_portcfg_t *port)
{
#define SIMPLE(up,dn)                                           \
  case SVZ_PROTO_ ## up :  return port->protocol. dn .device

  switch (port->proto)
    {
      SIMPLE (TCP, tcp);
      SIMPLE (UDP, udp);
      SIMPLE (ICMP, icmp);
      SIMPLE (RAW, raw);
    default: return NULL;
    }
#undef SIMPLE
}

/**
 * Return the UDP or TCP port of the given port configuration or zero
 * if it neither TCP nor UDP.
 */
in_port_t
svz_portcfg_port (svz_portcfg_t *port)
{
#define SIMPLE(up,dn)                                           \
  case SVZ_PROTO_ ## up :  return port->protocol. dn .port

  switch (port->proto)
    {
      SIMPLE (TCP, tcp);
      SIMPLE (UDP, udp);
    default: return 0;
    }
#undef SIMPLE
}

/**
 * Create a new blank port configuration.
 */
svz_portcfg_t *
svz_portcfg_create (void)
{
  svz_portcfg_t *port = svz_calloc (sizeof (svz_portcfg_t));
  return port;
}

/*
 * Return 1 if the devices of port-configs A and B are the same, else 0.
 */
static int
same_devices (svz_portcfg_t *a, svz_portcfg_t *b)
{
  char *sa = svz_portcfg_device (a);
  char *sb = svz_portcfg_device (b);

  /* Apparently, ‘svz_portcfg_device’ can return NULL,
     which elicits a warning from gcc for ‘strcmp’.  */
  return !strcmp (sa ? sa : "",
                  sb ? sb : "");
}

/**
 * Check if two given port configurations structures are equal, i.e.
 * specifying the same network port or pipe files.  Return
 * @code{SVZ_PORTCFG_EQUAL} if @var{a} and @var{b} are identical,
 * @code{SVZ_PORTCFG_MATCH} if the network address of either port
 * configuration contains the other (INADDR_ANY match), and otherwise
 * @code{SVZ_PORTCFG_NOMATCH} or possibly @code{SVZ_PORTCFG_CONFLICT}.
 */
int
svz_portcfg_equal (svz_portcfg_t *a, svz_portcfg_t *b)
{
  struct sockaddr_in *a_addr, *b_addr;

  if ((a->proto & (SVZ_PROTO_TCP | SVZ_PROTO_UDP |
                   SVZ_PROTO_ICMP | SVZ_PROTO_RAW)) &&
      (a->proto == b->proto))
    {
      /* Two network ports are equal if both local port and IP address
         are equal or one of them is INADDR_ANY.  */
      a_addr = svz_portcfg_addr (a);
      b_addr = svz_portcfg_addr (b);

      switch (a->proto)
        {
        case SVZ_PROTO_UDP:
        case SVZ_PROTO_TCP:
          if (a_addr->sin_port == b_addr->sin_port)
            {
              if ((a->flags & PORTCFG_FLAG_DEVICE) ||
                  (b->flags & PORTCFG_FLAG_DEVICE))
                {
                  if ((a->flags & PORTCFG_FLAG_DEVICE) &&
                      (b->flags & PORTCFG_FLAG_DEVICE))
                    {
                      if (same_devices (a, b))
                        return SVZ_PORTCFG_EQUAL;
                      return SVZ_PORTCFG_NOMATCH;
                    }
                  return SVZ_PORTCFG_CONFLICT;
                }
              if (a_addr->sin_addr.s_addr == b_addr->sin_addr.s_addr)
                return SVZ_PORTCFG_EQUAL;
              if (a->flags & PORTCFG_FLAG_ANY || b->flags & PORTCFG_FLAG_ANY)
                return SVZ_PORTCFG_MATCH;
            }
          break;
        case SVZ_PROTO_ICMP:
          if (SVZ_CFG_ICMP (a, type) == SVZ_CFG_ICMP (b, type))
            {
              if ((a->flags & PORTCFG_FLAG_DEVICE) ||
                  (b->flags & PORTCFG_FLAG_DEVICE))
                {
                  if ((a->flags & PORTCFG_FLAG_DEVICE) &&
                      (b->flags & PORTCFG_FLAG_DEVICE) &&
                      same_devices (a, b))
                    return SVZ_PORTCFG_EQUAL;
                  return SVZ_PORTCFG_CONFLICT;
                }
              if (a_addr->sin_addr.s_addr == b_addr->sin_addr.s_addr)
                return SVZ_PORTCFG_EQUAL;
              if (a->flags & PORTCFG_FLAG_ANY || b->flags & PORTCFG_FLAG_ANY)
                return SVZ_PORTCFG_MATCH;
            }
          break;
        case SVZ_PROTO_RAW:
          if ((a->flags & PORTCFG_FLAG_DEVICE) ||
              (b->flags & PORTCFG_FLAG_DEVICE))
            {
              if ((a->flags & PORTCFG_FLAG_DEVICE) &&
                  (b->flags & PORTCFG_FLAG_DEVICE) &&
                  same_devices (a, b))
                return SVZ_PORTCFG_EQUAL;
              return SVZ_PORTCFG_CONFLICT;
            }
          if (a_addr->sin_addr.s_addr == b_addr->sin_addr.s_addr)
            return SVZ_PORTCFG_EQUAL;
          if (a->flags & PORTCFG_FLAG_ANY || b->flags & PORTCFG_FLAG_ANY)
            return SVZ_PORTCFG_MATCH;
          break;
        }
    }
  else if (a->proto & SVZ_PROTO_PIPE && a->proto == b->proto)
    {
#define XNAME(p,member)  SVZ_CFG_PIPE (p, member).name
      /*
       * Two pipe configs are equal if they use the same files.
       */
      if (!strcmp (XNAME (a, recv), XNAME (b, recv)) &&
          !strcmp (XNAME (a, send), XNAME (b, send)))
        return SVZ_PORTCFG_EQUAL;
#undef XNAME
    }

  /* Do not even the same proto flag -> cannot be equal.  */
  return SVZ_PORTCFG_NOMATCH;
}

/**
 * Add the given port configuration @var{port} associated with the name
 * @var{name} to the list of known port configurations.  Return @code{NULL}
 * on errors.  If the return port configuration equals the given port
 * configuration the given one has been successfully added.
 */
svz_portcfg_t *
svz_portcfg_add (char *name, svz_portcfg_t *port)
{
  svz_portcfg_t *replace;

  /* Do not handle invalid arguments.  */
  if (name == NULL || port == NULL)
    return NULL;

  /* Check if the port configuration hash is inited.  */
  if (portcfgs == NULL)
    {
      if ((portcfgs = svz_hash_create (4, (svz_free_func_t)
                                           svz_portcfg_free)) == NULL)
        return NULL;
    }

  /* Try adding a new port configuration.  */
  if ((replace = svz_hash_get (portcfgs, name)) != NULL)
    {
#if ENABLE_DEBUG
      svz_log (SVZ_LOG_DEBUG, "portcfg `%s' already registered\n", name);
#endif
      svz_hash_put (portcfgs, name, port);
      return replace;
    }
  svz_hash_put (portcfgs, name, port);
  return port;
}

/*
 * This function can be used to set the character string representation
 * of a the port configuration @var{this} in dotted decimal form
 * (@var{ipaddr}).  Returns zero on success, non-zero otherwise.
 */
static int
set_ipaddr (svz_portcfg_t *this, char *ipaddr)
{
  if (!this || !ipaddr)
    return -1;

  switch (this->proto)
    {
    case SVZ_PROTO_TCP:
      svz_free_and_zero (SVZ_CFG_TCP (this, ipaddr));
      SVZ_CFG_TCP (this, ipaddr) = ipaddr;
      break;
    case SVZ_PROTO_UDP:
      svz_free_and_zero (SVZ_CFG_UDP (this, ipaddr));
      SVZ_CFG_UDP (this, ipaddr) = ipaddr;
      break;
    case SVZ_PROTO_ICMP:
      svz_free_and_zero (SVZ_CFG_ICMP (this, ipaddr));
      SVZ_CFG_ICMP (this, ipaddr) = ipaddr;
      break;
    case SVZ_PROTO_RAW:
      svz_free_and_zero (SVZ_CFG_RAW (this, ipaddr));
      SVZ_CFG_RAW (this, ipaddr) = ipaddr;
      break;
    default:
      return -1;
    }
  return 0;
}

struct expand_closure
{
  svz_portcfg_t *this;
  svz_array_t *ports;
};

static int
expand_internal (const svz_interface_t *ifc, void *closure)
{
  in_addr_t ipaddr;
  struct expand_closure *x = closure;
  svz_portcfg_t *port = svz_portcfg_dup (x->this);
  struct sockaddr_in *addr = svz_portcfg_addr (port);

  svz_address_to (&ipaddr, ifc->addr);
  addr->sin_addr.s_addr = ipaddr;
  set_ipaddr (port, svz_strdup (svz_inet_ntoa (ipaddr)));
  svz_array_add (x->ports, port);
  return 0;
}

/*
 * Expand the given port configuration @var{this} if it is a network port
 * configuration and if the network ip address is @code{INADDR_ANY}.  Return
 * an array of port configurations which are copies of the given.
 */
svz_array_t *
svz_portcfg_expand (svz_portcfg_t *this)
{
  struct expand_closure x = { this, svz_array_create (1, NULL) };

  /* Is this a network port configuration and should it be expanded?  */
  if (NULL != svz_portcfg_addr (this)
      && (this->flags & PORTCFG_FLAG_ALL)
      && !(this->flags & PORTCFG_FLAG_DEVICE))
    svz_foreach_interface (expand_internal, &x);
  /* No, just add the given port configuration.  */
  else
    svz_array_add (x.ports, svz_portcfg_dup (this));
  return x.ports;
}

/**
 * Make a copy of the given port configuration @var{port}.
 */
svz_portcfg_t *
svz_portcfg_dup (svz_portcfg_t *port)
{
  svz_portcfg_t *copy;

  /* Return NULL if necessary.  */
  if (port == NULL)
    return NULL;

  /* First plain copy.  */
  copy = svz_malloc (sizeof (svz_portcfg_t));
  memcpy (copy, port, sizeof (svz_portcfg_t));
  copy->name = svz_strdup (port->name);

  /* Depending on the protocol, copy various strings.  */
  switch (port->proto)
    {
    case SVZ_PROTO_TCP:
      SVZ_CFG_TCP (copy, ipaddr) = svz_strdup (SVZ_CFG_TCP (port, ipaddr));
      SVZ_CFG_TCP (copy, device) = svz_strdup (SVZ_CFG_TCP (port, device));
      break;
    case SVZ_PROTO_UDP:
      SVZ_CFG_UDP (copy, ipaddr) = svz_strdup (SVZ_CFG_UDP (port, ipaddr));
      SVZ_CFG_UDP (copy, device) = svz_strdup (SVZ_CFG_UDP (port, device));
      break;
    case SVZ_PROTO_ICMP:
      SVZ_CFG_ICMP (copy, ipaddr) = svz_strdup (SVZ_CFG_ICMP (port, ipaddr));
      SVZ_CFG_ICMP (copy, device) = svz_strdup (SVZ_CFG_ICMP (port, device));
      break;
    case SVZ_PROTO_RAW:
      SVZ_CFG_RAW (copy, ipaddr) = svz_strdup (SVZ_CFG_RAW (port, ipaddr));
      SVZ_CFG_RAW (copy, device) = svz_strdup (SVZ_CFG_RAW (port, device));
      break;
    case SVZ_PROTO_PIPE:
#define COPY(x)                                                         \
      SVZ_CFG_PIPE (copy, x) = svz_strdup (SVZ_CFG_PIPE (port, x))

      COPY (recv.name);
      COPY (recv.user);
      COPY (recv.group);
      COPY (send.name);
      COPY (send.user);
      COPY (send.group);
      break;
#undef COPY
    }

  copy->accepted = NULL;

  /* Make a copy of the "deny" and "allow" access lists.  */
  copy->allow = svz_array_strdup (port->allow);
  copy->deny = svz_array_strdup (port->deny);

  return copy;
}

/*
 * This function frees all resources allocated by the given port
 * configuration @var{port}.
 */
void
svz_portcfg_free (svz_portcfg_t *port)
{
  /* Free the name of the port configuration.  */
  svz_free (port->name);

  /* Depending on the type of configuration perform various operations.  */
  switch (port->proto)
    {
    case SVZ_PROTO_TCP:
      svz_free (SVZ_CFG_TCP (port, ipaddr));
      svz_free (SVZ_CFG_TCP (port, device));
      break;
    case SVZ_PROTO_UDP:
      svz_free (SVZ_CFG_UDP (port, ipaddr));
      svz_free (SVZ_CFG_UDP (port, device));
      break;
    case SVZ_PROTO_ICMP:
      svz_free (SVZ_CFG_ICMP (port, ipaddr));
      svz_free (SVZ_CFG_ICMP (port, device));
      break;
    case SVZ_PROTO_RAW:
      svz_free (SVZ_CFG_RAW (port, ipaddr));
      svz_free (SVZ_CFG_RAW (port, device));
      break;
    case SVZ_PROTO_PIPE:
#define FREE(x)                                 \
      svz_free (SVZ_CFG_PIPE (port, x))

      FREE (recv.user);
      FREE (recv.name);
      FREE (recv.group);
      FREE (send.user);
      FREE (send.name);
      FREE (send.group);
      break;
#undef FREE
    }

  /* Destroy access and connection list.  */
  if (port->deny)
    {
      svz_array_destroy (port->deny);
      port->deny = NULL;
    }
  if (port->allow)
    {
      svz_array_destroy (port->allow);
      port->allow = NULL;
    }
  if (port->accepted)
    {
      svz_hash_destroy (port->accepted);
      port->accepted = NULL;
    }

  /* Free the port configuration itself.  */
  svz_free (port);
}

/**
 * Make the given port configuration @var{port} completely unusable,
 * removing it from the list of known port configurations.
 * Do nothing if @var{port} is @code{NULL}.
 */
void
svz_portcfg_destroy (svz_portcfg_t *port)
{
  char *name;

  /* Return here if NULL pointer given.  */
  if (port == NULL)
    return;

  /* Delete from port configuration hash if necessary.  */
  if (portcfgs && (name = svz_hash_contains (portcfgs, port)) != NULL)
    svz_hash_delete (portcfgs, name);

  /* Free the port configuration.  */
  svz_portcfg_free (port);
}

/**
 * Return the port configuration associated with the given name @var{name}.
 * Return @code{NULL} on errors.
 */
svz_portcfg_t *
svz_portcfg_get (char *name)
{
  /* Do not handle invalid arguments.  */
  if (name == NULL || portcfgs == NULL)
    return NULL;

  return svz_hash_get (portcfgs, name);
}

/*
 * Delete the list of known port configurations.  This routine should
 * definitely called from the core library's finalizer.
 */
void
svz_portcfg_finalize (void)
{
  if (portcfgs != NULL)
    {
      svz_hash_destroy (portcfgs);
      portcfgs = NULL;
    }
}

/*
 * Converts the given network address @var{str} either given in dotted
 * decimal form or as network interface name and saves the result in the
 * @code{sockaddr_in.sin_addr.s_addr} field.  Return zero on success.
 */
static int
convert_addr (char *str, struct sockaddr_in *addr)
{
  svz_interface_t *ifc;

  if ((ifc = svz_interface_search (str)) != NULL)
    {
#if ENABLE_DEBUG
      char buf[64];

      svz_log (SVZ_LOG_DEBUG, "`%s' is %s\n", ifc->description,
               SVZ_PP_ADDR (buf, ifc->addr));
#endif
      svz_address_to (&addr->sin_addr.s_addr, ifc->addr);
      return 0;
    }
  return svz_inet_aton (str, addr);
}

/**
 * Construct the @code{sockaddr_in} fields from the @code{ipaddr} field.
 * Return zero if it worked.  If it does not work, the @code{ipaddr} field
 * did not consist of an ip address in dotted decimal form.
 */
int
svz_portcfg_mkaddr (svz_portcfg_t *this)
{
  struct sockaddr_in *sa;
  char *ip;
  int err = 0;

  switch (this->proto)
    {
      /* For all network protocols we assign AF_INET as protocol family,
         determine the network port (if necessary) and put the ip address.  */
    case SVZ_PROTO_TCP:
      ip = SVZ_CFG_TCP (this, ipaddr);
      sa = &SVZ_CFG_TCP (this, addr);
      sa->sin_family = AF_INET;
      if (svz_portcfg_device (this))
        {
          this->flags |= PORTCFG_FLAG_DEVICE;
          sa->sin_addr.s_addr = INADDR_ANY;
        }
      else if (ip == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "%s: no TCP/IP address given\n", this->name);
          err = -1;
        }
      else if (any_p (ip))
        {
          this->flags |= PORTCFG_FLAG_ANY;
          sa->sin_addr.s_addr = INADDR_ANY;
        }
      else if (no_ip_p (ip))
        {
          this->flags |= PORTCFG_FLAG_ALL;
          sa->sin_addr.s_addr = INADDR_ANY;
        }
      else
        {
          err = convert_addr (ip, sa);
          if (err)
            {
              svz_log (SVZ_LOG_ERROR, "%s: `%s' is not a valid IP address\n",
                       this->name, ip);
            }
        }
      sa->sin_port = htons (SVZ_CFG_TCP (this, port));
      if (SVZ_CFG_TCP (this, backlog) > SOMAXCONN)
        {
          svz_log (SVZ_LOG_ERROR, "%s: TCP backlog out of range (1..%d)\n",
                   this->name, SOMAXCONN);
          err = -1;
        }
      break;
    case SVZ_PROTO_UDP:
      ip = SVZ_CFG_UDP (this, ipaddr);
      sa = &SVZ_CFG_UDP (this, addr);
      sa->sin_family = AF_INET;
      if (svz_portcfg_device (this))
        {
          this->flags |= PORTCFG_FLAG_DEVICE;
          sa->sin_addr.s_addr = INADDR_ANY;
        }
      else if (ip == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "%s: no UDP/IP address given\n", this->name);
          err = -1;
        }
      else if (any_p (ip))
        {
          this->flags |= PORTCFG_FLAG_ANY;
          sa->sin_addr.s_addr = INADDR_ANY;
        }
      else if (no_ip_p (ip))
        {
          this->flags |= PORTCFG_FLAG_ALL;
          sa->sin_addr.s_addr = INADDR_ANY;
        }
      else
        {
          err = convert_addr (ip, sa);
          if (err)
            {
              svz_log (SVZ_LOG_ERROR, "%s: `%s' is not a valid IP address\n",
                       this->name, ip);
            }
        }
      sa->sin_port = htons (SVZ_CFG_UDP (this, port));
      break;
    case SVZ_PROTO_ICMP:
      ip = SVZ_CFG_ICMP (this, ipaddr);
      sa = &SVZ_CFG_ICMP (this, addr);
      if (svz_portcfg_device (this))
        {
          this->flags |= PORTCFG_FLAG_DEVICE;
          sa->sin_addr.s_addr = INADDR_ANY;
        }
      else if (ip == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "%s: no ICMP/IP address given\n", this->name);
          err = -1;
        }
      else
        {
          err = convert_addr (ip, sa);
          if (err)
            {
              svz_log (SVZ_LOG_ERROR, "%s: `%s' is not a valid IP address\n",
                       this->name, ip);
            }
        }
      sa->sin_family = AF_INET;
      break;
    case SVZ_PROTO_RAW:
      sa = &SVZ_CFG_RAW (this, addr);
      if (svz_portcfg_device (this))
        {
          this->flags |= PORTCFG_FLAG_DEVICE;
          sa->sin_addr.s_addr = INADDR_ANY;
        }
      else if (SVZ_CFG_RAW (this, ipaddr) == NULL)
        {
          svz_log (SVZ_LOG_ERROR, "%s: no IP address given\n", this->name);
          err = -1;
        }
      else
        {
          err = convert_addr (SVZ_CFG_RAW (this, ipaddr), sa);
          if (err)
            {
              svz_log (SVZ_LOG_ERROR, "%s: `%s' is not a valid IP address\n",
                       this->name, SVZ_CFG_RAW (this, ipaddr));
            }
        }
      sa->sin_family = AF_INET;
      break;
      /* The pipe protocol needs a check for the validity of the permissions,
         the group and user names and its id's.  */
    case SVZ_PROTO_PIPE:
      {
        svz_pipe_t *r = &SVZ_CFG_PIPE (this, recv);
        svz_pipe_t *s = &SVZ_CFG_PIPE (this, send);

        if (r->name == NULL)
          {
            svz_log (SVZ_LOG_ERROR, "%s: no receiving pipe file given\n",
                     this->name);
            err = -1;
          }
        else
          {
            err |= svz_pipe_check_user (r);
            err |= svz_pipe_check_group (r);
          }
        if (s->name == NULL)
          {
            svz_log (SVZ_LOG_ERROR, "%s: no sending pipe file given\n",
                     this->name);
            err = -1;
          }
        else
          {
            err |= svz_pipe_check_user (s);
            err |= svz_pipe_check_group (s);
          }
      }
      break;
    default:
      err = 0;
    }
  return err;
}

/*
 * Prepare the given port configuration @var{port}.  Fill in default values
 * for yet undefined variables.
 */
void
svz_portcfg_prepare (svz_portcfg_t *port)
{
  /* Check the TCP backlog value.  */
  if (port->proto & SVZ_PROTO_TCP)
    {
      if (SVZ_CFG_TCP (port, backlog) <= 0
          || SVZ_CFG_TCP (port, backlog) > SOMAXCONN)
        SVZ_CFG_TCP (port, backlog) = SOMAXCONN;
    }
  /* Check the detection barriers for pipe and tcp sockets.  */
  if (port->proto & (SVZ_PROTO_PIPE | SVZ_PROTO_TCP))
    {
      if (port->detection_fill <= 0 ||
          port->detection_fill > SOCK_MAX_DETECTION_FILL)
        port->detection_fill = SOCK_MAX_DETECTION_FILL;
      if (port->detection_wait <= 0 ||
          port->detection_wait > SOCK_MAX_DETECTION_WAIT)
        port->detection_wait = SOCK_MAX_DETECTION_WAIT;
    }
  /* Check the initial send and receive buffer sizes.  */
  if (port->send_buffer_size <= 0 ||
      port->send_buffer_size >= MAX_BUF_SIZE)
    {
      if (port->proto & (SVZ_PROTO_TCP | SVZ_PROTO_PIPE))
        port->send_buffer_size = SEND_BUF_SIZE;
      else if (port->proto & SVZ_PROTO_UDP)
        port->send_buffer_size = SVZ_UDP_BUF_SIZE;
      else if (port->proto & (SVZ_PROTO_ICMP | SVZ_PROTO_RAW))
        port->send_buffer_size = ICMP_BUF_SIZE;
    }
  if (port->recv_buffer_size <= 0 ||
      port->recv_buffer_size >= MAX_BUF_SIZE)
    {
      if (port->proto & (SVZ_PROTO_TCP | SVZ_PROTO_PIPE))
        port->recv_buffer_size = RECV_BUF_SIZE;
      else if (port->proto & SVZ_PROTO_UDP)
        port->recv_buffer_size = SVZ_UDP_BUF_SIZE;
      else if (port->proto & (SVZ_PROTO_ICMP | SVZ_PROTO_RAW))
        port->recv_buffer_size = ICMP_BUF_SIZE;
    }
  /* Check the connection frequency.  */
  if (port->connect_freq <= 0)
    {
      /* Sane value is: 100 connections per second.  */
      port->connect_freq = 100;
    }
}

/*
 * Helper function for the below routine.  Converts a Internet network
 * address or the appropiate device into a text representation.
 */
static char *
text_from_addr (svz_portcfg_t *port, struct sockaddr_in *addr)
{
  if (port->flags & PORTCFG_FLAG_DEVICE)
    return svz_portcfg_device (port);
  else if (port->flags & PORTCFG_FLAG_ANY)
    return "*";
  return svz_inet_ntoa (addr->sin_addr.s_addr);
}

/*
 * Format a text representation of the port configuration @var{port} into
 * @var{buf}, which has @var{size} bytes.  The string is guaranteed to be
 * nul-terminated.  Return the length (at most @code{@var{size} - 1}) of
 * the formatted string.
 */
size_t
svz_pp_portcfg (char *buf, size_t size, svz_portcfg_t *port)
{
  struct sockaddr_in *addr;
  int len;

  /* TCP and UDP */
  if (port->proto & (SVZ_PROTO_TCP | SVZ_PROTO_UDP))
    {
      addr = svz_portcfg_addr (port);
      len = snprintf (buf, size, "%s:[%s:%d]",
                      (port->proto & SVZ_PROTO_TCP) ? "TCP" : "UDP",
                      text_from_addr (port, addr),
                      ntohs (addr->sin_port));
    }
  /* RAW and ICMP */
  else if (port->proto & (SVZ_PROTO_RAW | SVZ_PROTO_ICMP))
    {
      int icmp_p = port->proto & SVZ_PROTO_ICMP;

      addr = svz_portcfg_addr (port);
      len = snprintf (buf, size, "%s:[%s%s%s]",
                      (port->proto & SVZ_PROTO_RAW) ? "RAW" : "ICMP",
                      text_from_addr (port, addr),
                      icmp_p ? "/" : "",
                      icmp_p ? svz_itoa (SVZ_CFG_ICMP (port, type)) : "");
    }
  /* PIPE */
  else if (port->proto & SVZ_PROTO_PIPE)
    len = snprintf (buf, size, "PIPE:[%s]-[%s]",
                    SVZ_CFG_PIPE (port, recv.name),
                    SVZ_CFG_PIPE (port, send.name));
  else
    {
      buf[0] = '\0';
      len = 0;
    }
  return len;
}
