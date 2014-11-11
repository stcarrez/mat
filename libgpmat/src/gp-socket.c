/*  gp-socket.c -- Write probe information over a socket stream
--  Copyright (C) 2011, 2012, 2013, 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
*/
#include "gp-config.h"
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "gp-remote.h"
#include "gp-file.h"

#ifndef O_CLOEXEC
# define O_CLOEXEC 0
#endif

struct gp_socket_server 
{
  struct gp_buffered_server root;
  int fd;
};

static struct gp_socket_server server;

/**
 * @brief Flush the data stored in the buffer.
 *
 * @param file the GP server instance.
 * @return 0 if the operation succeeded.
 */
static int gp_socket_flush (struct gp_buffered_server* server)
{
  struct gp_socket_server* sock = (struct gp_socket_server*) server;
  size_t sz = sock->root.write_ptr - sock->root.buffer;
  ssize_t res = write (sock->fd, sock->root.buffer, sz);
  if (res != sz)
    {
      close (sock->fd);
      sock->fd = -1;
      return -1;
    }
  sock->root.write_ptr = sock->root.buffer;
  return 0;
}

/**
 * @brief Close the GP server file.
 *
 * @param server the GP server instance.
 */
void gp_socket_close (struct gp_server* server)
{
  struct gp_socket_server* sock = (struct gp_socket_server*) server;

  if (sock->fd < 0)
    return;

  if (gp_socket_flush ((struct gp_buffered_server*) sock) == 0)
    close (sock->fd);
  sock->fd = -1;
}

/**
 * @brief Open the socket and prepare for probe monitoring to a remote TCP/IP port.
 *
 * The parameter string has the following format:
 *
 * tcp://host:port
 *
 * @param param the TCP/IP server to connect.
 * @return the GP server instance.
 */
struct gp_socket_server* gp_socket_open (const char* param)
{
  char host[PATH_MAX];
  char* s;
  struct sockaddr_in sinaddr;
  int on = 1;
  int port;
  struct in_addr ip;

  strncpy (host, param, sizeof (host));
  s = strchr (host, ':');
  if (s != NULL)
    {
      *s = 0;
      port = strtol (s + 1, &s, 10);
    }
  else
    {
      port = DEFAULT_TCP_PORT;
      s = host;
    }
  s = strchr (s, '?');
  if (s != NULL)
    {
      *s = 0;
    }

  if (inet_aton (host, &ip) == 0)
    {
      return NULL;
    }
  
  server.fd = socket (PF_INET, SOCK_STREAM, 0);
  if (server.fd < 0)
    {
      return NULL;
    }
  
  memset (&sinaddr, 0, sizeof (struct sockaddr_in));
  sinaddr.sin_family = AF_INET;
  sinaddr.sin_port = htons (port);
  sinaddr.sin_addr = ip;
  if (connect (server.fd, (struct sockaddr *) &sinaddr, sizeof (struct sockaddr_in)) < 0)
    {
      close (server.fd);
      return NULL;
    }
  setsockopt (server.fd, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof(on));
  gp_buffered_server_initialize (&server.root);
  server.root.to_flush = gp_socket_flush;
  server.root.root.to_close = gp_socket_close;
  unsetenv ("GP_SERVER");
  return &server;
}
