/*  mat-socket.c -- Write probe information over a socket stream
--  Copyright (C) 2011, 2012, 2013, 2014, 2021 Stephane Carrez
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
#include "mat-config.h"
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <fcntl.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#include <string.h>
#include <limits.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "mat-remote.h"
#include "mat-socket.h"

#ifndef SOCK_CLOEXEC
# define SOCK_CLOEXEC 0
# define SET_CLOEXEC(fd) fcntl (fd, F_SETFD, FD_CLOEXEC);
#else
# define SET_CLOEXEC(fd)
#endif

/**
 * @brief Flush the data stored in the buffer.
 *
 * @param file the GP server instance.
 * @return 0 if the operation succeeded.
 */
static int mat_socket_flush (struct mat_buffered_server* server)
{
  struct mat_socket_server* sock = (struct mat_socket_server*) server;
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
void mat_socket_close (struct mat_server* server)
{
  struct mat_socket_server* sock = (struct mat_socket_server*) server;

  if (sock->fd < 0)
    return;

  if (mat_socket_flush ((struct mat_buffered_server*) sock) == 0)
    close (sock->fd);
  sock->fd = -1;
}

/**
 * @brief Open the socket and prepare for probe monitoring to a remote TCP/IP port.
 *
 * The parameter string has the following format:
 *
 * tcp://host:port[?sync]
 *
 * @param server the socket server instance to initialize.
 * @param param the TCP/IP server to connect.
 * @return the GP server instance.
 */
struct mat_socket_server* mat_socket_open (struct mat_socket_server* server, const char* param)
{
  char host[PATH_MAX];
  char* s;
  struct sockaddr_in sinaddr;
  int on = 1;
  int port;
  struct in_addr ip;

  strncpy (host, param, sizeof (host) - 1);
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
      s++;
    }

  if (inet_aton (host, &ip) == 0)
    {
      return NULL;
    }
  
  server->fd = socket (PF_INET, SOCK_STREAM | SOCK_CLOEXEC, 0);
  if (server->fd < 0)
    {
      return NULL;
    }
  
  memset (&sinaddr, 0, sizeof (struct sockaddr_in));
  sinaddr.sin_family = AF_INET;
  sinaddr.sin_port = htons (port);
  sinaddr.sin_addr = ip;
  if (connect (server->fd, (struct sockaddr *) &sinaddr, sizeof (struct sockaddr_in)) < 0)
    {
      close (server->fd);
      return NULL;
    }
  setsockopt (server->fd, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof(on));
  SET_CLOEXEC (server->fd);

  mat_buffered_server_initialize (&server->root, s);
  server->root.to_flush = mat_socket_flush;
  server->root.root.to_close = mat_socket_close;
  unsetenv ("MAT_SERVER");
  return server;
}
