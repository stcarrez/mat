/*  gp-remote.c -- Remote access
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
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "gp-probe.h"
#include "gp-events.h"
#include "gp-file.h"
#include "gp-socket.h"
#include "shm-channel.h"

static struct gp_server* server;

#ifdef DEBUG
static void
to_hex (char* buf, unsigned byte)
{
  buf[0] = "0123456789ABCDEF"[(byte >> 4) & 0x0F];
  buf[1] = "0123456789ABCDEF"[byte & 0x0F];
}

void
gp_dump (const char* title, int indent, const void* addr, size_t len)
{
  char buf[256];
  unsigned char* ptr = (unsigned char*) addr;

  if (indent > 0)
    {
      memset (buf, ' ', indent);
      write (STDERR_FILENO, buf, indent);
    }
  write (STDERR_FILENO, title, strlen (title));
  write (STDERR_FILENO, ": ", 2);
  while (len != 0) {
    to_hex(buf, *ptr);
    ptr++;
    len--;
    write (STDERR_FILENO, buf, 2);
  }
  write (STDERR_FILENO, " ", 1);
}

void
gp_write (const char* title, int indent, const void* addr, size_t len)
{
  gp_dump (title, indent, addr, len);
  gp_remote_send (addr, len);
}

#endif

/**
 * @brief Send the content through an internal buffer.
 *
 * @param server the GP server instance.
 * @param ptr the data to send.
 * @param len the number of bytes to send.
 */
void gp_buffered_send (struct gp_server* server, const void* ptr, size_t len)
{
  struct gp_buffered_server* buffer = (struct gp_buffered_server*) server;

  while (len > 0)
    {
      size_t avail = buffer->last_ptr - buffer->write_ptr;
      if (avail > len)
        avail = len;

      if (avail > 0)
        {
          memcpy (buffer->write_ptr, ptr, avail);
          buffer->write_ptr += avail;
          ptr = ((unsigned char*) ptr) + avail;
          len -= avail;
        }
      else
        {
          if (buffer->to_flush (buffer) != 0)
            break;
        }
    }
}

/**
 * @brief Synchronize with the GP server.
 *
 * Flush the pending data.
 *
 * @param server the GP server instance.
 * @return 0 if the operation succeeded or an error code.
 */
int gp_buffered_synchronize (struct gp_server* server)
{
  struct gp_buffered_server* buffer = (struct gp_buffered_server*) server;

  return buffer->to_flush (buffer);
}

void
gp_remote_send (const void *addr, size_t len)
{
  if (server)
    {
      server->to_send (server, addr, len);
    }
}

void
gp_remote_close (void)
{
  if (server)
    {
      server->to_close (server);
    }
}

void
gp_remote_sync (void)
{
  if (server && server->to_synchronize)
    {
      server->to_synchronize (server);
    }
}

/**
 * @brief Initialize the buffered server instance.
 *
 * @param server the server instance.
 */
void
gp_buffered_server_initialize (struct gp_buffered_server* server)
{
  server->write_ptr = server->buffer;
  server->last_ptr  = &server->buffer[sizeof (server->buffer)];
  server->root.to_send        = gp_buffered_send;
  server->root.to_synchronize = gp_buffered_synchronize;
}

/**
 * @brief Initialize the connection to the server.
 *
 * file://<pattern>     Write the probe stream in a file.
 * tcp://host:port      Send the probe stream to the TCP/IP server.
 *
 * @return 0
 */
int
gp_remote_initialize (void)
{
  char* p = getenv("MAT_SERVER");
  if (p != NULL)
    {
      if (strncmp (p, "file://", 7) == 0)
        {
          server = (struct gp_server*) gp_file_open (&p[7]);
        }
      else if (strncmp (p, "tcp://", 6) == 0)
        {
          server = (struct gp_server*) gp_socket_open (&p[6]);
        }
    }
  return 0;
  
}

