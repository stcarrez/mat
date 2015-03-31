/*  gp-remote.c -- Remote access
--  Copyright (C) 2011, 2012, 2013, 2014, 2015 Stephane Carrez
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
#include <stdarg.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#include <string.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include "mat-remote.h"
#include "mat-probe.h"
#include "mat-events.h"
#include "mat-file.h"
#include "mat-socket.h"
#include "shm-channel.h"

static union 
{
  struct mat_file_server   file;
  struct mat_socket_server socket;
} server_data;

static struct mat_server* server;

#ifdef DEBUG
static int mat_debug = 0;

static void
to_hex (char* buf, unsigned byte)
{
  buf[0] = "0123456789ABCDEF"[(byte >> 4) & 0x0F];
  buf[1] = "0123456789ABCDEF"[byte & 0x0F];
}

void
mat_dump (const char* title, int indent, const void* addr, size_t len)
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
mat_write (const char* title, int indent, const void* addr, size_t len)
{
  if (mat_debug)
    mat_dump (title, indent, addr, len);
  mat_remote_send (addr, len);
}

void
mat_debug_msg (const char*  msg)
{
  if (mat_debug)
    write (STDERR_FILENO, msg, strlen (msg));
}

#endif

/**
 * @brief Send the content through an internal buffer.
 *
 * @param server the GP server instance.
 * @param ptr the data to send.
 * @param len the number of bytes to send.
 */
void mat_buffered_send (struct mat_server* server, const void* ptr, size_t len)
{
  struct mat_buffered_server* buffer = (struct mat_buffered_server*) server;

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
int mat_buffered_synchronize (struct mat_server* server)
{
  struct mat_buffered_server* buffer = (struct mat_buffered_server*) server;

  return buffer->to_flush (buffer);
}

/**
 * @brief Send the raw data to the file or socket.
 *
 * @param addr the data to send.
 * @param len the number of bytes to send.
 */
void
mat_remote_send (const void *addr, size_t len)
{
  if (server)
    {
      server->to_send (server, addr, len);
    }
}

/**
 * @brief Close the event probe stream.
 */
void
mat_remote_close (void)
{
  if (server)
    {
      server->to_close (server);
    }
}

/**
 * @brief Synchronize the event probe stream (if enabled).
 */
void
mat_remote_sync (void)
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
 * @param param the configuration parameter.
 */
void
mat_buffered_server_initialize (struct mat_buffered_server* server, const char* param)
{
  server->write_ptr = server->buffer;
  server->last_ptr  = &server->buffer[sizeof (server->buffer)];
  server->root.to_send        = mat_buffered_send;
  server->root.to_synchronize = NULL;
  if (param != NULL && strcmp (param, "sync") == 0)
    server->root.to_synchronize = mat_buffered_synchronize;
}

/**
 * @brief Initialize the connection to the server.
 *
 * file://<pattern>[?sync]     Write the probe stream in a file.
 * tcp://host:port[?sync]      Send the probe stream to the TCP/IP server.
 *
 * The optional <tt>sync</tt> flag enables the synchronous mode which flushes
 * the event probe stream after each operation.
 *
 * @return 0
 */
int
mat_remote_initialize (const char* p)
{
  if (p != NULL)
    {
      if (strncmp (p, "file://", 7) == 0)
        {
          server = (struct mat_server*) mat_file_open (&server_data.file, &p[7]);
        }
      else if (strncmp (p, "tcp://", 6) == 0)
        {
          server = (struct mat_server*) mat_socket_open (&server_data.socket, &p[6]);
        }
#ifdef DEBUG
      if ((server != NULL) && (getenv("MAT_DEBUG") != NULL))
        {
          mat_debug = 1;
        }
#endif
    }
  return 0;
  
}

