/*  gp-remote.c -- Remote access
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
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
#include "shm-channel.h"

static struct gp_server* server;

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
  if (server)
    {
      server->to_synchronize (server);
    }
}

int
gp_remote_initialize (void)
{
  char* p = getenv("GP_SERVER");
  if (p != NULL)
    {
      if (strncmp (p, "file://", 7) == 0)
        {
            server = (struct gp_server*) gp_file_open (&p[7]);
        }
    }
  // return gp_shm_channel_create (&gp_shm, SHARED_MEMORY_CLIENT_KEY, 8192);
  return 0;
  
}

