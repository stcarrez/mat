/* Remote access
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
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "gp-remote.h"
#include "gp-file.h"

#ifndef O_CLOEXEC
# define O_CLOEXEC 0
#endif

struct gp_file_server 
{
  struct gp_server root;
  int fd;
  unsigned char* write_ptr;
  unsigned char* last_ptr;
  unsigned char buffer[4096];
};

static struct gp_file_server server;

static int gp_file_flush (struct gp_file_server* file)
{
  size_t sz = file->write_ptr - file->buffer;
  ssize_t res = write (file->fd, file->buffer, sz);
  if (res != sz)
    {
      close (file->fd);
      file->fd = -1;
      return -1;
    }
  file->write_ptr = file->buffer;
  return 0;
}

void gp_file_send (struct gp_server* server, const void* ptr, size_t len)
{
   struct gp_file_server* file = (struct gp_file_server*) server;

   if (file->fd < 0)
     return;

   while (len > 0)
     {
       size_t avail = file->last_ptr - file->write_ptr;
       if (avail > len)
         avail = len;

       if (avail > 0)
         {
           memcpy (file->write_ptr, ptr, avail);
           file->write_ptr += avail;
           ptr = ((unsigned char*) ptr) + avail;
           len -= avail;
         }
       else
         {
           if (gp_file_flush (file) != 0)
             break;
         }
     }
}

int gp_file_synchronize (struct gp_server* server)
{
    return 0;
}

void gp_file_close (struct gp_server* server)
{
   struct gp_file_server* file = (struct gp_file_server*) server;

   if (file->fd < 0)
     return;

   if (gp_file_flush (file) == 0)
     close (file->fd);
   file->fd = -1;
}

struct gp_file_server* gp_file_open (const char* param)
{
  char path[PATH_MAX];
  char* s;
  int pid = getpid ();
  int i;

  s = path;
  while ((s < &path[PATH_MAX - 10]) && (*param != 0))
    {
      *s++ = *param++;
    }
  *s++ = '-';
  for (i = 1000; i > 0; i = i / 10)
    {
      *s++ = "0123456789"[(pid / i) % 10];
    }
  strcpy (s, ".mat");

  server.fd = open (path, O_CREAT | O_WRONLY | O_TRUNC | O_CLOEXEC, 0644);
  if (server.fd <= 0) 
    {
      return NULL;
    }
  server.write_ptr = server.buffer;
  server.last_ptr  = &server.buffer[sizeof (server.buffer)];
  server.root.to_send        = gp_file_send;
  server.root.to_synchronize = gp_file_synchronize;
  server.root.to_close       = gp_file_close;
   
  return &server;
}
