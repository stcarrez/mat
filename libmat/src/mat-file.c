/*  mat-file.c -- file storage
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
#include "mat-config.h"
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <fcntl.h>
#include <string.h>
#include <limits.h>
#include "mat-remote.h"
#include "mat-file.h"

/**
 * @brief Flush the data stored in the buffer.
 *
 * @param file the GP server instance.
 * @return 0 if the operation succeeded.
 */
static int gp_file_flush (struct gp_buffered_server* server)
{
  struct gp_file_server* file = (struct gp_file_server*) server;
  size_t sz = file->root.write_ptr - file->root.buffer;
  ssize_t res = write (file->fd, file->root.buffer, sz);
  if (res != sz)
    {
      close (file->fd);
      file->fd = -1;
      return -1;
    }
  file->root.write_ptr = file->root.buffer;
  return 0;
}

/**
 * @brief Close the GP server file.
 *
 * @param server the GP server instance.
 */
void gp_file_close (struct gp_server* server)
{
  struct gp_file_server* file = (struct gp_file_server*) server;

  if (file->fd < 0)
    return;

  if (gp_file_flush ((struct gp_buffered_server*) server) == 0)
    close (file->fd);
  file->fd = -1;
}

/**
 * @brief Open the file and prepare for probe monitoring on a file.
 *
 * @param server the file server instance to initialize.
 * @param param the file pattern to create.
 * @return the GP server instance.
 */
struct gp_file_server* gp_file_open (struct gp_file_server* server, const char* param)
{
  char path[PATH_MAX];
  char* s;
  int pid = getpid ();
  int i;

  s = path;
  while ((s < &path[PATH_MAX - 10]) && (*param != 0) && (*param != '?'))
    {
      *s++ = *param++;
    }
  *s++ = '-';
  for (i = 10000; i > 0; i = i / 10)
    {
      *s++ = "0123456789"[(pid / i) % 10];
    }
  strcpy (s, ".mat");

  server->fd = open (path, O_CREAT | O_WRONLY | O_TRUNC | O_CLOEXEC, 0644);
  if (server->fd <= 0) 
    {
      return NULL;
    }
  if (*param == '?')
    {
      param++;
    }

  gp_buffered_server_initialize (&server->root, param);
  server->root.root.to_close       = gp_file_close;
  server->root.to_flush            = gp_file_flush;
   
  return server;
}
