/*  mat-file.c -- file storage
--  Copyright (C) 2011, 2012, 2013, 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
static int mat_file_flush (struct mat_buffered_server* server)
{
  struct mat_file_server* file = (struct mat_file_server*) server;
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
void mat_file_close (struct mat_server* server)
{
  struct mat_file_server* file = (struct mat_file_server*) server;

  if (file->fd < 0)
    return;

  if (mat_file_flush ((struct mat_buffered_server*) server) == 0)
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
struct mat_file_server* mat_file_open (struct mat_file_server* server, const char* param)
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

  mat_buffered_server_initialize (&server->root, param);
  server->root.root.to_close       = mat_file_close;
  server->root.to_flush            = mat_file_flush;
   
  return server;
}
