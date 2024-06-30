/* mat-proc.c -- Information extracted from /proc
--  Copyright (C) 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
*/
#include "mat-config.h"
#include <string.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include "mat-proc.h"

static void
mat_extract_map_line (char* data, map_info_t* info)
{
  int i;

  if (data[0] == '0' && data[1] == 'x')
    data += 2;
  
  info->start = (void*) strtoul (data, &data, 16);
  if (*data == '-' || *data == ' ')
    data++;
  
  if (data[0] == '0' && data[1] == 'x')
    data += 2;

  info->end = (void*) strtoul (data, &data, 16);
  while (*data && *data != 'r')
    data++;

  info->flags = 0;
  if (*data == 'r')
    {
      info->flags = MAP_R_OK;
      if (data[1] == 'w')
        info->flags |= MAP_W_OK;
      if (data[2] == 'x')
        info->flags |= MAP_X_OK;
      if (data[3] == 'p')
        info->flags |= MAP_PRIV;
    }
  for (i = 0; ; i++)
    {
      while (*data && *data != ' ')
        data++;

      while (*data && *data == ' ')
        data++;

      if (i == 3)
        break;

      if (*data)
        data++;
    }
  info->name = data;
}

/**
 * @brief Read the process /proc/<pid>/maps file.
 *
 * @param pid the process id.
 * @param callback the callback execute for each map entry.
 * @param data the callback data.
 */
void
mat_read_proc_maps (int pid, mat_extract_map_callback callback, void *data)
{
  int fd;
  char buf[512];
  char *p;
  char *src;
  char line[512];
  ssize_t size;
  map_info_t info;

  snprintf (buf, sizeof (buf), "/proc/%d/maps", (int) pid);
  fd = open (buf, O_RDONLY);
  if (fd < 0)
    {
      return;
    }
  p = line;
  src = buf;
  size = 0;
  while (1)
    {
      if (size == 0)
        {
          size = read (fd, buf, sizeof (buf));
          if (size <= 0)
            break;
          src = buf;
        }

      /* Fill the line until we reach the end of line or end of input buffer.  */
      while (size > 0 && *src != '\n')
        {
          if (p < &line[sizeof (line)])
            {
              *p = *src;
              p++;
            }
          src++;
          size--;
        }

      /* We have a line, extract the map information.  */
      if (*src == '\n')
        {
          src++;
          size--;
          *p = 0;
          mat_extract_map_line (line, &info);
          callback (&info, data);
          p = line;
        }
    }
  close (fd);
}

