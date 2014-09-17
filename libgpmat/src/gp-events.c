/* gp-events.c -- Event operations
   Copyright 2001 Free Software Foundation, Inc.
   Written by Stephane Carrez (stcarrez@worldnet.fr)

This file is part of gprofiler.

gprofiler is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

gprofiler is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with EBCS; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <stdarg.h>
// #include "gprofiler.h"
#include "gp-probe.h"
#include "gp-events.h"

void
gp_event_send (struct gp_probe *gp, int size,
               const struct gp_event_def *type, ...)
{
  size_t len;
  const struct gp_attr_def *attr;
  int i;
  va_list argp;

  len = sizeof (gp_event_type)
    + size
    + gp_remote_sizeof_probe (gp);

  gp_remote_send (&len, sizeof (len));
  gp_remote_send (&type->type, sizeof (type->type));
  gp_remote_send_probe (gp);
  
  va_start (argp, type);
  attr = type->attributes;
  for (i = type->nr_attrs; --i >= 0; attr++)
    {
      union u
        {
          gp_uint8 u8;
          gp_uint16 u16;
          gp_uint32 u32;
          gp_uint64 u64;
      } u;
      
      switch (attr->size)
        {
        case 1:
          u.u8 = va_arg (argp, gp_uint8_varg);
          gp_remote_send (&u.u8, sizeof (gp_uint8));
          break;

        case 2:
          u.u16 = va_arg (argp, gp_uint16_varg);
          gp_remote_send (&u.u16, sizeof (gp_uint16));
          break;
          
        case 4:
          u.u32 = va_arg (argp, gp_uint32);
          gp_remote_send (&u.u32, sizeof (gp_uint32));
          break;

        case 8:
          u.u64 = va_arg (argp, gp_uint64);
          gp_remote_send (&u.u64, sizeof (gp_uint64));
          break;

        default:
          break;
        }
    }
  va_end (argp);
}

const struct gp_attr_def gp_malloc_attrs[] = {
  { "pointer", GP_TYPE_POINTER, sizeof (void*) },
  { "size",    GP_TYPE_SIZE_T,  sizeof (size_t) }
};

const struct gp_event_def gp_event_malloc_def = {
  "malloc",
  GP_EVENT_MALLOC,
  2,
  gp_malloc_attrs
};

void
gp_event_malloc (struct gp_probe *gp, void *p, size_t size)
{
   gp_event_send (gp, 8, &gp_event_malloc_def, p, size);
   // gp_remote_send_vmaddr (p);
   // gp_remote_send_size (size);
}

const struct gp_attr_def gp_free_attrs[] = {
  { "pointer", GP_TYPE_POINTER, sizeof (void*) }
};
  
const struct gp_event_def gp_event_free_def = {
  "free",
  GP_EVENT_FREE,
  1,
  gp_free_attrs
};

void
gp_event_free (struct gp_probe *gp, void *p)
{
  gp_event_send (gp, 4, &gp_event_free_def, p);
}

const struct gp_attr_def gp_realloc_attrs[] = {
  { "pointer", GP_TYPE_POINTER, sizeof (void*) },
  { "old-pointer", GP_TYPE_POINTER, sizeof (void*) },
  { "size",    GP_TYPE_SIZE_T, sizeof (size_t) }
};

const struct gp_event_def gp_event_realloc_def = {
  "realloc",
  GP_EVENT_REALLOC,
  3,
  gp_realloc_attrs
};

void
gp_event_realloc (struct gp_probe *gp, void *p, void *old, size_t size)
{
  gp_event_send (gp, 12, &gp_event_free_def, p, old, size);
}

const struct gp_event_def gp_event_begin_def = {
  "begin",
  GP_EVENT_BEGIN,
  0,
  0
};

void
gp_event_begin (struct gp_probe *gp)
{
  gp_event_send (gp, 0, &gp_event_begin_def);
}

const struct gp_event_def gp_event_end_def = {
  "end",
  GP_EVENT_END,
  0,
  0
};

void
gp_event_end (struct gp_probe *gp)
{
  gp_event_send (gp, 0, &gp_event_end_def);
}

