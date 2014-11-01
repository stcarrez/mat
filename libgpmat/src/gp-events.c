/* gp-events.c -- Event operations
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
#include <string.h>
#include <unistd.h>
#include "gp-probe.h"
#include "gp-events.h"

void
gp_send_attributes (const struct gp_event_def *type)
{
  gp_uint8 len = strlen(type->name);
  gp_uint16 val = type->type;
  const struct gp_attr_def *attr;
  int i;
  
  gp_write ("BLOCK", 0, &len, sizeof (len));
  gp_write ("EVENT", 2, type->name, len);
  gp_write ("ID", 2, &val, sizeof (val));

  len = type->nr_attrs;
  gp_write ("COUNT", 2, &len, sizeof (len));

  attr = type->attributes;
  for (i = type->nr_attrs; --i >= 0; attr++)
    {
      len = strlen (attr->name);
      val = attr->size;
      gp_write ("A-LEN", 4, &len, sizeof (len));
      gp_write ("A-NAME", 4, attr->name, len);
      gp_write ("A-TYPE", 4, &val, sizeof (val));
      write (STDERR_FILENO, "\n", 1);
    }
}

static int
gp_get_attribute_size (const struct gp_event_def *type)
{
  int result = sizeof (gp_uint8);
  const struct gp_attr_def *attr;
  int i;

  result += strlen (type->name);
  result += sizeof (gp_uint16);
  result += sizeof (gp_uint8);

  attr = type->attributes;
  for (i = type->nr_attrs; --i >= 0; attr++)
    {
      result += sizeof (gp_uint8) + sizeof (gp_uint16);
      result += strlen (attr->name);
    }
  
  return result;
}

void
gp_event_send (struct gp_probe *gp, int size,
               const struct gp_event_def *type, ...)
{
  gp_uint16 len;
  gp_uint16 val;
  const struct gp_attr_def *attr;
  int i;
  va_list argp;

  len = sizeof (gp_uint16)
    + size
    + gp_remote_sizeof_probe (gp);

  val = type->type;
  gp_write ("PROBE", 0, &len, sizeof (len));
  gp_write (type->name, 2, &val, sizeof (val));
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
          gp_write (attr->name, 4, &u.u8, sizeof (gp_uint8));
          break;

        case 2:
          u.u16 = va_arg (argp, gp_uint16_varg);
          gp_write (attr->name, 4, &u.u16, sizeof (gp_uint16));
          break;
          
        case 4:
          u.u32 = va_arg (argp, gp_uint32);
          gp_write (attr->name, 4, &u.u32, sizeof (gp_uint32));
          break;

        case 8:
          u.u64 = va_arg (argp, gp_uint64);
          gp_write (attr->name, 4, &u.u64, sizeof (gp_uint64));
          break;

        default:
          break;
        }
    }
  va_end (argp);
  write (STDERR_FILENO, "\n", 1);
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
  gp_event_send (gp, 12, &gp_event_realloc_def, p, old, size);
}

const struct gp_attr_def gp_frame_attrs[] = {
  { "time-sec",  GP_TYPE_UINT32, sizeof (gp_uint32) },
  { "time-usec", GP_TYPE_UINT32, sizeof (gp_uint32) },
  { "thread-id", GP_TYPE_UINT32, sizeof (gp_uint32) },
  { "thread-sp", GP_TYPE_UINT32, sizeof (gp_uint32) },
#ifdef HAVE_RUSAGE
  { "ru-minflt", GP_TYPE_UINT32, sizeof (gp_uint32) },  
  { "ru-majflt", GP_TYPE_UINT32, sizeof (gp_uint32) },  
  { "ru-nvcsw",  GP_TYPE_UINT32, sizeof (gp_uint32) },  
  { "ru-nivcsw", GP_TYPE_UINT32, sizeof (gp_uint32) },  
#endif
  { "frame",     GP_TYPE_UINT16, sizeof (gp_uint16) },
  { "frame-pc",  GP_TYPE_UINT32, sizeof (gp_uint32) },
};

#define GP_TABLE_SIZE(T) ((sizeof(T)) / sizeof(T[0]))

const struct gp_event_def gp_event_begin_frame_def = {
  "begin",
  GP_EVENT_BEGIN,
  GP_TABLE_SIZE(gp_frame_attrs),
  gp_frame_attrs
};

const struct gp_attr_def gp_begin_attrs[] = {
  { "pid",   GP_TYPE_UINT16, sizeof (pid_t) }
};

const struct gp_event_def gp_event_begin_def = {
  "begin",
  GP_EVENT_BEGIN,
  GP_TABLE_SIZE(gp_begin_attrs),
  gp_begin_attrs
};

static const struct gp_event_def* events[] = {
  &gp_event_begin_frame_def,
  &gp_event_malloc_def,
  &gp_event_free_def,
  &gp_event_realloc_def,
  &gp_event_end_def
};

void
gp_event_begin (struct gp_probe *gp)
{
  int i;
  gp_uint8  mode;
  gp_uint16 version = GP_VERSION;
  gp_uint16 count   = sizeof (events) / sizeof (const struct gp_event_def*);
  gp_uint16 len;

  i = 1;
  if (((unsigned char*) &i)[0] == 1)
    {
      mode = GP_LITTLE_ENDIAN;
    }
  else
    {
      mode = GP_BIG_ENDIAN;
    }
  gp_remote_send (&mode, sizeof (mode));

  len = sizeof (version);
  len += sizeof (count);
  for (i = 0; i < count; i++)
    {
      len += gp_get_attribute_size (events[i]);
    }
  gp_remote_send (&len, sizeof (len));
  gp_remote_send (&version, sizeof (version));
  gp_remote_send (&count, sizeof (count));
  for (i = 0; i < count; i++)
    {
      gp_send_attributes (events[i]);
    }
  gp_event_send (gp, 2, &gp_event_begin_def, getpid());
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

