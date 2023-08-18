/* mat-events.c -- Event operations
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2021, 2023 Stephane Carrez
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
#define _GNU_SOURCE
#include <link.h>
#include "mat-config.h"
#include <stdarg.h>
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
#include "mat-remote.h"
#include "mat-probe.h"
#include "mat-events.h"
#include "mat-proc.h"
#define _GNU_SOURCE
#include <dlfcn.h>

#ifndef RTLD_NEXT
# define RTLD_NEXT      ((void *) -1l)
#endif

extern char etext, edata, end, _start;


static void
mat_send_attributes (const struct mat_event_def *type)
{
  mat_uint16 len = strlen (type->name);
  mat_uint16 val = type->type;
  const struct mat_attr_def *attr;
  int i;
  
  mat_write ("E-LEN", 2, &len, sizeof (len));
  mat_write ("EVENT", 2, type->name, len);
  mat_write ("ID", 2, &val, sizeof (val));

  len = type->nr_attrs;
  mat_write ("COUNT", 2, &len, sizeof (len));
  mat_debug_msg ("\n");

  attr = type->attributes;
  for (i = type->nr_attrs; --i >= 0; attr++)
    {
      len = strlen (attr->name);
      val = attr->size;
      mat_write ("A-LEN", 4, &len, sizeof (len));
      mat_write ("A-NAME", 4, attr->name, len);
      mat_write ("A-SIZE", 4, &val, sizeof (val));
      mat_debug_msg ("\n");
    }
}

static int
mat_get_attribute_size (const struct mat_event_def *type)
{
  int result = sizeof (mat_uint16);
  const struct mat_attr_def *attr;
  int i;

  result += strlen (type->name);
  result += sizeof (mat_uint16);
  result += sizeof (mat_uint16);

  attr = type->attributes;
  for (i = type->nr_attrs; --i >= 0; attr++)
    {
      result += sizeof (mat_uint16) + sizeof (mat_uint16);
      result += strlen (attr->name);
    }
  
  return result;
}

static void
mat_event_send_attr (const struct mat_attr_def *attr, va_list argp)
{
  union u
  {
    mat_uint8 u8;
    mat_uint16 u16;
    mat_uint32 u32;
    mat_uint64 u64;
  } u;
  const char* data;

  switch (attr->type)
    {
    case GP_TYPE_UINT8:
      u.u8 = va_arg (argp, mat_uint8_varg);
      mat_write (attr->name, 4, &u.u8, sizeof (mat_uint8));
      break;

    case GP_TYPE_UINT16:
      u.u16 = va_arg (argp, mat_uint16_varg);
      mat_write (attr->name, 4, &u.u16, sizeof (mat_uint16));
      break;

    case GP_TYPE_UINT32:
      u.u32 = va_arg (argp, mat_uint32);
      mat_write (attr->name, 4, &u.u32, sizeof (mat_uint32));
      break;

    case GP_TYPE_UINT64:
      u.u64 = va_arg (argp, mat_uint64);
      mat_write (attr->name, 4, &u.u64, sizeof (mat_uint64));
      break;

    case GP_TYPE_STRING:
      u.u16 = va_arg (argp, mat_uint16_varg);
      mat_write (attr->name, 4, &u.u16, sizeof (mat_uint16));
      data = va_arg (argp, const char*);
      mat_write (attr->name, 4, data, (size_t) u.u16);
      break;

    default:
      break;
    }
}

static void
mat_event_send_vattr (const struct mat_attr_def *attr, ...)
{
  va_list argp;

  va_start (argp, attr);
  mat_event_send_attr (attr, argp);
  va_end (argp);
}

void
mat_event_send (struct mat_probe *gp, int size,
               const struct mat_event_def *type, ...)
{
  mat_uint16 len;
  mat_uint16 val;
  const struct mat_attr_def *attr;
  int i;
  va_list argp;

  len = sizeof (mat_uint16)
    + size
    + type->size
    + mat_remote_sizeof_probe (gp);

  val = type->type;
  mat_write ("PROBE", 0, &len, sizeof (len));
  mat_write (type->name, 2, &val, sizeof (val));
  mat_remote_send_probe (gp);
  
  va_start (argp, type);
  attr = type->attributes;
  for (i = type->nr_attrs; --i >= 0; attr++)
    {
      union u
        {
          mat_uint8 u8;
          mat_uint16 u16;
          mat_uint32 u32;
          mat_uint64 u64;
      } u;
      const char* data;
      
      switch (attr->type)
        {
        case GP_TYPE_UINT8:
          u.u8 = va_arg (argp, mat_uint8_varg);
          mat_write (attr->name, 4, &u.u8, sizeof (mat_uint8));
          break;

        case GP_TYPE_UINT16:
          u.u16 = va_arg (argp, mat_uint16_varg);
          mat_write (attr->name, 4, &u.u16, sizeof (mat_uint16));
          break;
          
        case GP_TYPE_UINT32:
          u.u32 = va_arg (argp, mat_uint32);
          mat_write (attr->name, 4, &u.u32, sizeof (mat_uint32));
          break;

        case GP_TYPE_UINT64:
          u.u64 = va_arg (argp, mat_uint64);
          mat_write (attr->name, 4, &u.u64, sizeof (mat_uint64));
          break;

        case GP_TYPE_STRING:
          u.u16 = va_arg (argp, mat_uint16_varg);
          mat_write (attr->name, 4, &u.u16, sizeof (mat_uint16));
          data = va_arg (argp, const char*);
          mat_write (attr->name, 4, data, (size_t) u.u16);
          break;
          
        default:
          break;
        }
    }
  va_end (argp);
  mat_debug_msg ("\n");
}

static const struct mat_attr_def mat_malloc_attrs[] = {
  { "pointer", GP_TYPE_POINTER, sizeof (void*) },
  { "size",    GP_TYPE_SIZE_T,  sizeof (size_t) },
  { "curbrk",  GP_TYPE_POINTER, sizeof (void*) }
};

static const struct mat_event_def mat_event_malloc_def = {
  "malloc",
  GP_EVENT_MALLOC,
  sizeof (void*) + sizeof (size_t) + sizeof (void*),
  GP_TABLE_SIZE (mat_malloc_attrs),
  mat_malloc_attrs
};

void
mat_event_malloc (struct mat_probe *gp, void *p, size_t size, void* cbrk)
{
  mat_event_send (gp, 0, &mat_event_malloc_def, p, size, cbrk);
}

static const struct mat_attr_def mat_free_attrs[] = {
  { "pointer", GP_TYPE_POINTER, sizeof (void*) }
};
  
static const struct mat_event_def mat_event_free_def = {
  "free",
  GP_EVENT_FREE,
  sizeof (void*),
  GP_TABLE_SIZE (mat_free_attrs),
  mat_free_attrs
};

void
mat_event_free (struct mat_probe *gp, void *p)
{
  mat_event_send (gp, 0, &mat_event_free_def, p);
}

static const struct mat_attr_def mat_realloc_attrs[] = {
  { "pointer", GP_TYPE_POINTER, sizeof (void*) },
  { "old-pointer", GP_TYPE_POINTER, sizeof (void*) },
  { "size",    GP_TYPE_SIZE_T, sizeof (size_t) },
  { "curbrk",  GP_TYPE_POINTER, sizeof (void*) }
};

static const struct mat_event_def mat_event_realloc_def = {
  "realloc",
  GP_EVENT_REALLOC,
  sizeof (void*) + sizeof (void*) + sizeof (size_t) + sizeof (void*),
  GP_TABLE_SIZE (mat_realloc_attrs),
  mat_realloc_attrs
};

void
mat_event_realloc (struct mat_probe *gp, void *p, void *old, size_t size, void* cbrk)
{
  mat_event_send (gp, 0, &mat_event_realloc_def, p, old, size, cbrk);
}

static const struct mat_attr_def mat_mutex_attrs[] = {
  { "pointer", GP_TYPE_POINTER, sizeof (void*) }
};
  
static const struct mat_event_def mat_event_mutex_lock_def = {
  "pthread_mutex_lock",
  GP_EVENT_MUTEX_LOCK,
  sizeof (void*),
  GP_TABLE_SIZE (mat_mutex_attrs),
  mat_mutex_attrs
};

void
mat_event_mutex_lock (struct mat_probe *gp, void *p)
{
  mat_event_send (gp, 0, &mat_event_mutex_lock_def, p);
}

static const struct mat_event_def mat_event_mutex_unlock_def = {
  "pthread_mutex_unlock",
  GP_EVENT_MUTEX_UNLOCK,
  sizeof (void*),
  GP_TABLE_SIZE (mat_mutex_attrs),
  mat_mutex_attrs
};

void
mat_event_mutex_unlock (struct mat_probe *gp, void *p)
{
  mat_event_send (gp, 0, &mat_event_mutex_unlock_def, p);
}

static const struct mat_event_def mat_event_mutex_trylock_def = {
  "pthread_mutex_trylock",
  GP_EVENT_MUTEX_TRYLOCK,
  sizeof (void*),
  GP_TABLE_SIZE (mat_mutex_attrs),
  mat_mutex_attrs
};

void
mat_event_mutex_trylock (struct mat_probe *gp, void *p)
{
  mat_event_send (gp, 0, &mat_event_mutex_trylock_def, p);
}

static const struct mat_attr_def mat_secondary_stack_mark_attrs[] = {
  { "size", GP_TYPE_SIZE_T, sizeof (size_t) },
  { "pointer", GP_TYPE_POINTER, sizeof (void*) }
};
  
static const struct mat_event_def mat_event_secondary_stack_mark_def = {
  "secondary_stack_mark",
  GP_EVENT_SECONDARY_STACK_MARK,
  sizeof (size_t) + sizeof (void*),
  GP_TABLE_SIZE (mat_secondary_stack_mark_attrs),
  mat_secondary_stack_mark_attrs
};

void
mat_event_secondary_stack_mark (struct mat_probe *gp, void *mark, size_t size)
{
  mat_event_send (gp, 0, &mat_event_secondary_stack_mark_def, size, mark);
}

static const struct mat_attr_def mat_secondary_stack_allocate_attrs[] = {
  { "pointer", GP_TYPE_POINTER, sizeof (void*) },
  { "size", GP_TYPE_SIZE_T, sizeof (size_t) }
};
  
static const struct mat_event_def mat_event_secondary_stack_allocate_def = {
  "secondary_stack_allocate",
  GP_EVENT_SECONDARY_STACK_ALLOCATE,
  sizeof (size_t) + sizeof (void *),
  GP_TABLE_SIZE (mat_secondary_stack_allocate_attrs),
  mat_secondary_stack_allocate_attrs
};

void
mat_event_secondary_stack_allocate (struct mat_probe *gp, void *p, size_t size)
{
  mat_event_send (gp, 0, &mat_event_secondary_stack_allocate_def, p, size);
}

static const struct mat_attr_def mat_secondary_stack_release_attrs[] = {
  { "size", GP_TYPE_SIZE_T, sizeof (size_t) },
  { "pointer", GP_TYPE_POINTER, sizeof (void*) }
};
  
static const struct mat_event_def mat_event_secondary_stack_release_def = {
  "secondary_stack_release",
  GP_EVENT_SECONDARY_STACK_RELEASE,
  sizeof (size_t) + sizeof (void*),
  GP_TABLE_SIZE (mat_secondary_stack_release_attrs),
  mat_secondary_stack_release_attrs
};

void
mat_event_secondary_stack_release (struct mat_probe *gp, void *mark, size_t size)
{
  mat_event_send (gp, 0, &mat_event_secondary_stack_release_def, size, mark);
}

static const struct mat_attr_def mat_shlib_attrs[] = {
  { "libname", GP_TYPE_STRING, sizeof (mat_uint16) },
  { "laddr",   GP_TYPE_POINTER, sizeof (char*) },
  { "count",   GP_TYPE_UINT16, sizeof (mat_uint16) },
  { "type",    GP_TYPE_UINT32, sizeof (mat_uint32) },
  { "flags",   GP_TYPE_UINT32, sizeof (mat_uint32) },
  { "vaddr",   GP_TYPE_POINTER, sizeof (char*) },
  { "size",    GP_TYPE_SIZE_T, sizeof (size_t) },
};

static const struct mat_event_def mat_event_shlib_def = {
  "shlib",
  GP_EVENT_SHLIB,
  sizeof (mat_uint32) + sizeof (mat_uint32) + sizeof (void*) + sizeof (size_t),
  GP_TABLE_SIZE (mat_shlib_attrs),
  mat_shlib_attrs
};

static const struct mat_attr_def mat_frame_attrs[] = {
  { "time-sec",  GP_TYPE_UINT32, sizeof (mat_uint32) },
  { "time-usec", GP_TYPE_UINT32, sizeof (mat_uint32) },
  { "thread-id", GP_TYPE_UINT32, sizeof (mat_uint32) },
  { "thread-sp", GP_TYPE_UINT32, sizeof (mat_uint32) },
#ifdef HAVE_RUSAGE
  { "ru-minflt", GP_TYPE_UINT32, sizeof (mat_uint32) },  
  { "ru-majflt", GP_TYPE_UINT32, sizeof (mat_uint32) },  
  { "ru-nvcsw",  GP_TYPE_UINT32, sizeof (mat_uint32) },  
  { "ru-nivcsw", GP_TYPE_UINT32, sizeof (mat_uint32) },  
#endif
  { "frame",     GP_TYPE_UINT16, sizeof (mat_uint16) },
  { "frame-pc",  GP_TYPE_UINT32, sizeof (mat_uint32) },
};

static const struct mat_event_def mat_event_begin_frame_def = {
  "start",
  GP_EVENT_BEGIN,
  0,
  GP_TABLE_SIZE (mat_frame_attrs),
  mat_frame_attrs
};

static const struct mat_attr_def mat_begin_attrs[] = {
  { "pid",      GP_TYPE_UINT32,  sizeof (mat_uint32) },
  { "exe",      GP_TYPE_STRING,  sizeof (mat_uint16) },
  { "hp_start", GP_TYPE_POINTER, sizeof (char*) },
  { "hp_end",   GP_TYPE_POINTER, sizeof (char*) },
  { "edata",    GP_TYPE_POINTER, sizeof (char*) },
  { "end",      GP_TYPE_POINTER, sizeof (char*) }
};

static const struct mat_event_def mat_event_begin_def = {
  "begin",
  GP_EVENT_BEGIN,
  sizeof (mat_uint32) + sizeof (mat_uint16) + sizeof (char*) * 4,
  GP_TABLE_SIZE (mat_begin_attrs),
  mat_begin_attrs
};

const struct mat_event_def mat_event_end_def = {
  "end",
  GP_EVENT_END,
  0,
  0,
  0
};

static const struct mat_event_def* start_events[] = {
  &mat_event_begin_frame_def,
  &mat_event_begin_def
};

static const struct mat_event_def* events[] = {
  &mat_event_malloc_def,
  &mat_event_free_def,
  &mat_event_realloc_def,
  &mat_event_mutex_lock_def,
  &mat_event_mutex_unlock_def,
  &mat_event_mutex_trylock_def,
  &mat_event_end_def,
  &mat_event_shlib_def,
  &mat_event_secondary_stack_mark_def,
  &mat_event_secondary_stack_allocate_def,
  &mat_event_secondary_stack_release_def
};

/**
 * @brief Send the event descriptions.
 *
 * @param events the event description.
 * @param count the number of event descriptions.
 */
static void
mat_send_attribute_list (const struct mat_event_def** events, size_t count)
{
  mat_uint16 version = GP_VERSION;
  mat_uint16 len;
  int i;

  len = sizeof (version) + sizeof (len);
  for (i = 0; i < count; i++)
    {
      len += mat_get_attribute_size (events[i]);
    }
  mat_write ("DEF", 0, &len, sizeof (len));
  mat_write ("VER", 0, &version, sizeof (version));

  len = count;
  mat_write ("E-CNT", 0, &len, sizeof (len));
  mat_debug_msg ("\n");
  
  for (i = 0; i < count; i++)
    {
      mat_send_attributes (events[i]);
    }
  mat_debug_msg ("\n");
}

static int dl_callback (struct dl_phdr_info* info, size_t size, void* data)
{
  mat_uint16 len;
  size_t slen;
  int count;
  struct mat_probe* gp;
  mat_uint16 val;
  int i;

  gp = (struct mat_probe*) data;
  slen = strlen (info->dlpi_name);
  count = info->dlpi_phnum;
  len = sizeof (mat_uint16)
    + sizeof (mat_pointer)
    + sizeof (mat_uint16)
    + sizeof (mat_uint16)
    + slen
    + mat_remote_sizeof_probe (gp)
    + count * mat_event_shlib_def.size;

  val = mat_event_shlib_def.type;
  mat_write ("PROBE", 0, &len, sizeof (len));
  mat_write (mat_shlib_attrs[0].name, 2, &val, sizeof (val));
  mat_remote_send_probe (gp);

  mat_event_send_vattr (&mat_shlib_attrs[0], (mat_uint16) slen, info->dlpi_name);
  mat_event_send_vattr (&mat_shlib_attrs[1], (mat_pointer) info->dlpi_addr);
  mat_event_send_vattr (&mat_shlib_attrs[2], (mat_uint16) count);
  for (i = 0; i < count; i++)
    {
      mat_event_send_vattr (&mat_shlib_attrs[3], (mat_uint32) info->dlpi_phdr[i].p_type);
      mat_event_send_vattr (&mat_shlib_attrs[3], (mat_uint32) info->dlpi_phdr[i].p_flags);
      mat_event_send_vattr (&mat_shlib_attrs[4], (mat_pointer) info->dlpi_phdr[i].p_vaddr);
      mat_event_send_vattr (&mat_shlib_attrs[5], (mat_pointer) info->dlpi_phdr[i].p_memsz);
    }
  return 0;  
}

static void
mat_identify_heap (map_info_t* map, void* data)
{
  map_info_t* result;
  
  if (strcmp (map->name, "[heap]") == 0)
    {
      result = (map_info_t*) data;

      result->start = map->start;
      result->end   = map->end;
      result->flags = map->flags;
    }
}

void
mat_event_begin (struct mat_probe *gp)
{
  int i;
  mat_uint8  mode;
  char cur_proc[PATH_MAX];
  char path[PATH_MAX];
  mat_uint32 pid;
  ssize_t size;
  map_info_t heap;

  i = 1;
  if (((unsigned char*) &i)[0] == 1)
    {
      mode = GP_LITTLE_ENDIAN;
    }
  else
    {
      mode = GP_BIG_ENDIAN;
    }
  mat_remote_send (&mode, sizeof (mode));
  mat_send_attribute_list (start_events, GP_TABLE_SIZE (start_events));

  pid = getpid ();
  mat_read_proc_maps ((int) pid, mat_identify_heap, &heap);
  snprintf (cur_proc, sizeof (cur_proc), "/proc/%d/exe", (int) pid);

  size = readlink (cur_proc, path, sizeof (path));
  if (size < 0)
    {
      path[0] = 0;
      size = 0;
    }
  else
    {
      path[size] = 0;
    }

  mat_event_send (gp, size, &mat_event_begin_def, pid, size, path, heap.start, heap.end, &edata, &end);
  mat_send_attribute_list (events, GP_TABLE_SIZE (events));
  dl_iterate_phdr (dl_callback, gp);
}

void
mat_event_end (struct mat_probe *gp)
{
  mat_event_send (gp, 0, &mat_event_end_def);
}

