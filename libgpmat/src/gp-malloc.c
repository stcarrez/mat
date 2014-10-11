/*  gp-malloc.c -- Malloc operations
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
#include "gp-probe.h"
#include "gp-events.h"
#include <stdlib.h>
#include <dlfcn.h>

#ifndef RTLD_NEXT
# define RTLD_NEXT      ((void *) -1l)
#endif

typedef void* (*gp_malloc_t) (size_t);
typedef void* (*gp_realloc_t) (void*, size_t);
typedef void (*gp_free_t) (void*);

static gp_malloc_t _malloc;
static gp_realloc_t _realloc;
static gp_free_t _free;

static void __attribute__ ((constructor)) init (void)
{
  _malloc = (gp_malloc_t) dlsym(RTLD_NEXT, "__libc_malloc");
  _realloc = (gp_realloc_t) dlsym(RTLD_NEXT, "__libc_realloc");
  _free = (gp_free_t) dlsym(RTLD_NEXT, "__libc_free");
}


/**
 * @brief Overrides the libc malloc.
 *
 * @param size size of memory block to allocate
 * @return the allocated block
 */
void*
__libc_malloc (size_t size)
{
  struct gp_probe probe;
  int has_probe;
  void *p;

  /* Get the probe.  */
  has_probe = gp_get_probe (&probe);

  if (_malloc == 0)
    {
      _malloc = (gp_malloc_t) dlsym (RTLD_NEXT, "__libc_malloc");
    }
  
  /* Call the real memory allocator.  */
  p = _malloc (size);

  /* Send the information only when this is possible (can have
     recursive calls to malloc from gp_get_probe, and the communication
     could also failed to be open).  */
  if (has_probe) 
    {
      gp_frame_add_skip (&probe, 2);
      gp_event_malloc (&probe, p, size);
      gp_free_probe (&probe);
    }

  return p;
}

void*
malloc (size_t size)
{
  return __libc_malloc (size);
}

void*
__libc_realloc (void *ptr, size_t size)
{
  struct gp_probe probe;
  int has_probe;
  void *p;

  /* Get the probe information.  */
  has_probe = gp_get_probe (&probe);

  p = _realloc (ptr, size);

  if (has_probe)
    {
      gp_frame_add_skip (&probe, 2);
      gp_event_realloc (&probe, p, ptr, size);
      gp_free_probe (&probe);
    }

  return p;
}

void*
realloc (void* ptr, size_t size)
{
  return __libc_realloc (ptr, size);
}

void
__libc_free (void* ptr)
{
  struct gp_probe probe;
  int has_probe;

  if (ptr == NULL)
    {
      return;
    }

  /* Get the probe information.  */
  has_probe = gp_get_probe (&probe);

  if (has_probe) 
    {
      gp_frame_add_skip (&probe, 2);
      gp_event_free (&probe, ptr);
      gp_free_probe (&probe);
    }

  if (_free != NULL)
    {
      _free (ptr);
    }
}

void
free (void* ptr)
{
  __libc_free (ptr);
}

