/*  gp-malloc.c -- Malloc operations
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
#define _GNU_SOURCE 1
#include <dlfcn.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#include "mat-remote.h"
#include "mat-probe.h"
#include "mat-events.h"

#ifndef RTLD_NEXT
# define RTLD_NEXT      ((void *) -1l)
#endif

#ifdef HAVE___CURBRK
extern void* __curbrk;
# define CURBRK __curbrk
#else
# define CURBRK (void*) 0
#endif

typedef void* (*mat_malloc_t) (size_t);
typedef void* (*mat_realloc_t) (void*, size_t);
typedef void (*mat_free_t) (void*);

static mat_malloc_t _malloc;
static mat_realloc_t _realloc;
static mat_free_t _free;

#ifdef HAVE___LIBC_MALLOC
# define LIBC_MALLOC_SYM   "__libc_malloc"
# define LIBC_REALLOC_SYM  "__libc_realloc"
# define LIBC_FREE_SYM     "__libc_free"
#else
# define LIBC_MALLOC_SYM   "malloc"
# define LIBC_REALLOC_SYM  "realloc"
# define LIBC_FREE_SYM     "free"
#endif

static void
load_symbols (void)
{
  _malloc = (mat_malloc_t) dlsym (RTLD_NEXT, LIBC_MALLOC_SYM);
  _realloc = (mat_realloc_t) dlsym (RTLD_NEXT, LIBC_REALLOC_SYM);
  _free = (mat_free_t) dlsym (RTLD_NEXT, LIBC_FREE_SYM);
}

#ifndef HAVE___LIBC_MALLOC
/**
 * @brief Override the calloc for uClibc.
 *
 * The uClibc calloc is taking the uClibc malloc lock and then
 * calls malloc.  This creates deadlock.
 * The glibc does not have this issue.
 */
void*
calloc (size_t n_elements, size_t elem_size)
{
  size_t len = n_elements * elem_size;
  void* p;

  p = malloc (len);
  if (p != NULL)
    memset (p, 0, len);

  return p;
}
#endif

/**
 * @brief Overrides the libc malloc.
 *
 * @param size size of memory block to allocate
 * @return the allocated block
 */
void*
__libc_malloc (size_t size)
{
  struct mat_probe probe;
  int has_probe;
  void *p;

  /* Get the probe.  */
  has_probe = mat_get_probe (&probe);

  if (_malloc == 0)
    {
      load_symbols ();
    }
  
  /* Call the real memory allocator.  */
  p = _malloc (size);

  /* Send the information only when this is possible (can have
     recursive calls to malloc from mat_get_probe, and the communication
     could also failed to be open).  */
  if (has_probe) 
    {
      mat_frame_add_skip (&probe, 1);
      mat_event_malloc (&probe, p, size, CURBRK);
      mat_free_probe (&probe);
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
  struct mat_probe probe;
  int has_probe;
  void *p;

  /* Get the probe information.  */
  has_probe = mat_get_probe (&probe);

  if (_realloc == 0)
    {
      load_symbols ();
    }
  p = _realloc (ptr, size);

  if (has_probe)
    {
      mat_frame_add_skip (&probe, 1);
      mat_event_realloc (&probe, p, ptr, size, CURBRK);
      mat_free_probe (&probe);
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
  struct mat_probe probe;
  int has_probe;

  if (ptr == NULL)
    {
      return;
    }

  /* Get the probe information.  */
  has_probe = mat_get_probe (&probe);

  if (has_probe) 
    {
      mat_frame_add_skip (&probe, 1);
      mat_event_free (&probe, ptr);
      mat_free_probe (&probe);
    }

  if (_free == 0)
    {
      load_symbols ();
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

