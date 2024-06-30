/*  mat-gnat.c -- GNAT secondary stack inspection
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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

/* See GNAT System.Secondary_Stack declarations in s-sectsta.ads (gcc 12.2)
   Note: gcc 11 uses another implementation, gcc 7 another one etc... */
typedef struct gnat_chunk {
  size_t size;
  struct gnat_chunk *next;
  size_t size_up_to_chunk;
} gnat_chunk_t;

typedef struct {
  size_t memory_index;
  gnat_chunk_t *chunk;
} gnat_stack_pointer_t;

typedef struct {
  size_t default_chunk_size;
  size_t freeable;
  size_t high_water_mark;
  gnat_stack_pointer_t top;
  gnat_chunk_t static_chunk;
} gnat_secondary_stack_t;

typedef struct {
  gnat_secondary_stack_t *stack_ptr;
  gnat_stack_pointer_t top;  
} gnat_secondary_mark_t;

typedef void* (*mat_secondary_allocate_t) (size_t);
typedef void (*mat_secondary_mark_t) (gnat_secondary_mark_t *);
typedef void (*mat_secondary_release_t) (gnat_secondary_mark_t *);

static mat_secondary_allocate_t _secondary_allocate;
static mat_secondary_mark_t _secondary_mark;
static mat_secondary_release_t _secondary_release;
static volatile int loading_symbols = 0;

#define GNAT_SECONDARY_ALLOCATE_SYM   "system__secondary_stack__ss_allocate"
#define GNAT_SECONDARY_MARK_SYM       "system__secondary_stack__ss_mark"
#define GNAT_SECONDARY_RELEASE_SYM    "system__secondary_stack__ss_release"

static void
load_symbols (void)
{
  loading_symbols = 1;
  _secondary_allocate = (mat_secondary_allocate_t) dlsym (RTLD_NEXT, GNAT_SECONDARY_ALLOCATE_SYM);
  _secondary_mark = (mat_secondary_mark_t) dlsym (RTLD_NEXT, GNAT_SECONDARY_MARK_SYM);
  _secondary_release = (mat_secondary_release_t) dlsym (RTLD_NEXT, GNAT_SECONDARY_RELEASE_SYM);
  loading_symbols = 0;
}

static size_t
compute_secondary_stack_size (gnat_secondary_mark_t *mark)
{
  if (mark == 0 || mark->stack_ptr == 0 || mark->top.chunk == 0)
    return 0;

  return mark->top.chunk->size_up_to_chunk + mark->stack_ptr->top.memory_index - 1;
}

/**
 * @brief Overrides the GNAT system__secondary_stack__ss_allocate.
 *
 * @param size size of memory block to allocate
 * @return the allocated block
 */
void*
system__secondary_stack__ss_allocate (size_t size)
{
  struct mat_probe probe;
  int has_probe;
  void *p;

  if (_secondary_allocate == 0)
    {
      load_symbols ();
    }
  
  /* Call the real memory allocator.  */
  p = _secondary_allocate (size);

  /* Get the probe.  */
  has_probe = mat_get_probe (&probe);

  /* Send the information only when this is possible (can have
     recursive calls to malloc from mat_get_probe, and the communication
     could also failed to be open).  */
  if (has_probe) 
    {
      mat_frame_add_skip (&probe, 1);
      mat_event_secondary_stack_allocate (&probe, p, size);
      mat_free_probe (&probe);
    }

  return p;
}

void
system__secondary_stack__ss_mark (gnat_secondary_mark_t *ptr)
{
  struct mat_probe probe;
  int has_probe;

  if (_secondary_mark == 0)
    {
      load_symbols ();
    }
  _secondary_mark (ptr);

  /* Get the probe information.  */
  has_probe = mat_get_probe (&probe);

  if (has_probe)
    {
      size_t size = compute_secondary_stack_size (ptr);
      mat_frame_add_skip (&probe, 1);
      mat_event_secondary_stack_mark (&probe, ptr, size);
      mat_free_probe (&probe);
    }
}

void
system__secondary_stack__ss_release (gnat_secondary_mark_t* ptr)
{
  struct mat_probe probe;
  int has_probe;

  /* Get the probe information.  */
  has_probe = mat_get_probe (&probe);

  if (has_probe) 
    {
      size_t size = compute_secondary_stack_size (ptr);
      mat_frame_add_skip (&probe, 1);
      mat_event_secondary_stack_release (&probe, ptr, size);
      mat_free_probe (&probe);
    }

  if (_secondary_release == 0)
    {
      load_symbols ();
    }
  _secondary_release (ptr);
}

