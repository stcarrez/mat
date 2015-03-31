/* shm-ops.h -- Shared Memory Segment Encapsulation
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

#ifndef _SHARED_MEMORY_OPS_H
#define _SHARED_MEMORY_OPS_H

/* The `SharedSegment' class encapsulates the Unix shared memory
   abstraction. The constructor initializes the shared memory
   segment, and creates it if needed. The destructor releases the
   system resources and physically deletes the segment if the
   constructor created it.

   The segment is immediately mapped in memory, its size and
   starting address can be fetched using size() and addr().  */
struct mat_shm_segment 
{
  void*	segStart;
  long	segSize;
  int	segId;
  int   semId;
  char	segCreated;
  char  semCreated;
};

/* Map the shared memory segment identified by the key `_key'
   in the process's address space. The memory segment is created
   only when the flag O_CREAT is passed in `_mode'.  */
extern int mat_shm_create (struct mat_shm_segment *seg,
                          long key, long size, int mode, int creat);

/* Unmap the shared memory segment and delete it if it was created
   by the constructor (O_CREAT).  */
extern void mat_shm_destroy (struct mat_shm_segment *seg);

extern long mat_shm_get_key (const char *env, const char *def);

/* Get the semaphore.  */
extern void mat_shm_wait (struct mat_shm_segment *seg);


static inline int
mat_shm_is_initialized (struct mat_shm_segment *seg)
{
  return seg->segId >= 0;
}

static inline void *
mat_shm_addr (struct mat_shm_segment *seg)
{
  return seg->segStart;
}


#endif
