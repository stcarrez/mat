/* shm-ops.h -- Shared Memory Segment Encapsulation
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
*/

#include "mat-config.h"
#include "shm-ops.h"

#include <stddef.h>
#include <errno.h>
#include <stdlib.h>

#ifdef HAVE_SYS_IPC_H
#  include <sys/ipc.h>
#endif

#ifdef HAVE_SYS_SHM_H
#  include <sys/shm.h>
#endif

#ifdef HAVE_SYS_SEM_H
#  include <sys/sem.h>
#endif

#if defined(__GNU_LIBRARY__) && !defined(_SEM_SEMUN_UNDEFINED)
/* union semun is defined by including <sys/sem.h> */
#else
/* according to X/OPEN we have to define it ourselves */
union semun {
  int val;                    /* value for SETVAL */
  struct semid_ds *buf;       /* buffer for IPC_STAT, IPC_SET */
  unsigned short int *array;  /* array for GETALL, SETALL */
  struct seminfo *__buf;      /* buffer for IPC_INFO */
};
#endif

/* Map the shared memory segment identified by the key `_key'
   in the process's address space. The memory segment is created
   only when the flag O_CREAT is passed in `_mode'.  */
int
mat_shm_create (struct mat_shm_segment *seg, long key, long size, 
               int mode, int creat)
{
  seg->segStart   = 0;
  seg->segCreated = 0;
  seg->segSize    = 0;
  seg->semId = -1;
  seg->semCreated = 0;

  mode = mode & 0777;
  mode |= (creat ? IPC_CREAT : 0);
  seg->segId = shmget (key, size, mode);
  if (seg->segId < 0) 
    {
      return -1;
    }

  seg->segCreated = creat != 0;

  /* Map the shared memory.  */
  seg->segStart = shmat (seg->segId, NULL, 0);
  if (seg->segStart == (void*) (0xffffffffL)) 
    {
      return -1;
    }
  seg->segSize = size;

  /* Create or get the semaphore used to block the sender.  */
  seg->semId = semget (key, 1, mode);
  if (seg->semId < 0)
    {
      return -1;
    }
  if (creat) 
    {
      union semun arg;

      seg->semCreated = 1;

      arg.val = 0;
      semctl (seg->semId, 0, SETVAL, arg);
    }

  return 0;
}


/* Unmap the shared memory segment and delete it if it was created
   by the constructor (O_CREAT).  */
void
mat_shm_destroy (struct mat_shm_segment *seg)
{
    int res __attribute__((unused));

  if (seg->segStart) 
    {
      res = shmdt ((char*) seg->segStart);
    }

  if (seg->segCreated) 
    {
      res = shmctl (seg->segId, IPC_RMID, (struct shmid_ds*) NULL);
    }

  if (seg->semId >= 0 && seg->semCreated) 
    {
      union semun su;

      su.val = 0;
      res = semctl (seg->semId, 0, IPC_RMID, su);
    }
}


/* Find the key which must be used to connect to the shared
   memory (or open the communication).  */
long
mat_shm_get_key (const char *env, const char *def)
{
  const char *n = getenv (env);
  long	val;

  if (n == 0) 
    {
      n = def;
      if (n == 0) 
        {
          return 0;
	}
    }

  /* Convert the string (hexadecimal form) into a long.  */
  for (val = 0; n[0]; n++) 
    {
      char c = *n;

      if (c >= 'a') 
        {
          c -= 'a' + 10;
	} 
      else if (c >= 'A') 
        {
          c -= 'A' + 10;
	} 
      else 
        {
          c -= '0';
	}
	
      val = (val << 4) | (long) (c & 0x0F);
    }
  return val;
}

/* Get the semaphore.  */
void
mat_shm_wait (struct mat_shm_segment *seg)
{
  struct sembuf sem;

  sem.sem_num = 0;
  sem.sem_op  = -1;
  sem.sem_flg = 0;

  while (1) 
    {
      int res = semop (seg->semId, &sem, 1);
      if (res == 0) break;
      if (errno != EINTR) 
        {
          break;
	}
    }
}


/* Release the semaphore.  */
void
mat_shm_wakeup (struct mat_shm_segment *seg)
{
  struct sembuf sem;
  int res __attribute__((unused));

  sem.sem_num = 0;
  sem.sem_op  = 1;
  sem.sem_flg = 0;

  res = semop (seg->semId, &sem, 1);
}

