/* shm-channel.c -- Shared memory probe interface module
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
*/
#include "mat-config.h"
#include <stdio.h>
#include <string.h>
#include "shm-channel.h"
#include "shm-ops.h"

int
mat_shm_channel_create (struct mat_shm_channel *ch, const char *key,
                       long size)
{
  int result;

  ch->shm_key = mat_shm_get_key (key, SHARED_MEMORY_DEFAULT_KEY);
  result = mat_shm_create (&ch->shm_send, ch->shm_key, size, 0644, 0);
  return result;
}

/* Send the message pointed to by `_addr' and holding `_size' bytes.  */
    void
mat_shm_send (struct mat_shm_channel *ch, const void *_addr, long _size)
{
  volatile struct mat_shm_header *shm;
  
  if (mat_shm_is_initialized (&ch->shm_send) == 0)
    return;

  /* Get shared memory communication descriptor.  */
  shm = (struct mat_shm_header *) mat_shm_addr (&ch->shm_send);

  while (1) 
    {
      long freeSize;
      long readPos  = shm->readPos;
	
      if (readPos > shm->writePos) 
        {
          freeSize = readPos - shm->writePos - 1;
	}
      else if (readPos)
        {
          freeSize = shm->lastPos - shm->writePos;
	}
      else
        {
          freeSize = (shm->lastPos - shm->writePos) - 1;
        }
      
      if (freeSize > _size) 
        {
          freeSize = _size;
	}
	
      if (freeSize <= 0) 
        {
          shm->senderBlocked = 1;
          mat_shm_wait (&ch->shm_send);
          /* siSemSend->P();*/
          continue;
	}
	
      memcpy ((void*) &shm->buf[shm->writePos], (void*) _addr, freeSize);

      if (shm->writePos + freeSize == shm->lastPos) 
        {
          shm->writePos = 0;
	} 
      else 
        {
          shm->writePos += freeSize;
	}
      _size -= freeSize;
      if (_size == 0) 
        {
          break;
	}
      _addr = (const void*) (((const char*) _addr) + freeSize);
    }
}


/* Synchronize with the server. We block until the server has
   processed the previous message.  */
void
mat_shm_synchronize (struct mat_shm_channel *ch)
{
  volatile struct mat_shm_header *shm;

  if (mat_shm_is_initialized (&ch->shm_send) == 0)
    return;

  shm = (struct mat_shm_header *) mat_shm_addr (&ch->shm_send);

  if (shm->syncMode) 
    {
      shm->senderBlocked = 1;
      mat_shm_wait (&ch->shm_send);
      /* siSemSend->P(); */
    }
}

void
mat_shm_channel_destroy (struct mat_shm_channel *ch)
{
  mat_shm_destroy (&ch->shm_send);
}

