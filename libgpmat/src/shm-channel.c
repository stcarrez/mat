/* shm-channel.c -- Shared memory probe interface module
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
#include <stdio.h>
#include <string.h>
#include "shm-channel.h"
#include "shm-ops.h"

int
gp_shm_channel_create (struct gp_shm_channel *ch, const char *key,
                       long size)
{
  int result;

  ch->shm_key = gp_shm_get_key (key, SHARED_MEMORY_DEFAULT_KEY);
  result = gp_shm_create (&ch->shm_send, ch->shm_key, size, 0644, 0);
  return result;
}

/* Send the message pointed to by `_addr' and holding `_size' bytes.  */
    void
gp_shm_send (struct gp_shm_channel *ch, const void *_addr, long _size)
{
  volatile struct gp_shm_header *shm;
  
  if (gp_shm_is_initialized (&ch->shm_send) == 0)
    return;

  /* Get shared memory communication descriptor.  */
  shm = (struct gp_shm_header *) gp_shm_addr (&ch->shm_send);

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
          gp_shm_wait (&ch->shm_send);
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
gp_shm_synchronize (struct gp_shm_channel *ch)
{
  volatile struct gp_shm_header *shm;

  if (gp_shm_is_initialized (&ch->shm_send) == 0)
    return;

  shm = (struct gp_shm_header *) gp_shm_addr (&ch->shm_send);

  if (shm->syncMode) 
    {
      shm->senderBlocked = 1;
      gp_shm_wait (&ch->shm_send);
      /* siSemSend->P(); */
    }
}

void
gp_shm_channel_destroy (struct gp_shm_channel *ch)
{
  gp_shm_destroy (&ch->shm_send);
}

