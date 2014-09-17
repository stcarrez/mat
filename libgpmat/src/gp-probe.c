/*  gp-probe.c --  Probe implementation
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

#include "config.h"
#include "gp-probe.h"
#include "gp-events.h"
#include <stdio.h>

#define GP_STACK_FRAME_MAX 256

static void* gp_stack_frame_buffer [GP_STACK_FRAME_MAX];

enum gp_probe_state
{
  GP_NOT_INITIALIZED = 0,
  GP_NOT_CONNECTED,
  GP_CONNECTED
};

static enum gp_probe_state gp_is_initialized = GP_NOT_INITIALIZED;

int
gp_probe_lock ()
{
  return 0;
}

void
gp_probe_unlock ()
{
  ;
}

int
gp_get_probe (struct gp_probe *gp)
{
  /* Initialize the client prober interface (if needed).  */
  switch (gp_is_initialized)
    {
    case GP_CONNECTED:
      break;

    case GP_NOT_CONNECTED:
      return 0;

    default:
      if (_gp_initialize () != 0)
        {
          return 0;
        }
    }

  /* Lock the prober interface (only one thread must use it at
     a time, and in particular the same thread must not recursively
     call the prober routines).  */
  if (gp_probe_lock () != 0)
    {
      return 0;
    }

  gp_get_probe_info (gp);

  gp->frame.frame_count = gp_fetch_stack_frame (gp_stack_frame_buffer,
                                                GP_STACK_FRAME_MAX, 1);
  gp->frame.frame_pc = gp_stack_frame_buffer;
  
  gp->thread.thread_id    = 0;
  gp->thread.thread_stack = (long) (gp) + sizeof (*gp);
  return 1;
}

void
gp_free_probe (struct gp_probe *gp)
{
  gp_probe_unlock ();
}

void
gp_exit ()
{
  struct gp_probe probe;
  int result;

  result = gp_get_probe (&probe);
    
  if (result == 0)
    {
      gp_event_end (&probe);
      gp_free_probe (&probe);
    }

  gp_remote_close ();
  gp_is_initialized = GP_NOT_INITIALIZED;
}

int
_gp_initialize ()
{
  struct gp_probe probe;
  int result;

  gp_is_initialized = GP_NOT_CONNECTED;

  /* Initialize the communication channel.  */
  result = gp_remote_initialize ();
  if (result < 0)
    {
      return result;
    }

  gp_is_initialized = GP_CONNECTED;
  (void) gp_get_probe (&probe);
  gp_event_begin (&probe);

  atexit (gp_exit);

  gp_free_probe (&probe);

  return 0;
}
