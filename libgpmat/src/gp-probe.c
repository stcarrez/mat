/*  gp-probe.c --  Probe implementation
--  Copyright (C) 2011, 2012, 2013, 2014 Stephane Carrez
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
#include <dlfcn.h>
#include <stdio.h>
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#include "gp-remote.h"
#include "gp-probe.h"
#include "gp-events.h"

#ifndef HAVE_PTHREAD_H
# define pthread_mutex_lock(L)
# define pthread_mutex_unlock(L)
#endif

#ifndef RTLD_NEXT
# define RTLD_NEXT      ((void *) -1l)
#endif

int _gp_initialize (void);

#ifdef HAVE_FRAME
#define GP_STACK_FRAME_MAX 256

static void* gp_stack_frame_buffer [GP_STACK_FRAME_MAX];
#endif

#define LIBC_PTHREAD_MUTEX_LOCK    "pthread_mutex_lock"
#define LIBC_PTHREAD_MUTEX_UNLOCK  "pthread_mutex_unlock"
#define LIBC_PTHREAD_MUTEX_TRYLOCK "pthread_mutex_trylock"

enum gp_probe_state
{
  GP_NOT_INITIALIZED = 0,
  GP_NOT_CONNECTED,
  GP_CONNECTED,
  GP_CLOSED
};

static enum gp_probe_state gp_is_initialized = GP_NOT_INITIALIZED;

static __thread int gp_recursive = 0;
#ifdef HAVE_PTHREAD_H
static pthread_mutex_t gp_lock;
#endif

typedef int (* gp_mutex_lock_t) (pthread_mutex_t* m);
typedef int (* gp_mutex_unlock_t) (pthread_mutex_t* m);
typedef int (* gp_mutex_trylock_t) (pthread_mutex_t* m);

static gp_mutex_lock_t _lock;
static gp_mutex_unlock_t _unlock;
static gp_mutex_trylock_t _trylock;

int
gp_probe_lock (void)
{
  if (gp_recursive != 0)
    return -1;

  gp_recursive++;
  if (_lock != NULL)
    {
      _lock (&gp_lock);
    }
  return 0;
}

void
gp_probe_unlock (void)
{
  gp_recursive--;
  if (gp_recursive == 0 && _unlock)
    _unlock (&gp_lock);
}

int
pthread_mutex_lock (pthread_mutex_t* mutex)
{
  struct gp_probe probe;
  int has_probe;

  /* Get the probe information.  */
  has_probe = gp_get_probe (&probe);

  if (has_probe) 
    {
      gp_frame_add_skip (&probe, 2);
      gp_event_mutex_lock (&probe, mutex);
      gp_free_probe (&probe);
    }

  if (_lock != NULL)
    {
      return _lock (mutex);
    }

  return 0;
}

int
pthread_mutex_unlock (pthread_mutex_t* mutex)
{
  struct gp_probe probe;
  int has_probe;

  /* Get the probe information.  */
  has_probe = gp_get_probe (&probe);

  if (has_probe) 
    {
      gp_frame_add_skip (&probe, 2);
      gp_event_mutex_lock (&probe, mutex);
      gp_free_probe (&probe);
    }

  if (_unlock != NULL)
    {
      return _unlock (mutex);
    }

  return 0;
}

int
pthread_mutex_trylock (pthread_mutex_t* mutex)
{
    return _trylock(mutex)    ;
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
    case GP_CLOSED:
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

#ifdef HAVE_FRAME
  gp->frame.frame_count = gp_fetch_stack_frame (gp_stack_frame_buffer, GP_STACK_FRAME_MAX, 1);
  gp->frame.frame_skip_count = 2;
  gp->frame.frame_pc = gp_stack_frame_buffer;
#endif
  gp->thread.thread_stack = (long) (gp) + sizeof (*gp);
  return 1;
}

void
gp_free_probe (struct gp_probe *gp)
{
  gp_remote_sync ();
  gp_probe_unlock ();
}

void __attribute__ ((destructor))
gp_exit (void)
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
  gp_is_initialized = GP_CLOSED;
}

int
_gp_initialize (void)
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

  _lock = (gp_mutex_lock_t) dlsym (RTLD_NEXT, LIBC_PTHREAD_MUTEX_LOCK);
  _unlock = (gp_mutex_unlock_t) dlsym (RTLD_NEXT, LIBC_PTHREAD_MUTEX_UNLOCK);
  _trylock = (gp_mutex_trylock_t) dlsym (RTLD_NEXT, LIBC_PTHREAD_MUTEX_TRYLOCK);
  
  gp_is_initialized = GP_CONNECTED;
  (void) gp_get_probe (&probe);
  gp_event_begin (&probe);

  gp_free_probe (&probe);

  return 0;
}
