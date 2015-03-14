/*  gp-probe.c --  Probe implementation
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

#include "gp-config.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
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
#define LIBGCC_UNWIND_FIND_FDE     "_Unwind_Find_FDE"

enum gp_probe_state
{
  GP_NOT_INITIALIZED = 0,
  GP_NOT_CONNECTED,
  GP_CONNECTED,
  GP_CLOSED
};

static enum gp_probe_state gp_is_initialized = GP_NOT_INITIALIZED;
static void* gp_libgcc_s;

#ifdef HAVE_TLS

static __thread int gp_recursive = 0;

#elif defined(HAVE_PTHREAD_H)
static int gp_recursive = 0;
static pthread_t gp_recursive_thread = -1;
static pthread_mutex_t gp_recursive_lock = PTHREAD_MUTEX_INITIALIZER;

#else
static int gp_recursive = 0;
#endif

#ifdef HAVE_PTHREAD_H
static pthread_mutex_t gp_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

typedef int (* gp_mutex_lock_t) (pthread_mutex_t* m);
typedef int (* gp_mutex_unlock_t) (pthread_mutex_t* m);
typedef int (* gp_mutex_trylock_t) (pthread_mutex_t* m);
typedef void* (* gp_unwind_find_fde_t) (void* pc, void* bases);

static gp_mutex_lock_t _lock;
static gp_mutex_unlock_t _unlock;
static gp_mutex_trylock_t _trylock;
static gp_unwind_find_fde_t _unwind_find_fde;

static inline int mat_lock (pthread_mutex_t* m)
{
  if (_lock != NULL)
    return _lock (m);
  else
    return 0;
}

static inline int mat_unlock (pthread_mutex_t* m)
{
  if (_unlock != NULL)
    return _unlock (m);
  else
    return 0;
}

int
gp_probe_lock (void)
{
#ifdef HAVE_TLS
  if (gp_recursive != 0)
    return -1;

  gp_recursive++;
  return mat_lock (&gp_lock);

#elif defined(HAVE_PTHREAD_H)
  pthread_t self = pthread_self ();

  mat_lock (&gp_recursive_lock);
  if (self == gp_recursive_thread)
    {
      mat_unlock (&gp_recursive_lock);

      /* We already have the lock.  */
      return -1;
    }
  mat_unlock (&gp_recursive_lock);

  /* Get the probe lock and mark the thread as owner.  */
  mat_lock (&gp_lock);

  mat_lock (&gp_recursive_lock);
  gp_recursive_thread = self;
  gp_recursive++;
  mat_unlock (&gp_recursive_lock);
  return 0;

#else
  if (gp_recursive != 0)
    return -1;

  gp_recursive++;  
  return 0;
#endif
}

void
gp_probe_unlock (void)
{
#ifdef HAVE_TLS
  gp_recursive--;
  if (gp_recursive == 0)
    mat_unlock (&gp_lock);

#elif defined(HAVE_PTHREAD_H)
  int count;

  mat_lock (&gp_recursive_lock);
  count = --gp_recursive;
  if (count == 0)
      gp_recursive_thread = -1;

  mat_unlock (&gp_recursive_lock);

  /* Release the lock when the counter reaches 0.  */
  if (count == 0)
    mat_unlock (&gp_lock);

#else
  gp_recursive--;
  if (gp_recursive == 0)
    mat_unlock (&gp_lock);

#endif
}

void*
_Unwind_Find_FDE (void* pc, void* bases)
{
    void* result;

    /* The real _Unwind_Find_FDE will pthread_mutex_lock to protect some global
     * variable.  Since the GCC unwinder is used for exceptions as well as from
     * within this library, this creates a deadlock when we try to instrument
     * the pthread_mutex_lock.  Increment the per-thread recursion variable to
     * avoid monitoring these mutex calls.
     */
    gp_recursive++;
    result = _unwind_find_fde (pc, bases);
    gp_recursive--;
    return result;
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

  return mat_lock (mutex);
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

  return mat_unlock (mutex);
}

int
pthread_mutex_trylock (pthread_mutex_t* mutex)
{
  return _trylock(mutex);
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
  return gp_initialize (getenv("MAT_SERVER"));
}

/**
 * @brief Initialize the connection to the server.
 *
 * file://<pattern>[?sync]     Write the probe stream in a file.
 * tcp://host:port[?sync]      Send the probe stream to the TCP/IP server.
 *
 * The optional <tt>sync</tt> flag enables the synchronous mode which flushes
 * the event probe stream after each operation.
 *
 * @param p the connection string.
 * @return 0
 */
int
gp_initialize (const char* p)
{
  struct gp_probe probe;
  int result;
  gp_mutex_lock_t lock;
  gp_mutex_unlock_t unlock;
  gp_mutex_trylock_t trylock;
  gp_unwind_find_fde_t find_fde;

  gp_is_initialized = GP_NOT_CONNECTED;

  /* Initialize the communication channel.  */
  result = gp_remote_initialize (p);
  if (result < 0)
    {
      return result;
    }

  /* The dlsym may call malloc.  Set the global variables
     only when we have everything.  */
  lock = (gp_mutex_lock_t) dlsym (RTLD_NEXT, LIBC_PTHREAD_MUTEX_LOCK);
  unlock = (gp_mutex_unlock_t) dlsym (RTLD_NEXT, LIBC_PTHREAD_MUTEX_UNLOCK);
  trylock = (gp_mutex_trylock_t) dlsym (RTLD_NEXT, LIBC_PTHREAD_MUTEX_TRYLOCK);
  find_fde = (gp_unwind_find_fde_t) dlsym (RTLD_NEXT, LIBGCC_UNWIND_FIND_FDE);
  if (find_fde == NULL)
    {
      gp_libgcc_s = dlopen ("libgcc_s.so.1", RTLD_LAZY);
      if (gp_libgcc_s != NULL)
        find_fde = (gp_unwind_find_fde_t) dlsym (gp_libgcc_s, LIBGCC_UNWIND_FIND_FDE);
    }

  _lock = lock;
  _unlock = unlock;
  _trylock = trylock;
  _unwind_find_fde = find_fde;

  gp_is_initialized = GP_CONNECTED;
  (void) gp_get_probe (&probe);
  gp_event_begin (&probe);

  gp_free_probe (&probe);

  return 0;
}
