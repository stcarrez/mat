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

#include "mat-config.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#include "mat-remote.h"
#include "mat-probe.h"
#include "mat-events.h"

#ifndef HAVE_PTHREAD_H
# define pthread_mutex_lock(L)
# define pthread_mutex_unlock(L)
#endif

#ifndef RTLD_NEXT
# define RTLD_NEXT      ((void *) -1l)
#endif

int _mat_initialize (void);

#ifdef HAVE_FRAME
#define GP_STACK_FRAME_MAX 256

static void* mat_stack_frame_buffer [GP_STACK_FRAME_MAX];
#endif

#define LIBC_PTHREAD_MUTEX_LOCK    "pthread_mutex_lock"
#define LIBC_PTHREAD_MUTEX_UNLOCK  "pthread_mutex_unlock"
#define LIBC_PTHREAD_MUTEX_TRYLOCK "pthread_mutex_trylock"
#define LIBGCC_UNWIND_FIND_FDE     "_Unwind_Find_FDE"

enum mat_probe_state
{
  GP_NOT_INITIALIZED = 0,
  GP_NOT_CONNECTED,
  GP_CONNECTED,
  GP_CLOSED
};

static enum mat_probe_state mat_is_initialized = GP_NOT_INITIALIZED;
static void* mat_libgcc_s;

#ifdef HAVE_TLS

static __thread int mat_recursive = 0;

#elif defined(HAVE_PTHREAD_H)
static int mat_recursive = 0;
static pthread_t mat_recursive_thread = -1;
static pthread_mutex_t mat_recursive_lock = PTHREAD_MUTEX_INITIALIZER;

#else
static int mat_recursive = 0;
#endif

#ifdef HAVE_PTHREAD_H
static pthread_mutex_t mat_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

typedef int (* mat_mutex_lock_t) (pthread_mutex_t* m);
typedef int (* mat_mutex_unlock_t) (pthread_mutex_t* m);
typedef int (* mat_mutex_trylock_t) (pthread_mutex_t* m);
typedef void* (* mat_unwind_find_fde_t) (void* pc, void* bases);

static mat_mutex_lock_t _lock;
static mat_mutex_unlock_t _unlock;
static mat_mutex_trylock_t _trylock;
static mat_unwind_find_fde_t _unwind_find_fde;

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
mat_probe_lock (void)
{
#ifdef HAVE_TLS
  if (mat_recursive != 0)
    return -1;

  mat_recursive++;
  return mat_lock (&mat_mutex);

#elif defined(HAVE_PTHREAD_H)
  pthread_t self = pthread_self ();

  mat_lock (&mat_recursive_lock);
  if (self == mat_recursive_thread)
    {
      mat_unlock (&mat_recursive_lock);

      /* We already have the lock.  */
      return -1;
    }
  mat_unlock (&mat_recursive_lock);

  /* Get the probe lock and mark the thread as owner.  */
  mat_lock (&mat_mutex);

  mat_lock (&mat_recursive_lock);
  mat_recursive_thread = self;
  mat_recursive++;
  mat_unlock (&mat_recursive_lock);
  return 0;

#else
  if (mat_recursive != 0)
    return -1;

  mat_recursive++;  
  return 0;
#endif
}

void
mat_probe_unlock (void)
{
#ifdef HAVE_TLS
  mat_recursive--;
  if (mat_recursive == 0)
    mat_unlock (&mat_mutex);

#elif defined(HAVE_PTHREAD_H)
  int count;

  mat_lock (&mat_recursive_lock);
  count = --mat_recursive;
  if (count == 0)
      mat_recursive_thread = -1;

  mat_unlock (&mat_recursive_lock);

  /* Release the lock when the counter reaches 0.  */
  if (count == 0)
    mat_unlock (&mat_mutex);

#else
  mat_recursive--;
  if (mat_recursive == 0)
    mat_unlock (&mat_mutex);

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
    mat_recursive++;
    result = _unwind_find_fde (pc, bases);
    mat_recursive--;
    return result;
}

int
pthread_mutex_lock (pthread_mutex_t* mutex)
{
  struct mat_probe probe;
  int has_probe;

  /* Get the probe information.  */
  has_probe = mat_get_probe (&probe);

  if (has_probe) 
    {
      mat_frame_add_skip (&probe, 2);
      mat_event_mutex_lock (&probe, mutex);
      mat_free_probe (&probe);
    }

  return mat_lock (mutex);
}

int
pthread_mutex_unlock (pthread_mutex_t* mutex)
{
  struct mat_probe probe;
  int has_probe;

  /* Get the probe information.  */
  has_probe = mat_get_probe (&probe);

  if (has_probe) 
    {
      mat_frame_add_skip (&probe, 2);
      mat_event_mutex_lock (&probe, mutex);
      mat_free_probe (&probe);
    }

  return mat_unlock (mutex);
}

int
pthread_mutex_trylock (pthread_mutex_t* mutex)
{
  return _trylock(mutex);
}

int
mat_get_probe (struct mat_probe *gp)
{
  /* Initialize the client prober interface (if needed).  */
  switch (mat_is_initialized)
    {
    case GP_CONNECTED:
      break;

    case GP_NOT_CONNECTED:
    case GP_CLOSED:
      return 0;

    default:
      if (_mat_initialize () != 0)
        {
          return 0;
        }
    }

  /* Lock the prober interface (only one thread must use it at
     a time, and in particular the same thread must not recursively
     call the prober routines).  */
  if (mat_probe_lock () != 0)
    {
      return 0;
    }

  mat_get_probe_info (gp);

#ifdef HAVE_FRAME
  gp->frame.frame_count = mat_fetch_stack_frame (mat_stack_frame_buffer, GP_STACK_FRAME_MAX, 1);
  gp->frame.frame_skip_count = 2;
  gp->frame.frame_pc = mat_stack_frame_buffer;
#endif
  gp->thread.thread_stack = (long) (gp) + sizeof (*gp);
  return 1;
}

void
mat_free_probe (struct mat_probe *gp)
{
  mat_remote_sync ();
  mat_probe_unlock ();
}

void __attribute__ ((destructor))
mat_exit (void)
{
  struct mat_probe probe;
  int result;

  result = mat_get_probe (&probe);
    
  if (result == 0)
    {
      mat_event_end (&probe);
      mat_free_probe (&probe);
    }

  mat_remote_close ();
  mat_is_initialized = GP_CLOSED;
}

int
_mat_initialize (void)
{
  return mat_initialize (getenv("MAT_SERVER"));
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
mat_initialize (const char* p)
{
  struct mat_probe probe;
  int result;
  mat_mutex_lock_t lock;
  mat_mutex_unlock_t unlock;
  mat_mutex_trylock_t trylock;
  mat_unwind_find_fde_t find_fde;

  mat_is_initialized = GP_NOT_CONNECTED;

  /* Initialize the communication channel.  */
  result = mat_remote_initialize (p);
  if (result < 0)
    {
      return result;
    }

  /* The dlsym may call malloc.  Set the global variables
     only when we have everything.  */
  lock = (mat_mutex_lock_t) dlsym (RTLD_NEXT, LIBC_PTHREAD_MUTEX_LOCK);
  unlock = (mat_mutex_unlock_t) dlsym (RTLD_NEXT, LIBC_PTHREAD_MUTEX_UNLOCK);
  trylock = (mat_mutex_trylock_t) dlsym (RTLD_NEXT, LIBC_PTHREAD_MUTEX_TRYLOCK);
  find_fde = (mat_unwind_find_fde_t) dlsym (RTLD_NEXT, LIBGCC_UNWIND_FIND_FDE);
  if (find_fde == NULL)
    {
      mat_libgcc_s = dlopen ("libgcc_s.so.1", RTLD_LAZY);
      if (mat_libgcc_s != NULL)
        find_fde = (mat_unwind_find_fde_t) dlsym (mat_libgcc_s, LIBGCC_UNWIND_FIND_FDE);
    }

  _lock = lock;
  _unlock = unlock;
  _trylock = trylock;
  _unwind_find_fde = find_fde;

  mat_is_initialized = GP_CONNECTED;
  (void) mat_get_probe (&probe);
  mat_event_begin (&probe);

  mat_free_probe (&probe);

  return 0;
}
