/*  mat-unix.h -- Unix specific information
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

#ifndef _GP_UNIX_H
#define _GP_UNIX_H

#include <stddef.h>
#include <sys/time.h>

#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif

#ifdef HAVE_BACKTRACE
# define HAVE_FRAME 1
#endif

#ifdef HAVE__UNWIND_BACKTRACE
# define HAVE_FRAME 1
#endif

#ifdef HAVE_SYS_SYSCALL_H
# include <unistd.h>
# include <sys/syscall.h>
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

/**
 * @brief Get the current thread ID.
 *
 * On Linux the gettid() system call gives a more useful information about the
 * current thread.  This is however not portable.
 *
 * @return the thread ID.
 */
static inline mat_uint32 mat_get_thread_id (void)
{
#ifdef SYS_gettid
  return syscall (SYS_gettid);
#else
  return (mat_uint32) pthread_self ();
#endif
}

struct proc_info
{
  mat_uint32	pid;
  mat_addr	etext;
  mat_addr	edata;
  mat_addr	ebss;
  mat_addr	bss;
  mat_uint16 exe_length;
};

struct thread_info
{
  mat_uint32 thread_id;
  mat_addr	thread_stack;
};

struct rusage_info
{
  mat_uint32	ru_minflt;
  mat_uint32	ru_majflt;
  mat_uint32	ru_nvcsw;
  mat_uint32	ru_nivcsw;
};

#ifdef HAVE_FRAME
struct frame_info
{
  mat_uint16	frame_count;
  mat_uint16 frame_skip_count;
  void**	frame_pc;
};
#endif

typedef unsigned long long time_info;

struct mat_probe
{
  struct timeval        time;
  struct thread_info	thread;
#ifdef HAVE_RUSAGE
  struct rusage_info	rusage;
#endif
#ifdef HAVE_FRAME
  struct frame_info	frame;
#endif
};

static inline void
mat_get_probe_info (struct mat_probe *gp)
{
  gettimeofday (&gp->time, (struct timezone*) NULL);
  gp->thread.thread_id    = mat_get_thread_id ();
  gp->thread.thread_stack = 0;
#ifdef HAVE_RUSAGE
  {
    struct rusage ru;

    getrusage (RUSAGE_SELF, &ru);

    gp->rusage.ru_minflt = ru.ru_minflt;
    gp->rusage.ru_majflt = ru.ru_majflt;
    gp->rusage.ru_nvcsw  = ru.ru_nvcsw;
    gp->rusage.ru_nivcsw = ru.ru_nivcsw;
  }
#endif
}

static inline void
mat_remote_send_probe (struct mat_probe *gp)
{
  mat_uint32 val = gp->time.tv_sec;
  mat_remote_send (&val, sizeof (val));
  val = gp->time.tv_usec;
  mat_remote_send (&val, sizeof (val));
  mat_remote_send (&gp->thread, sizeof (gp->thread));
#ifdef HAVE_RUSAGE
  mat_remote_send (&gp->rusage, sizeof (gp->rusage));
#endif
#ifdef HAVE_FRAME
  if (gp->frame.frame_count > gp->frame.frame_skip_count)
    {
      mat_uint16 val = gp->frame.frame_count - gp->frame.frame_skip_count;
      
      mat_remote_send (&val, sizeof (val));
      mat_remote_send (&gp->frame.frame_pc[gp->frame.frame_skip_count],
                      (gp->frame.frame_count - gp->frame.frame_skip_count) * sizeof (void*));
    }
  else
    {
      mat_uint16 val = 0;

      mat_remote_send (&val, sizeof (val));
    }
#endif
}

static inline size_t
mat_remote_sizeof_probe (struct mat_probe *gp)
{
  size_t result;
  
  result = sizeof (gp->time) + sizeof (gp->thread);
#ifdef HAVE_RUSAGE
  result += sizeof (gp->rusage);
#endif
#ifdef HAVE_FRAME
  result += sizeof (gp->frame.frame_count);
  if (gp->frame.frame_count > gp->frame.frame_skip_count)
    result += (gp->frame.frame_count - gp->frame.frame_skip_count) * sizeof (void*);
#endif
  return result;
}

#ifdef HAVE_FRAME
extern int mat_fetch_stack_frame (void** table, int size, int skip);
#endif

#endif
