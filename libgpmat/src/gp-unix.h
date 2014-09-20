/*  gp-unix.h -- Unix specific information
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

#ifndef _GP_UNIX_H
#define _GP_UNIX_H

#include <stddef.h>
#include <sys/time.h>

#include "gp-remote.h"

#ifdef __cplusplus
/*extern "C" {*/
#endif

/*! @defgroup triacs Triacs Control Interface


 */
/*@{*/

struct proc_info
{
  long	pid;
  long	etext;
  long	edata;
  long	ebss;
  long	bss;
};

struct thread_info
{
  long	thread_id;
  long	thread_stack;
};

struct rusage_info
{
  long	ru_minflt;
  long	ru_majflt;
  long	ru_nswap;
};

struct frame_info
{
  long	frame_count;
  void*	frame_pc;
};

typedef unsigned long long time_info;

struct gp_probe
{
  struct timeval        time;
  struct thread_info	thread;
  struct rusage_info	rusage;
  struct frame_info	frame;
};

static inline void
gp_get_probe_info (struct gp_probe *gp)
{
  gettimeofday (&gp->time, (struct timezone*) NULL);

#ifdef HAVE_RUSAGE
  {
    struct rusage ru;

    getrusage (RUSAGE_SELF, &ru);

    gp->rusage.ru_minflt = ru.ru_minflt;
    gp->rusage.ru_majflt = ru.ru_majflt;
    gp->rusage.ru_nswap  = ru.ru_nswap;
  }
#endif
  
}

static inline void
gp_remote_send_probe (struct gp_probe *gp)
{
  gp_remote_send (&gp->time, sizeof (gp->time));
  gp_remote_send (&gp->thread, sizeof (gp->thread));
  gp_remote_send (&gp->rusage, sizeof (gp->rusage));
  gp_remote_send (&gp->frame.frame_count, sizeof (gp->frame.frame_count));
  gp_remote_send (gp->frame.frame_pc,
                  gp->frame.frame_count * sizeof (gp->frame.frame_pc[0]));
}

static inline size_t
gp_remote_sizeof_probe (struct gp_probe *gp)
{
  return sizeof (gp->time)
    + sizeof (gp->thread)
    + sizeof (gp->rusage)
    + sizeof (gp->frame.frame_count)
    + gp->frame.frame_count * sizeof (gp->frame.frame_pc[0]);
}

extern int gp_fetch_stack_frame (void** table, int size, int skip);

/*@}*/

#ifdef __cplusplus
/*};*/
#endif

#endif
