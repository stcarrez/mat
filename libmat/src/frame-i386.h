/* frame-i386.h -- Intel Stack frame
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
#ifndef _FRAME_I386_H
#define _FRAME_I386_H

#define FRAME_TYPE_DEFINED

#undef MAX_STACK_FRAME_PC
#define MAX_STACK_FRAME_PC	256

#undef ALIGN_MASK
#define ALIGN_MASK		0x07

#define	IS_ALIGNED_ADDR(X)	(((long)(X) & ALIGN_MASK) == 0)

#undef IS_VALID_ADDR
#define	IS_VALID_ADDR(X)	((((long)X) != 0))

#ifndef OVERHEAD_FRAME_COUNT
# define	OVERHEAD_FRAME_COUNT	0
#endif

struct frame
{
  struct frame *fp;
  void* pc;
};

static inline struct frame *
mat_get_frame_pointer ()
{
  struct frame *fp;

  __asm__ ("mov %%ebp,%0" : "=r"(fp));
  return fp;
}

static inline void *
mat_get_frame_pc (struct frame *fp)
{
  return fp->pc;
}

static inline struct frame *
mat_get_frame_next (struct frame *fp)
{
  return fp->fp;
}

static inline int
mat_frame_is_valid (struct frame *fp)
{
  return IS_VALID_ADDR (fp);
}

#endif
