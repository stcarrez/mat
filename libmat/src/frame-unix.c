/* frame-unix.c -- Stack frame unwinding (Unix implementation)
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
*/

/*
**
** Frame pointer :
** ---------------
**
**	In most computers, the frame pointer is a register which is
** used to keep track of the different nested calls of functions. It
** represents the head of a single linked list of a structure
** containing the return address of the function. This list is used
** by the debugger (dbx, gdb, adb) to print the hierarchie of function
** calls. The stack looks like:
**
** F2		|		|
**		+---------------+
**		|		|<------+
**		+---------------+	|
**		|		|	|
**		|		|	|
** F1		+---------------+	|
**	fp  ->	|		|-------/
**		+---------------+
**	sp  ->	|		|
**
**
**	One function is implemented which walks the frame pointer list and
** collects the return address of functions in a global table. While this
** list is walked, the SIGBUS and SIGSEGV signals are catched. If one of
** these signal is received, the analysis of the stack stops (This is a
** security against stack corruption; On some processors such as Motorola's
** the frame pointer is not mandatory and can be removed by the compiler
** which can use the register for another purpose. In these cases, the
** stack analysis might be broken...)
*/
#include "mat-config.h"
#include <stdio.h>
#include "mat-remote.h"
#include "mat-probe.h"

#ifdef HAVE_BACKTRACE
# include <execinfo.h>
#endif

#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif

#ifdef HAVE_SETJMP_H
# include <setjmp.h>
#endif

#ifdef sparc
#  include <sun4/frame.h>
#endif

#ifdef sun3
#  include <sun3/frame.h>
#endif

#ifdef HAVE_UNWIND_H
# include <unwind.h>
#endif

#define	OVERHEAD_FRAME_COUNT	1

#if defined(__i386) || defined(i386)
#  include "frame-i386.h"
#endif

#ifndef FRAME_TYPE_DEFINED
#error "CPU not supported for frame unwinding"
#endif

#ifdef HAVE_REGISTER_WINDOW

#  ifndef REGISTER_WINDOW_COUNT
#    define REGISTER_WINDOW_COUNT	(32-1)
#  endif

/* Force the processor to flush its register windows onto the stack.
   If the registers are not correcly flushed, the stack contains
   wrong results which do not reflect the current stack frame.
   Sparc processors are in this case.  */
static int
mat_flush_registers (int count)
{
  if (count > 0)
    {
      count = mat_flush_registers (count - 1);
    }

  return count;
}
#endif

#ifdef HAVE_BACKTRACE
int
mat_fetch_stack_frame (void **table, int size, int skip)
{
    return backtrace (table, size);
}
#else

static jmp_buf	mat_frame_restart_point;

/* A SIGBUS or SIGSEGV signal has been received while
   examining the stack frame.  */
static void
catch_bus_error (int sig)
{
  longjmp (mat_frame_restart_point, 1);
}

int
mat_fetch_stack_frame (void **table, int size, int skip)
{
  struct frame	*fp;
  auto int nr_frames = 0;

#ifdef HAVE_SIGACTION
  struct sigaction	oact_SIGBUS;
  struct sigaction	oact_SIGSEGV;
  struct sigaction	act;
  sigset_t set;
  sigset_t oldset;

  sigfillset (&set);
  sigdelset (&set, SIGBUS);
  sigdelset (&set, SIGSEGV);
  (void) sigprocmask (SIG_BLOCK, &set, &oldset);

  /* Catch the SIGBUS and SIGSEGV signals.  */
  act.sa_handler = catch_bus_error;
  sigemptyset (&act.sa_mask);
  act.sa_flags   = 0;

  /* We have to set a proper value to `RestartPoint'
     before setting the new handlers (In most cases this is
     not needed but this is safer: a signal might arrive
     after `sigaction' and before the second `setjmp').  */
  if (setjmp (mat_frame_restart_point) != 0) 
    {
      sigaction (SIGBUS, &oact_SIGBUS, (struct sigaction *) 0);
      sigaction (SIGSEGV, &oact_SIGSEGV, (struct sigaction *) 0);

      (void) sigprocmask (SIG_SETMASK, &oldset, NULL);
      return 0;
    }

  /* Setup new handlers.  */
  sigaction (SIGBUS, &act, &oact_SIGBUS);
  sigaction (SIGSEGV, &act, &oact_SIGSEGV);

  if (setjmp (mat_frame_restart_point) != 0) 
    {
      sigaction (SIGBUS, &oact_SIGBUS, (struct sigaction *) 0);
      sigaction (SIGSEGV, &oact_SIGSEGV, (struct sigaction *) 0);

      (void) sigprocmask (SIG_SETMASK, &oldset, NULL);
      return nr_frames;
    }
#endif

#ifdef REGISTER_WINDOW_COUNT
  /* We have to make sure that the processor's registers are copied in
     memory. This is done by forcing the processor to flush its
     register windows.  */
  mat_flush_registers (REGISTER_WINDOW_COUNT);
#endif

  /* Call the assembly routine to get the frame pointer.  */
  skip++;
  fp = mat_get_frame_pointer ();

  nr_frames = 0;

  /* Make sure this is a valid frame pointer
     and the table is not full.  */
  while (mat_frame_is_valid (fp) && nr_frames < size)
    {
      if (skip > 0)
        {
          skip--;
        }
      else
        {
          *table++ = mat_get_frame_pc (fp);
          nr_frames++;
        }

      /* Get the next frame.  */
      fp = mat_get_frame_next (fp);
    }

#ifdef HAVE_SIGACION
  sigaction (SIGBUS, &oact_SIGBUS, (struct sigaction *) 0);
  sigaction (SIGSEGV, &oact_SIGSEGV, (struct sigaction *) 0);

  (void) sigprocmask (SIG_SETMASK, &oldset, NULL);
#endif

  return nr_frames;
}
#endif

