/* frame-backtrace.c -- Stack frame unwinding (Glibc implementation)
--  Copyright (C) 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
*/
#include "mat-config.h"
#include <sys/types.h>
#include "mat-remote.h"
#include "mat-probe.h"

#ifdef HAVE_BACKTRACE
# include <execinfo.h>
#endif

int
mat_fetch_stack_frame (void **table, int size, int skip)
{
  return backtrace (table, size);
}

