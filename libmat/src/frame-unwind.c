/* Return backtrace of current program state.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Jakub Jelinek <jakub@redhat.com>, 2003.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   SPDX-License-Identifier: LGPL-2.1+

*/
#include <stdlib.h>
#include <unwind.h>

struct trace_arg
{
  void **array;
  _Unwind_Word cfa;
  int cnt;
  int size;
};

static _Unwind_Reason_Code
backtrace_helper (struct _Unwind_Context *ctx, void *a)
{
  struct trace_arg *arg = a;

  /* We are first called with address in the __backtrace function.
     Skip it.  */
  if (arg->cnt != -1)
    {
      arg->array[arg->cnt] = (void *) _Unwind_GetIP (ctx);

      /* Check whether we make any progress.  */
      _Unwind_Word cfa = _Unwind_GetCFA (ctx);

      if (arg->cnt > 0 && arg->array[arg->cnt - 1] == arg->array[arg->cnt]
	 && cfa == arg->cfa)
       return _URC_END_OF_STACK;
      arg->cfa = cfa;
    }
  if (++arg->cnt == arg->size)
    return _URC_END_OF_STACK;
  return _URC_NO_REASON;
}

int
mat_fetch_stack_frame (void **array, int size, int skip)
{
  struct trace_arg arg = { .array = array, .cfa = 0, .size = size, .cnt = -1 };

  if (size >= 1)
      _Unwind_Backtrace (backtrace_helper, &arg);

  /* _Unwind_Backtrace seems to put NULL address above
     _start.  Fix it up here.  */
  if (arg.cnt > 1 && arg.array[arg.cnt - 1] == NULL)
    --arg.cnt;
  return arg.cnt != -1 ? arg.cnt : 0;
}
