/*  gp-probe.h -- Probe definition
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

#ifndef _GP_PROBE_H
#define _GP_PROBE_H

typedef unsigned char gp_uint8;
typedef unsigned gp_uint8_varg;
typedef unsigned short gp_uint16;
typedef unsigned gp_uint16_varg;
typedef unsigned long gp_uint32;
typedef unsigned long long gp_uint64;

typedef gp_uint32 gp_addr;
// typedef gp_uint64 gp_addr;

#include "gp-unix.h"

/**
 * @brief Skip a number of frames in the backtrace report.
 *
 * @param gp the probe information.
 * @param skip the number of frames to skip.
 */
static inline void
gp_frame_add_skip (struct gp_probe *gp, int skip)
{
#ifdef HAVE_FRAME
    gp->frame.frame_skip_count += skip;
#endif
}

extern int gp_get_probe (struct gp_probe *gp);

extern void gp_free_probe (struct gp_probe *gp);

#endif
