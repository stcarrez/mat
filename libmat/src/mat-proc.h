/* mat-proc.c -- Information extracted from /proc
--  Copyright (C) 2014 Stephane Carrez
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
#ifndef _GP_PROC_H
#define _GP_PROC_H

#define MAP_R_OK 0x01
#define MAP_W_OK 0x02
#define MAP_X_OK 0x04
#define MAP_PRIV 0x08

typedef struct map_info
{
  void* start;
  void* end;
  int   flags;
  char* name;
} map_info_t;

typedef void (* gp_extract_map_callback) (map_info_t* map, void* data);

/**
 * @brief Read the process /proc/<pid>/maps file.
 *
 * @param pid the process id.
 * @param callback the callback execute for each map entry.
 * @param data the callback data.
 */
extern void
gp_read_proc_maps (int pid, gp_extract_map_callback callback, void *data);

#endif
