/*  gp-events.h -- Event operations
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

#ifndef _GP_EVENTS_H
#define _GP_EVENTS_H

typedef enum
{
  GP_TYPE_UINT8,
  GP_TYPE_UIN16,
  GP_TYPE_UIN32,
  GP_TYPE_UINT64,
  GP_TYPE_POINTER,
  GP_TYPE_SIZE_T
} gp_type_id;

typedef enum
{
  GP_EVENT_BEGIN,
  GP_EVENT_END,
  GP_EVENT_MALLOC,
  GP_EVENT_REALLOC,
  GP_EVENT_FREE
} gp_event_type;

struct gp_attr_def
{
  const char *name;
  gp_type_id type;
  int        size;
};

struct gp_event_def
{
  const char *name;
  gp_event_type type;
  int nr_attrs;
  const struct gp_attr_def *attributes;
};

extern void gp_event_send (struct gp_probe *gp, int size,
                           const struct gp_event_def *type, ...);

extern const struct gp_event_def gp_event_malloc_def;
extern void gp_event_malloc (struct gp_probe *gp, void *p, size_t size);

extern const struct gp_event_def gp_event_free_def;
extern void gp_event_free (struct gp_probe *gp, void *p);

extern const struct gp_event_def gp_event_realloc_def;
extern void gp_event_realloc (struct gp_probe *gp, void *p,
                              void *old, size_t size);

extern const struct gp_event_def gp_event_begin_def;
extern void gp_event_begin (struct gp_probe *gp);

extern const struct gp_event_def gp_event_end_def;
extern void gp_event_end (struct gp_probe *gp);

extern void gp_event_begin (struct gp_probe *gp);

extern void gp_event_end (struct gp_probe *gp);


#endif
