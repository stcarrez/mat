/*  mat-events.h -- Event operations
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2023 Stephane Carrez
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

#define GP_LITTLE_ENDIAN (0)
#define GP_BIG_ENDIAN    (1)

#define GP_TABLE_SIZE(T) ((sizeof(T)) / sizeof(T[0]))

typedef enum
{
  GP_CPU_X86    = 0x8600,
  GP_CPU_X86_64 = 0x8664,
  GP_CPU_MIPS
} mat_cpu_id;
    
/**
 * @brief Basic data types which can be sent to the event probe stream.
 */
typedef enum
{
  GP_TYPE_UINT8,
  GP_TYPE_UINT16,
  GP_TYPE_UINT32,
  GP_TYPE_UINT64,
  GP_TYPE_STRING
} mat_type_id;

#define GP_TYPE_SIZE_T  GP_TYPE_UINT32
#define GP_TYPE_POINTER GP_TYPE_UINT32

typedef mat_uint32 mat_pointer;

/**
 * @brief The list of probe events which can be sent in the event probe  stream.
 */
typedef enum
{
  GP_EVENT_BEGIN,
  GP_EVENT_END,
  GP_EVENT_MALLOC,
  GP_EVENT_REALLOC,
  GP_EVENT_FREE,
  GP_EVENT_MUTEX_LOCK,
  GP_EVENT_MUTEX_TRYLOCK,
  GP_EVENT_MUTEX_UNLOCK,
  GP_EVENT_SHLIB,
  GP_EVENT_SECONDARY_STACK_MARK,
  GP_EVENT_SECONDARY_STACK_ALLOCATE,
  GP_EVENT_SECONDARY_STACK_RELEASE
} mat_event_type;


/**
 * @brief Attribute description.
 *
 * Each attribute description describes a value that is sent in the event data stream.
 */
struct mat_attr_def
{
  const char *name;
  mat_type_id type;
  int        size;
};

/**
 * @brief Event description.
 *
 * The event description describes the values that are sent in the event data
 * stream for a given event probe.
 */
struct mat_event_def
{
  const char *name;
  mat_event_type type;
  size_t size;
  int nr_attrs;
  const struct mat_attr_def *attributes;
};

extern void mat_event_send (struct mat_probe *gp, int size,
                           const struct mat_event_def *type, ...);

extern void mat_event_malloc (struct mat_probe *gp, void *p, size_t size, void* curbrk);

extern void mat_event_free (struct mat_probe *gp, void *p);

extern void mat_event_realloc (struct mat_probe *gp, void *p,
                              void *old, size_t size, void* curbrk);

extern void mat_event_begin (struct mat_probe *gp);

extern void mat_event_end (struct mat_probe *gp);

extern void mat_event_begin (struct mat_probe *gp);

extern void mat_event_end (struct mat_probe *gp);

extern void mat_event_mutex_lock (struct mat_probe *gp, void* mutex);
extern void mat_event_mutex_unlock (struct mat_probe *gp, void* mutex);
extern void mat_event_mutex_trylock (struct mat_probe *gp, void* mutex);

extern void mat_event_secondary_stack_mark (struct mat_probe *gp, void *mark, size_t size);
extern void mat_event_secondary_stack_allocate (struct mat_probe *gp, void* p, size_t size);
extern void mat_event_secondary_stack_release (struct mat_probe *gp, void *mark, size_t size);

#endif
