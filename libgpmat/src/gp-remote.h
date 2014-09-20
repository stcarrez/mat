/* Remote access
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
#ifndef _GP_REMOTE_H
#define _GP_REMOTE_H

struct gp_server;

typedef void (* gp_remote_send_t) (struct gp_server *, const void *, size_t);

typedef int (* gp_remote_sync_t) (struct gp_server *);

typedef void (* gp_remote_close_t) (struct gp_server *);

struct gp_server
{
  gp_remote_send_t to_send;
  gp_remote_sync_t to_synchronize;
  gp_remote_close_t to_close;
};

extern void gp_remote_send (const void *addr, size_t len);

extern void gp_remote_sync (void);

extern void gp_remote_close (void);

extern int gp_remote_initialize (void);

#endif
