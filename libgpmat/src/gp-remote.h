/* Remote access
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
#ifndef _GP_REMOTE_H
#define _GP_REMOTE_H

#define GP_VERSION 1

#ifndef PATH_MAX
# define PATH_MAX 256
#endif

#ifndef O_CLOEXEC
# define O_CLOEXEC 0
#endif

#define DEFAULT_TCP_PORT 4096

struct gp_server;
struct gp_buffered_server;

typedef void (* gp_remote_send_t) (struct gp_server *, const void *, size_t);

typedef int (* gp_remote_sync_t) (struct gp_server *);

typedef void (* gp_remote_close_t) (struct gp_server *);

typedef int (* gp_remote_flush_t) (struct gp_buffered_server *);

struct gp_server
{
  gp_remote_send_t  to_send;
  gp_remote_sync_t  to_synchronize;
  gp_remote_close_t to_close;
};

struct gp_buffered_server
{
  struct gp_server  root;
  gp_remote_flush_t to_flush;
  unsigned char*    write_ptr;
  unsigned char*    last_ptr;
  unsigned char     buffer[4096];
};

/**
 * @brief Send the raw data to the file or socket.
 *
 * @param addr the data to send.
 * @param len the number of bytes to send.
 */
extern void gp_remote_send (const void *addr, size_t len);

/**
 * @brief Close the event probe stream.
 */
extern void gp_remote_close (void);

/**
 * @brief Synchronize the event probe stream (if enabled).
 */
extern void gp_remote_sync (void);

/**
 * @brief Initialize the connection to the server.
 *
 * file://<pattern>[?sync]     Write the probe stream in a file.
 * tcp://host:port[?sync]      Send the probe stream to the TCP/IP server.
 *
 * The optional <tt>sync</tt> flag enables the synchronous mode which flushes
 * the event probe stream after each operation.
 *
 * @return 0
 */
extern int gp_remote_initialize (void);

/**
 * @brief Initialize the buffered server instance.
 *
 * @param server the server instance.
 * @param param the configuration parameter.
 */
extern void gp_buffered_server_initialize (struct gp_buffered_server* server, const char* param);

#ifdef DEBUG
extern void gp_dump (const char* title, int indent, const void* addr, size_t len);
extern void gp_write (const char* title, int indent, const void* addr, size_t len);
#else
# define gp_dump(TITLE, INDENT, ADDR, LEN)
# define gp_write(TITLE, INDENT, ADDR, LEN) gp_remote_send(ADDR, LEN)
#endif

#endif
