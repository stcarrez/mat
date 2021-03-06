/*  Sock probe stream
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
#ifndef _GP_SOCKET_H
#define _GP_SOCKET_H

struct mat_socket_server 
{
  struct mat_buffered_server root;
  int fd;
};

/**
 * @brief Open the socket and prepare for probe monitoring to a remote TCP/IP port.
 *
 * The parameter string has the following format:
 *
 * tcp://host:port[?sync]
 *
 * @param server the socket server instance to initialize.
 * @param param the TCP/IP server to connect.
 * @return the GP server instance.
 */
extern struct mat_socket_server* mat_socket_open (struct mat_socket_server* server, const char* param);

#endif
