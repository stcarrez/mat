/*  File probe stream
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
#ifndef _GP_FILE_H
#define _GP_FILE_H

struct gp_file_server 
{
  struct gp_buffered_server root;
  int fd;
};

/**
 * @brief Open the file and prepare for probe monitoring on a file.
 *
 * @param server the file server instance to initialize.
 * @param param the file pattern to create.
 * @return the GP server instance.
 */
extern struct gp_file_server* gp_file_open (struct gp_file_server* server, const char* param);

#endif
