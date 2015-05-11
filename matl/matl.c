/*  matl.c --  Wrapper to launch a program with memory analysis
--  Copyright (C) 2015 Stephane Carrez
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
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <netdb.h>
#include <arpa/inet.h>

#define DEFAULT_PORT 4606

static int usage(void)
{
  fprintf (stderr, "Usage: matl [-o file] [-s host[:port]] command\n");
  return 2;
}

static void set_preload(void)
{
  putenv ("LD_PRELOAD=libmat.so");
}

static void set_file(const char* file)
{
  char* env = (char*) malloc (strlen (file) + sizeof ("MAT_SERVER=file://"));

  sprintf (env, "MAT_SERVER=file://%s", file);
  putenv (env);
}

static void set_server(const struct in_addr* ip, unsigned port)
{
  char buf[16];
  char* env = (char*) malloc (sizeof (buf) + sizeof ("MAT_SERVER=tcp://:nnnnn"));

  inet_ntop (AF_INET, ip, buf, sizeof (buf));
  sprintf (env, "MAT_SERVER=tcp://%s:%u", buf, port);
  putenv (env);
}

int main(int argc, char* argv[])
{
  char* file = NULL;
  char* server = NULL;
  int i;

  for (i = 1; i < argc; i++)
    {
      if (strcmp (argv[i], "-o") == 0)
        {
          i++;
          if (i == argc)
            return usage ();

          file = argv[i];
          set_file (file);
        }
      else if (strcmp (argv[i], "-s") == 0)
        {
      char* p;
      struct in_addr ip;
      unsigned port = DEFAULT_PORT;

          i++;
          if (i == argc)
            return usage ();

          server = argv[i];
      p = strchr (server, ':');

      /* Verify the port number if there is one.  */
      if (p != NULL)
        {
          *p = 0;
          p ++;
          port = strtol (p, &p, 10);
          if (port <= 0 || port >= 65536 || *p != 0)
        {
          fprintf (stderr, "matl: invalid TCP/IP port '%s'\n", p);
          return EXIT_FAILURE;
        }
        }

      /* Verify that the server is a valid IP address.  The libmat.so library accepts
       * only IP addresses in the dot notation.  Resolve the hostname here.
       */
      if (inet_pton (AF_INET, server, &ip) != 1)
        {
              struct hostent* hp = gethostbyname (server);
           
          if (hp == NULL)
            {
          fprintf (stderr, "matl: cannot find IP address associated with '%s'\n", server);
          return EXIT_FAILURE;
        }
          ip = *(struct in_addr*) hp->h_addr_list[0];
        }
      
          set_server (&ip, port);
        }
      else if (argv[i][0] == '-')
        {
          return usage ();
        }
      else
        {
          set_preload ();
          execvp (argv[i], &argv[i]);
          fprintf (stderr, "%s: not found\n", argv[i]);
          exit (255);
        }
    }
  return usage ();
}
