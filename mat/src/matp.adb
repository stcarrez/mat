-----------------------------------------------------------------------
--  mat-types -- Global types
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
-----------------------------------------------------------------------
with Ada.IO_Exceptions;

with Util.Log.Loggers;

with GNAT.Sockets;

with MAT.Commands;
with MAT.Targets;
with MAT.Consoles.Text;
with MAT.Readers.Streams.Sockets;
procedure Matp is
   Target  : MAT.Targets.Target_Type;
   Console : aliased MAT.Consoles.Text.Console_Type;
   Server  : MAT.Readers.Streams.Sockets.Socket_Listener_Type;
   Address : GNAT.Sockets.Sock_Addr_Type;
begin
   Util.Log.Loggers.Initialize ("matp.properties");
   Target.Console (Console'Unchecked_Access);
   Address.Addr := GNAT.Sockets.Any_Inet_Addr;
   Address.Port := 4096;
   Server.Start (Address);
   MAT.Commands.Interactive (Target);
   Server.Stop;

exception
   when Ada.IO_Exceptions.End_Error =>
      Server.Stop;
end Matp;
