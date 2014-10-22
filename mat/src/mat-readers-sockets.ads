-----------------------------------------------------------------------
--  mat-readers-sockets -- Reader for TCP/IP sockets
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
with Util.Streams.Buffered;
with Util.Streams.Files;
with Util.Streams.Sockets;
with GNAT.Sockets;
package MAT.Readers.Sockets is

   type Socket_Reader_Type is new Manager_Base with private;
   type Socket_Reader_Type_Access is access all Socket_Reader_Type'Class;

   --  Open the socket to accept connections.
   procedure Open (Reader  : in out Socket_Reader_Type;
                   Address : in GNAT.Sockets.Sock_Addr_Type);

   procedure Read_All (Reader : in out Socket_Reader_Type);

   procedure Close (Reader : in out Socket_Reader_Type);

private

   task type Socket_Reader_Task is
      entry Start (S : in Socket_Reader_Type_Access;
                   Address : in GNAT.Sockets.Sock_Addr_Type);
   end Socket_Reader_Task;

   type Socket_Reader_Type is new Manager_Base with record
      Socket : aliased Util.Streams.Sockets.Socket_Stream;
      Stream : Util.Streams.Buffered.Buffered_Stream;
      Server : Socket_Reader_Task;
      Stop   : Boolean := False;
   end record;

end MAT.Readers.Sockets;
