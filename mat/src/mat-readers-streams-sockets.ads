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
with Ada.Finalization;

with Util.Streams.Buffered;
with Util.Streams.Files;
with Util.Streams.Sockets;
with GNAT.Sockets;
package MAT.Readers.Streams.Sockets is

   type Socket_Listener_Type is new Ada.Finalization.Limited_Controlled with private;

   --  Initialize the socket listener.
   overriding
   procedure Initialize (Listener : in out Socket_Listener_Type);

   --  Open the socket to accept connections and start the listener task.
   procedure Start (Listener : in out Socket_Listener_Type;
                    Address  : in GNAT.Sockets.Sock_Addr_Type);

   --  Stop the listener socket.
   procedure Stop (Listener : in out Socket_Listener_Type);

   type Socket_Reader_Type is new MAT.Readers.Streams.Stream_Reader_Type with private;
   type Socket_Reader_Type_Access is access all Socket_Reader_Type'Class;

   --  Open the socket to accept connections.
   procedure Open (Reader  : in out Socket_Reader_Type;
                   Address : in GNAT.Sockets.Sock_Addr_Type);

   procedure Close (Reader : in out Socket_Reader_Type);

private

   task type Socket_Listener_Task is
      entry Start (Address : in GNAT.Sockets.Sock_Addr_Type);
   end Socket_Listener_Task;

   task type Socket_Reader_Task is
      entry Start (S : in Socket_Reader_Type_Access;
                   Address : in GNAT.Sockets.Sock_Addr_Type);
   end Socket_Reader_Task;

   type Socket_Reader_Type is new MAT.Readers.Streams.Stream_Reader_Type with record
      Socket : aliased Util.Streams.Sockets.Socket_Stream;
      Server : Socket_Reader_Task;
      Stop   : Boolean := False;
   end record;

   type Socket_Listener_Type is new Ada.Finalization.Limited_Controlled with record
      Accept_Selector : GNAT.Sockets.Selector_Type;
      Listener        : Socket_Listener_Task;
   end record;

end MAT.Readers.Streams.Sockets;
