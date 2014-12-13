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
with Ada.Containers.Doubly_Linked_Lists;

with Util.Streams.Sockets;
with GNAT.Sockets;
package MAT.Readers.Streams.Sockets is

   type Socket_Listener_Type is new Ada.Finalization.Limited_Controlled with private;

   --  Initialize the socket listener.
   overriding
   procedure Initialize (Listener : in out Socket_Listener_Type);

   --  Destroy the socket listener.
   overriding
   procedure Finalize (Listener : in out Socket_Listener_Type);

   --  Open the socket to accept connections and start the listener task.
   procedure Start (Listener : in out Socket_Listener_Type;
                    List     : in MAT.Events.Probes.Reader_List_Type_Access;
                    Address  : in GNAT.Sockets.Sock_Addr_Type);

   --  Stop the listener socket.
   procedure Stop (Listener : in out Socket_Listener_Type);

   --  Create a target instance for the new client.
   procedure Create_Target (Listener : in out Socket_Listener_Type;
                            Client   : in GNAT.Sockets.Socket_Type;
                            Address  : in GNAT.Sockets.Sock_Addr_Type);

   type Socket_Reader_Type is new MAT.Readers.Streams.Stream_Reader_Type with private;
   type Socket_Reader_Type_Access is access all Socket_Reader_Type'Class;

   procedure Close (Reader : in out Socket_Reader_Type);

private

   type Socket_Listener_Type_Access is access all Socket_Listener_Type;

   task type Socket_Listener_Task is
      entry Start (Listener : in Socket_Listener_Type_Access;
                   Address  : in GNAT.Sockets.Sock_Addr_Type);
   end Socket_Listener_Task;

   task type Socket_Reader_Task is
      entry Start (Reader  : in Socket_Reader_Type_Access;
                   Client  : in GNAT.Sockets.Socket_Type);
   end Socket_Reader_Task;

   type Socket_Reader_Type is new MAT.Readers.Streams.Stream_Reader_Type with record
      Socket : aliased Util.Streams.Sockets.Socket_Stream;
      Server : Socket_Reader_Task;
      Client : GNAT.Sockets.Socket_Type;
      Stop   : Boolean := False;
   end record;

   package Socket_Client_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Socket_Reader_Type_Access);

   type Socket_Listener_Type is new Ada.Finalization.Limited_Controlled with record
      List            : MAT.Events.Probes.Reader_List_Type_Access;
      Accept_Selector : aliased GNAT.Sockets.Selector_Type;
      Listener        : Socket_Listener_Task;
      Clients         : Socket_Client_Lists.List;
   end record;

end MAT.Readers.Streams.Sockets;
