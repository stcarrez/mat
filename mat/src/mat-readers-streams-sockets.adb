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
with Ada.Streams;
with Util.Log.Loggers;

with MAT.Readers.Marshaller;
package body MAT.Readers.Streams.Sockets is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Readers.Sockets");

   BUFFER_SIZE  : constant Natural := 100 * 1024;
   MAX_MSG_SIZE : constant Ada.Streams.Stream_Element_Offset := 2048;

   --  ------------------------------
   --  Initialize the socket listener.
   --  ------------------------------
   overriding
   procedure Initialize (Listener : in out Socket_Listener_Type) is
   begin
      GNAT.Sockets.Create_Selector (Listener.Accept_Selector);
   end Initialize;

   --  ------------------------------
   --  Open the socket to accept connections and start the listener task.
   --  ------------------------------
   procedure Start (Listener : in out Socket_Listener_Type;
                    Address  : in GNAT.Sockets.Sock_Addr_Type) is
   begin
      Log.Info ("Starting the listener socket task");

      Listener.Listener.Start (Address);
   end Start;

   --  ------------------------------
   --  Stop the listener socket.
   --  ------------------------------
   procedure Stop (Listener : in out Socket_Listener_Type) is
   begin
      GNAT.Sockets.Abort_Selector (Listener.Accept_Selector);
   end Stop;

   task body Socket_Listener_Task is
      use type GNAT.Sockets.Socket_Type;

      Peer     : GNAT.Sockets.Sock_Addr_Type;
      Server   : GNAT.Sockets.Socket_Type;
      Instance : Socket_Reader_Type_Access;
      Socket   : GNAT.Sockets.Socket_Type;
      Status   : GNAT.Sockets.Selector_Status;
   begin
      select
         accept Start (S : in Socket_Reader_Type_Access;
                       Address : in GNAT.Sockets.Sock_Addr_Type) do
            Instance := S;
            GNAT.Sockets.Create_Socket (Server);
            GNAT.Sockets.Set_Socket_Option (Server, GNAT.Sockets.Socket_Level,
                                            (GNAT.Sockets.Reuse_Address, True));
            GNAT.Sockets.Bind_Socket (Server, Address);
            GNAT.Sockets.Listen_Socket (Server);
         end Start;
      or
         terminate;
      end select;
      while not Instance.Stop loop
         GNAT.Sockets.Accept_Socket (Server, Socket, Peer, 1.0, null, Status);
         if Socket /= GNAT.Sockets.No_Socket then
            Instance.Socket.Open (Socket);
            Instance.Read_All;
         end if;
      end loop;
      GNAT.Sockets.Close_Socket (Server);
   end Socket_Listener_Task;

   task body Socket_Reader_Task is
      use type GNAT.Sockets.Socket_Type;

      Peer     : GNAT.Sockets.Sock_Addr_Type;
      Server   : GNAT.Sockets.Socket_Type;
      Instance : Socket_Reader_Type_Access;
      Socket   : GNAT.Sockets.Socket_Type;
      Status   : GNAT.Sockets.Selector_Status;
   begin
      select
         accept Start (S : in Socket_Reader_Type_Access;
                       Address : in GNAT.Sockets.Sock_Addr_Type) do
            Instance := S;
            --              Address.Addr := GNAT.Sockets.Addresses (Get_Host_By_Name (S.Get_Host), 1);
--              Address.Addr := GNAT.Sockets.Any_Inet_Addr;
--              Address.Port := 4096;
            GNAT.Sockets.Create_Socket (Server);
            GNAT.Sockets.Set_Socket_Option (Server, GNAT.Sockets.Socket_Level,
                                            (GNAT.Sockets.Reuse_Address, True));
            GNAT.Sockets.Bind_Socket (Server, Address);
--              Address := GNAT.Sockets.Get_Socket_Name (Server);
            GNAT.Sockets.Listen_Socket (Server);
         end Start;
      or
         terminate;
      end select;
      while not Instance.Stop loop
         GNAT.Sockets.Accept_Socket (Server, Socket, Peer, 1.0, null, Status);
         if Socket /= GNAT.Sockets.No_Socket then
            Instance.Socket.Open (Socket);
            Instance.Read_All;
         end if;
      end loop;
      GNAT.Sockets.Close_Socket (Server);

   exception
      when E : others =>
         Log.Error ("Exception", E);

   end Socket_Reader_Task;

   --  Open the socket to accept connections.
   procedure Open (Reader  : in out Socket_Reader_Type;
                   Address : in GNAT.Sockets.Sock_Addr_Type) is
   begin
      Log.Info ("Reading server stream");

      Reader.Stream.Initialize (Size   => BUFFER_SIZE,
                                Input  => Reader.Socket'Unchecked_Access,
                                Output => null);
      Reader.Server.Start (Reader'Unchecked_Access, Address);
   end Open;

   procedure Close (Reader : in out Socket_Reader_Type) is
   begin
      Reader.Stop := True;
   end Close;

end MAT.Readers.Streams.Sockets;
