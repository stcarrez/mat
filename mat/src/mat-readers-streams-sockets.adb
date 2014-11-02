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

package body MAT.Readers.Streams.Sockets is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Readers.Sockets");

   BUFFER_SIZE  : constant Natural := 100 * 1024;

   --  ------------------------------
   --  Initialize the socket listener.
   --  ------------------------------
   overriding
   procedure Initialize (Listener : in out Socket_Listener_Type) is
   begin
      GNAT.Sockets.Create_Selector (Listener.Accept_Selector);
   end Initialize;

   --  ------------------------------
   --  Destroy the socket listener.
   --  ------------------------------
   overriding
   procedure Finalize (Listener : in out Socket_Listener_Type) is
      use type GNAT.Sockets.Selector_Type;
   begin
      GNAT.Sockets.Close_Selector (Listener.Accept_Selector);
   end Finalize;

   --  ------------------------------
   --  Open the socket to accept connections and start the listener task.
   --  ------------------------------
   procedure Start (Listener : in out Socket_Listener_Type;
                    Address  : in GNAT.Sockets.Sock_Addr_Type) is
   begin
      Log.Info ("Starting the listener socket task");

      Listener.Listener.Start (Listener'Unchecked_Access, Address);
   end Start;

   --  ------------------------------
   --  Stop the listener socket.
   --  ------------------------------
   procedure Stop (Listener : in out Socket_Listener_Type) is
   begin
      GNAT.Sockets.Abort_Selector (Listener.Accept_Selector);
   end Stop;

   --  Create a target instance for the new client.
   procedure Create_Target (Listener : in out Socket_Listener_Type;
                            Client   : in GNAT.Sockets.Socket_Type;
                            Address  : in GNAT.Sockets.Sock_Addr_Type) is
      Reader : Socket_Reader_Type_Access := new Socket_Reader_Type;
   begin
      null;
   end Create_Target;

   task body Socket_Listener_Task is
      use type GNAT.Sockets.Socket_Type;
      use type GNAT.Sockets.Selector_Status;

      Peer     : GNAT.Sockets.Sock_Addr_Type;
      Server   : GNAT.Sockets.Socket_Type;
      Instance : Socket_Listener_Type_Access;
      Client   : GNAT.Sockets.Socket_Type;
      Status   : GNAT.Sockets.Selector_Status;
      Selector_Status : GNAT.Sockets.Selector_Status;
   begin
      select
         accept Start (Listener : in Socket_Listener_Type_Access;
                       Address  : in GNAT.Sockets.Sock_Addr_Type) do
            Instance := Listener;
            GNAT.Sockets.Create_Socket (Server);
            GNAT.Sockets.Set_Socket_Option (Server, GNAT.Sockets.Socket_Level,
                                            (GNAT.Sockets.Reuse_Address, True));
            GNAT.Sockets.Bind_Socket (Server, Address);
            GNAT.Sockets.Listen_Socket (Server);
         end Start;
      or
         terminate;
      end select;
      loop
         GNAT.Sockets.Accept_Socket (Server   => Server,
                                     Socket   => Client,
                                     Address  => Peer,
                                     Timeout  => GNAT.Sockets.Forever,
                                     Selector => Instance.Accept_Selector'Access,
                                     Status   => Selector_Status);
         exit when Selector_Status = GNAT.Sockets.Aborted;
         if Selector_Status = GNAT.Sockets.Completed then
            GNAT.Sockets.Close_Socket (Client);
         end if;
      end loop;
      GNAT.Sockets.Close_Socket (Server);
   end Socket_Listener_Task;

   task body Socket_Reader_Task is
      use type GNAT.Sockets.Socket_Type;

      Instance : Socket_Reader_Type_Access;
      Socket   : GNAT.Sockets.Socket_Type;
      Status   : GNAT.Sockets.Selector_Status;
   begin
      select
         accept Start (Reader  : in Socket_Reader_Type_Access;
                       Client  : in GNAT.Sockets.Socket_Type) do
            Instance := Reader;
            Socket := Client;
         end Start;
      or
         terminate;
      end select;
      Instance.Socket.Open (Socket);
      Instance.Read_All;
      GNAT.Sockets.Close_Socket (Socket);

   exception
      when E : others =>
         Log.Error ("Exception", E);
         GNAT.Sockets.Close_Socket (Socket);

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
