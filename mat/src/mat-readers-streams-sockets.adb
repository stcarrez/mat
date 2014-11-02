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

   procedure Read_All (Reader : in out Socket_Reader_Type) is
      use Ada.Streams;
      use type MAT.Types.Uint8;

      Data    : aliased Ada.Streams.Stream_Element_Array (0 .. MAX_MSG_SIZE);
      Buffer  : aliased Buffer_Type;
      Msg     : Message;
      Last    : Ada.Streams.Stream_Element_Offset;
      Format  : MAT.Types.Uint8;
   begin
      Msg.Buffer := Buffer'Unchecked_Access;
      Msg.Buffer.Start := Data (0)'Address;
      Msg.Buffer.Current := Msg.Buffer.Start;
      Msg.Buffer.Last := Data (MAX_MSG_SIZE)'Address;
      Msg.Buffer.Size := 3;
      Reader.Stream.Read (Data (0 .. 2), Last);
      Format := MAT.Readers.Marshaller.Get_Uint8 (Msg.Buffer);
      if Format = 0 then
         Msg.Buffer.Endian := LITTLE_ENDIAN;
         Log.Debug ("Data stream is little endian");
      else
         Msg.Buffer.Endian := BIG_ENDIAN;
         Log.Debug ("Data stream is big endian");
      end if;
      Msg.Size := Natural (MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer));
      if Msg.Size < 2 then
         Log.Error ("Invalid message size {0}", Natural'Image (Msg.Size));
      end if;
      Reader.Stream.Read (Data (0 .. Ada.Streams.Stream_Element_Offset (Msg.Size - 1)), Last);
      Msg.Buffer.Current := Msg.Buffer.Start;
      Msg.Buffer.Last    := Data (Last)'Address;
      Msg.Buffer.Size    := Msg.Size;
      Reader.Read_Headers (Msg);
      while not Reader.Stream.Is_Eof loop
         Reader.Stream.Read (Data (0 .. 1), Last);
         exit when Last /= 2;
         Msg.Buffer.Size := 2;
         Msg.Buffer.Current := Msg.Buffer.Start;
         Msg.Size := Natural (MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer));
         if Msg.Size < 2 then
            Log.Error ("Invalid message size {0}", Natural'Image (Msg.Size));
         end if;
         if Ada.Streams.Stream_Element_Offset (Msg.Size) >= Data'Last then
            Log.Error ("Message size {0} is too big", Natural'Image (Msg.Size));
            exit;
         end if;
         Reader.Stream.Read (Data (0 .. Ada.Streams.Stream_Element_Offset (Msg.Size - 1)), Last);
         Msg.Buffer.Current := Msg.Buffer.Start;
         Msg.Buffer.Last    := Data (Last)'Address;
         Msg.Buffer.Size    := Msg.Size;
         Reader.Dispatch_Message (Msg);
      end loop;
   end Read_All;

end MAT.Readers.Streams.Sockets;
