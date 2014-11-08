-----------------------------------------------------------------------
--  mat-readers-streams -- Reader for streams
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
with Ada.IO_Exceptions;

with Util.Log.Loggers;
with MAT.Readers.Marshaller;
package body MAT.Readers.Streams is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Readers.Files");

   MAX_MSG_SIZE : constant Ada.Streams.Stream_Element_Offset := 2048;

   --  ------------------------------
   --  Read a message from the stream.
   --  ------------------------------
   overriding
   procedure Read_Message (Reader : in out Stream_Reader_Type;
                           Msg    : in out Message) is
      use type Ada.Streams.Stream_Element_Offset;

      Buffer : constant Util.Streams.Buffered.Buffer_Access := Msg.Buffer.Buffer;
      Last   : Ada.Streams.Stream_Element_Offset;
   begin
      Reader.Stream.Read (Buffer (0 .. 1), Last);
      if Last /= 2 then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      Msg.Buffer.Size := 2;
      Msg.Buffer.Current := Msg.Buffer.Start;
      Msg.Size := Natural (MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer));
      if Msg.Size < 2 then
         Log.Error ("Invalid message size {0}", Natural'Image (Msg.Size));
      end if;
      if Ada.Streams.Stream_Element_Offset (Msg.Size) >= Buffer'Last then
         Log.Error ("Message size {0} is too big", Natural'Image (Msg.Size));
         raise Ada.IO_Exceptions.Data_Error;
      end if;
      Reader.Stream.Read (Buffer (0 .. Ada.Streams.Stream_Element_Offset (Msg.Size - 1)), Last);
      Msg.Buffer.Current := Msg.Buffer.Start;
      Msg.Buffer.Last    := Buffer (Last)'Address;
      Msg.Buffer.Size    := Msg.Size;
   end Read_Message;

   --  ------------------------------
   --  Read the events from the stream and stop when the end of the stream is reached.
   --  ------------------------------
   procedure Read_All (Reader : in out Stream_Reader_Type) is
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

end MAT.Readers.Streams;
