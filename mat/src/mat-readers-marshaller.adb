-----------------------------------------------------------------------
--  mat-readers-marshaller -- Marshalling of data in communication buffer
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
with Interfaces;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with Util.Log.Loggers;
package body MAT.Readers.Marshaller is

   use type System.Storage_Elements.Storage_Offset;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Readers.Marshaller");

   package Uint8_Access is new System.Address_To_Access_Conversions (MAT.Types.Uint8);

   package Uint32_Access is new System.Address_To_Access_Conversions (MAT.Types.Uint32);

   --  ------------------------------
   --  Get an 8-bit value from the buffer.
   --  ------------------------------
   function Get_Uint8 (Buffer : in Buffer_Ptr) return MAT.Types.Uint8 is
      use Uint8_Access;

      P : constant Object_Pointer := To_Pointer (Buffer.Current);
   begin
      if Buffer.Size = 0 then
         Log.Error ("Not enough data to get a uint8");
         raise Buffer_Underflow_Error;
      end if;
      Buffer.Size := Buffer.Size - 1;
      Buffer.Current := Buffer.Current + System.Storage_Elements.Storage_Offset (1);
      return P.all;
   end Get_Uint8;

   --  ------------------------------
   --  Get a 16-bit value either from big-endian or little endian.
   --  ------------------------------
   function Get_Uint16 (Buffer : in Buffer_Ptr) return MAT.Types.Uint16 is
      use Uint8_Access;
      use type Interfaces.Unsigned_16;

      High : Object_Pointer;
      Low  : Object_Pointer;
   begin
      if Buffer.Size <= 1 then
         Log.Error ("Not enough data to get a uint16");
         raise Buffer_Underflow_Error;
      end if;
      if Buffer.Endian = LITTLE_ENDIAN then
         Low  := To_Pointer (Buffer.Current);
         High := To_Pointer (Buffer.Current + System.Storage_Elements.Storage_Offset (1));
      else
         High := To_Pointer (Buffer.Current);
         Low  := To_Pointer (Buffer.Current + System.Storage_Elements.Storage_Offset (1));
      end if;
      Buffer.Size := Buffer.Size - 2;
      Buffer.Current := Buffer.Current + System.Storage_Elements.Storage_Offset (2);
      return MAT.Types.Uint16 (High.all) * 256 + MAT.Types.Uint16 (Low.all);
   end Get_Uint16;

   --  ------------------------------
   --  Get a 32-bit value either from big-endian or little endian.
   --  ------------------------------
   function Get_Uint32 (Buffer : in Buffer_Ptr) return MAT.Types.Uint32 is
      use Uint32_Access;

      Low, High : MAT.Types.Uint16;
   begin
      if Buffer.Size < 4 then
         Log.Error ("Not enough data to get a uint32");
         raise Buffer_Underflow_Error;
      end if;
      if Buffer.Endian = LITTLE_ENDIAN then
         Low  := Get_Uint16 (Buffer);
         High := Get_Uint16 (Buffer);
      else
         High := Get_Uint16 (Buffer);
         Low  := Get_Uint16 (Buffer);
      end if;
      return Interfaces.Shift_Left (MAT.Types.Uint32 (High), 16) + MAT.Types.Uint32 (Low);
   end Get_Uint32;

   --  ------------------------------
   --  Get a 64-bit value either from big-endian or little endian.
   --  ------------------------------
   function Get_Uint64 (Buffer : in Buffer_Ptr) return MAT.Types.Uint64 is
      Low : MAT.Types.Uint32;
      High : MAT.Types.Uint32;
   begin
      if Buffer.Endian = LITTLE_ENDIAN then
         Low := Get_Uint32 (Buffer);
         High := Get_Uint32 (Buffer);
      else
         High := Get_Uint32 (Buffer);
         Low  := Get_Uint32 (Buffer);
      end if;
      return Interfaces.Shift_Left (MAT.Types.Uint64 (High), 32) + MAT.Types.Uint64 (Low);
   end Get_Uint64;

   function Get_Target_Value (Msg  : in Message_Type;
                              Kind : in MAT.Events.Attribute_Type) return Target_Type is
   begin
      case Kind is
         when MAT.Events.T_UINT8 =>
            return Target_Type (Get_Uint8 (Msg.Buffer));

         when MAT.Events.T_UINT16 =>
            return Target_Type (Get_Uint16 (Msg.Buffer));

         when MAT.Events.T_UINT32 =>
            return Target_Type (Get_Uint32 (Msg.Buffer));

         when MAT.Events.T_UINT64 =>
            return Target_Type (Get_Uint64 (Msg.Buffer));

         when others =>
            Log.Error ("Invalid attribute type {0}",
                       MAT.Events.Attribute_Type'Image (Kind));
            return 0;
      end case;
   end Get_Target_Value;

   --  ------------------------------
   --  Extract a string from the buffer.  The string starts with a byte that
   --  indicates the string length.
   --  ------------------------------
   function Get_String (Buffer : in Message_Type) return String is
      Len    : constant MAT.Types.Uint16 := Get_Uint16 (Buffer.Buffer);
      Result : String (1 .. Natural (Len));
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Get_Uint8 (Buffer.Buffer));
      end loop;
      return Result;
   end Get_String;

   --  ------------------------------
   --  Extract a string from the buffer.  The string starts with a byte that
   --  indicates the string length.
   --  ------------------------------
   function Get_String (Msg : in Message_Type) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String (Get_String (Msg));
   end Get_String;

   --  ------------------------------
   --  Skip the given number of bytes from the message.
   --  ------------------------------
   procedure Skip (Buffer : in Message_Type;
                   Size   : in Natural) is
   begin
      Buffer.Buffer.Size := Buffer.Buffer.Size - Size;
      Buffer.Buffer.Current := Buffer.Buffer.Current + System.Storage_Elements.Storage_Offset (Size);
   end Skip;

   function Get_Target_Size (Msg  : in Message_Type;
                             Kind : in MAT.Events.Attribute_Type) return MAT.Types.Target_Size is
         function Get_Value is new Get_Target_Value (MAT.Types.Target_Size);
   begin
      return Get_Value (Msg, Kind);
   end Get_Target_Size;

   function Get_Target_Addr (Msg  : in Message_Type;
                             Kind : in MAT.Events.Attribute_Type) return MAT.Types.Target_Addr is
         function Get_Value is new Get_Target_Value (MAT.Types.Target_Addr);
   begin
      return Get_Value (Msg, Kind);
   end Get_Target_Addr;

   function Get_Target_Uint32 (Msg  : in Message_Type;
                               Kind : in MAT.Events.Attribute_Type) return MAT.Types.Uint32 is
      function Get_Value is new Get_Target_Value (MAT.Types.Target_Addr);
   begin
      return Get_Value (Msg, Kind);
   end Get_Target_Uint32;

end MAT.Readers.Marshaller;
