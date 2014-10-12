-----------------------------------------------------------------------
--  Ipc -- Ipc channel between profiler tool and application          --
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

with System; use System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with Util.Log.Loggers;
with Interfaces; use Interfaces;
package body MAT.Readers.Marshaller is

   use System.Storage_Elements;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Readers.Marshaller");

   package Uint8_Access is new System.Address_To_Access_Conversions (MAT.Types.Uint8);

   package Uint32_Access is new System.Address_To_Access_Conversions (MAT.Types.Uint32);

   function Get_Raw_Uint32 (Buf : System.Address) return MAT.Types.Uint32 is
      use Uint32_Access;

      P : constant Object_Pointer := To_Pointer (Buf);
   begin
      return P.all;
   end Get_Raw_Uint32;

   function Get_Uint8 (Buffer : in Buffer_Ptr) return MAT.Types.Uint8 is
      use Uint8_Access;

      P : constant Object_Pointer := To_Pointer (Buffer.Current);
   begin
      if Buffer.Size = 0 then
         Log.Error ("Not enough data to get a uint8");
         raise Buffer_Underflow_Error;
      end if;
      Buffer.Size := Buffer.Size - 1;
      Buffer.Current := Buffer.Current + Storage_Offset (1);
      return P.all;
   end Get_Uint8;

   function Get_Uint16 (Buffer : in Buffer_Ptr) return MAT.Types.Uint16 is
      use Uint8_Access;

      High : constant Object_Pointer := To_Pointer (Buffer.Current
                                                    + Storage_Offset (1));
      Low  : constant Object_Pointer := To_Pointer (Buffer.Current);
   begin
      if Buffer.Size <= 1 then
         Log.Error ("Not enough data to get a uint16");
         raise Buffer_Underflow_Error;
      end if;
      Buffer.Size := Buffer.Size - 2;
      Buffer.Current := Buffer.Current + Storage_Offset (2);
      return MAT.Types.Uint16 (High.all) * 256 + MAT.Types.Uint16 (Low.all);
   end Get_Uint16;

   function Get_Uint32 (Buffer : in Buffer_Ptr) return MAT.Types.Uint32 is
      use Uint32_Access;

      P : constant Object_Pointer := To_Pointer (Buffer.Current);
   begin
      if Buffer.Size < 4 then
         Log.Error ("Not enough data to get a uint32");
         raise Buffer_Underflow_Error;
      end if;
      Buffer.Size := Buffer.Size - 4;
      Buffer.Current := Buffer.Current + Storage_Offset (4);
      if Buffer.Current >= Buffer.Last then
         Buffer.Current := Buffer.Start;
      end if;
      return P.all;
   end Get_Uint32;

   function Get_Uint64 (Buffer : in Buffer_Ptr) return MAT.Types.Uint64 is
      Val : constant MAT.Types.Uint64 := MAT.Types.Uint64 (Get_Uint32 (Buffer));
   begin
      return Val + MAT.Types.Uint64 (Get_Uint32 (Buffer)) * 2**32;
   end Get_Uint64;

   function Get_Target_Value (Msg  : in Buffer_Ptr;
                              Kind : in MAT.Events.Attribute_Type) return Target_Type is
   begin
      case Kind is
         when MAT.Events.T_UINT8 =>
            return Target_Type (Get_Uint8 (Msg));

         when MAT.Events.T_UINT16 =>
            return Target_Type (Get_Uint16 (Msg));

         when MAT.Events.T_UINT32 =>
            return Target_Type (Get_Uint32 (Msg));

         when MAT.Events.T_UINT64 =>
            return Target_Type (Get_Uint64 (Msg));

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
   function Get_String (Buffer : in Buffer_Ptr) return String is
      Len    : constant MAT.Types.Uint8 := Get_Uint8 (Buffer);
      Result : String (1 .. Natural (Len));
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Get_Uint8 (Buffer));
      end loop;
      return Result;
   end Get_String;

   --  ------------------------------
   --  Skip the given number of bytes from the message.
   --  ------------------------------
   procedure Skip (Buffer : in Buffer_Ptr;
                   Size   : in Natural) is
   begin
      Buffer.Size := Buffer.Size - Size;
      Buffer.Current := Buffer.Current + Storage_Offset (Size);
   end Skip;

   function Get_Target_Size (Msg  : in Buffer_Ptr;
                             Kind : in MAT.Events.Attribute_Type) return MAT.Types.Target_Size is
         function Get_Value is new Get_Target_Value (MAT.Types.Target_Size);
   begin
      return Get_Value (Msg, Kind);
   end Get_Target_Size;

   function Get_Target_Addr (Msg  : in Buffer_Ptr;
                             Kind : in MAT.Events.Attribute_Type) return MAT.Types.Target_Addr is
         function Get_Value is new Get_Target_Value (MAT.Types.Target_Addr);
   begin
      return Get_Value (Msg, Kind);
   end Get_Target_Addr;

   function Get_Target_Uint32 (Msg  : in Buffer_Ptr;
                               Kind : in MAT.Events.Attribute_Type) return MAT.Types.Uint32 is
      function Get_Value is new Get_Target_Value (MAT.Types.Target_Addr);
   begin
      return Get_Value (Msg, Kind);
   end Get_Target_Uint32;

end MAT.Readers.Marshaller;
