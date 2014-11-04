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
with Ada.Strings.Unbounded;
with MAT.Types;
with MAT.Events;
package MAT.Readers.Marshaller is

   Buffer_Underflow_Error : exception;

   Buffer_Overflow_Error : exception;

   --  Get an 8-bit value from the buffer.
   function Get_Uint8 (Buffer : in Buffer_Ptr) return MAT.Types.Uint8;

   --  Get a 16-bit value either from big-endian or little endian.
   function Get_Uint16 (Buffer : in Buffer_Ptr) return MAT.Types.Uint16;

   --  Get a 32-bit value either from big-endian or little endian.
   function Get_Uint32 (Buffer : in Buffer_Ptr) return MAT.Types.Uint32;

   --  Get a 64-bit value either from big-endian or little endian.
   function Get_Uint64 (Buffer : in Buffer_Ptr) return MAT.Types.Uint64;

   --  Extract a string from the buffer.  The string starts with a byte that
   --  indicates the string length.
   function Get_String (Buffer : in Buffer_Ptr) return String;

   --  Extract a string from the buffer.  The string starts with a byte that
   --  indicates the string length.
   function Get_String (Msg : in Buffer_Ptr) return Ada.Strings.Unbounded.Unbounded_String;

   generic
      type Target_Type is mod <>;
   function Get_Target_Value (Msg  : in Buffer_Ptr;
                              Kind : in MAT.Events.Attribute_Type) return Target_Type;

   function Get_Target_Size (Msg  : in Buffer_Ptr;
                             Kind : in MAT.Events.Attribute_Type) return MAT.Types.Target_Size;

   function Get_Target_Addr (Msg  : in Buffer_Ptr;
                             Kind : in MAT.Events.Attribute_Type) return MAT.Types.Target_Addr;

   function Get_Target_Uint32 (Msg  : in Buffer_Ptr;
                               Kind : in MAT.Events.Attribute_Type) return MAT.Types.Uint32;

   --  Skip the given number of bytes from the message.
   procedure Skip (Buffer : in Buffer_Ptr;
                   Size   : in Natural);

end MAT.Readers.Marshaller;
