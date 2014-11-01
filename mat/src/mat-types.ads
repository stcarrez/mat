-----------------------------------------------------------------------
--  mat-types -- Global types
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
package MAT.Types is

   type String_Ptr is access all String;

   subtype Uint8 is Interfaces.Unsigned_8;

   subtype Uint16 is Interfaces.Unsigned_16;

   subtype Uint32 is Interfaces.Unsigned_32;

   subtype Uint64 is Interfaces.Unsigned_64;

   subtype Target_Addr is Interfaces.Unsigned_32;

   subtype Target_Size is Interfaces.Unsigned_32;

   subtype Target_Offset is Interfaces.Unsigned_32;

   subtype Target_Tick_Ref is Uint64;

   subtype Target_Thread_Ref is Uint32;

   subtype Target_Process_Ref is Uint32;

   subtype Target_Time is Uint64;

   --  Return an hexadecimal string representation of the value.
   function Hex_Image (Value : in Uint32;
                       Length : in Positive := 8) return String;

end MAT.Types;
