-----------------------------------------------------------------------
--  gprofiler-events - Profiler Events Description
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
with Ada.Strings.Unbounded;
with MAT.Types;
with Util.Strings;
package MAT.Events is

   use Interfaces;

   type Attribute_Type is (T_UINT8,
                           T_UINT16,
                           T_UINT32,
                           T_UINT64,
                           T_POINTER,
                           T_PROBE,
                           T_FRAME,
                           T_THREAD,
                           T_TIME,
                           T_SIZE_T);

   type Event_Type is new Interfaces.Unsigned_32;

   subtype Internal_Reference is Natural;

   type Attribute is record
      Name : Util.Strings.Name_Access;
      Size : Natural            := 0;
      Kind : Attribute_Type     := T_UINT8;
      Ref  : Internal_Reference := 0;
   end record;
   --  Logical description of an event attribute.

   type Attribute_Table is array (Natural range <>) of Attribute;
   type Attribute_Table_Ptr is access all Attribute_Table;
   type Const_Attribute_Table_Access is access constant Attribute_Table;

   type Event_Description (Nb_Attributes : Natural) is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Id   : Unsigned_32;
      Kind : Event_Type;
      Def  : Attribute_Table (1 .. Nb_Attributes);
   end record;
   type Event_Description_Ptr is access all Event_Description;
   subtype Addr is MAT.Types.Uint32;
   type Frame_Table is array (Natural range <>) of Addr;

   type Rusage_Info is record
      Minflt : Unsigned_32;
      Majflt : Unsigned_32;
      Nswap  : Unsigned_32;
   end record;

   type Frame_Info (Depth : Natural) is record
      Time   : Unsigned_64;
      Thid   : Unsigned_32;
      Thstk  : Unsigned_32;
      Rusage : Rusage_Info;
      Cur_Depth : Unsigned_32;
      Frame  : Frame_Table (1 .. Depth);
   end record;

   type Event_Data is record
      Kind  : Attribute_Type;
      U8    : MAT.Types.Uint8;
      U16   : MAT.Types.Uint16;
      U32   : MAT.Types.Uint32;
      U64   : MAT.Types.Uint64;
      Probe : Frame_Info (Depth => 10);
   end record;

   type Event_Data_Table is array (Natural range <>) of Event_Data;

   procedure Dump (Table : in Event_Data_Table;
                   Def   : in Event_Description);

end MAT.Events;
