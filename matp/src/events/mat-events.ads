-----------------------------------------------------------------------
--  gprofiler-events - Profiler Events Description
--  Copyright (C) 2014, 2015, 2021 Stephane Carrez
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
with MAT.Frames;
with Util.Strings;
package MAT.Events is

   use Interfaces;

   type Event_Id_Type is new Natural;

   type Probe_Index_Type is (MSG_BEGIN,
                             MSG_END,
                             MSG_LIBRARY,
                             MSG_MALLOC,
                             MSG_FREE,
                             MSG_REALLOC
                            );

   type Target_Event_Type is record
      Id       : Event_Id_Type;
      Next_Id  : Event_Id_Type := 0;
      Prev_Id  : Event_Id_Type := 0;
      Event    : MAT.Types.Uint16;
      Index    : Probe_Index_Type;
      Time     : MAT.Types.Target_Tick_Ref;
      Thread   : MAT.Types.Target_Thread_Ref;
      Frame    : MAT.Frames.Frame_Type;
      Addr     : MAT.Types.Target_Addr;
      Size     : MAT.Types.Target_Size;
      Old_Addr : MAT.Types.Target_Addr;
      Old_Size : MAT.Types.Target_Size;
   end record;

--     subtype Target_Event is Probe_Event_Type;

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

   type Event_Type is (EVENT_BEGIN, EVENT_END, EVENT_MALLOC, EVENT_FREE, EVENT_REALLOC);

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
   type Event_Description_Access is access all Event_Description;

--     subtype Addr is MAT.Types.Uint32;
   subtype Frame_Table is MAT.Frames.Frame_Table;

   type Rusage_Info is record
      Minflt : Unsigned_32;
      Majflt : Unsigned_32;
      Nswap  : Unsigned_32;
   end record;

   type Frame_Info (Depth : Natural) is record
      Time      : MAT.Types.Target_Time;
      Thread    : MAT.Types.Target_Thread_Ref;
      Stack     : MAT.Types.Target_Addr;
      Rusage    : Rusage_Info;
      Cur_Depth : Natural;
      Frame     : Frame_Table (1 .. Depth);
   end record;
   type Frame_Info_Access is access all Frame_Info;

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
