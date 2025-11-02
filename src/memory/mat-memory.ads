-----------------------------------------------------------------------
--  Memory - Memory slot
--  Copyright (C) 2014, 2015, 2019, 2023 Stephane Carrez
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
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with ELF;
with MAT.Types;
with MAT.Frames;
with MAT.Events;

package MAT.Memory is

   type Allocation is record
      Size   : MAT.Types.Target_Size;
      Frame  : Frames.Frame_Type;
      Time   : MAT.Types.Target_Tick_Ref;
      Thread : MAT.Types.Target_Thread_Ref;
      Event  : MAT.Events.Event_Id_Type;
   end record;

   --  Statistics about memory allocation.
   type Memory_Info is record
      Total_Size    : MAT.Types.Target_Size := 0;
      Alloc_Count   : Natural := 0;
      Min_Slot_Size : MAT.Types.Target_Size := 0;
      Max_Slot_Size : MAT.Types.Target_Size := 0;
      Min_Addr      : MAT.Types.Target_Addr := 0;
      Max_Addr      : MAT.Types.Target_Addr := 0;
   end record;

   --  Description of a memory region.
   type Region_Info is record
      Start_Addr : MAT.Types.Target_Addr := 0;
      End_Addr   : MAT.Types.Target_Addr := 0;
      Offset     : MAT.Types.Target_Addr := 0;
      Size       : MAT.Types.Target_Size := 0;
      Flags      : ELF.Elf32_Word := 0;
      Path       : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   use type MAT.Types.Target_Addr;
   package Allocation_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Addr,
                                      Element_Type => Allocation);

   subtype Allocation_Map is Allocation_Maps.Map;
   subtype Allocation_Cursor is Allocation_Maps.Cursor;

   --  Define a map of <tt>Memory_Info</tt> keyed by the thread Id.
   --  Such map allows to give the list of threads and a summary of their allocation.
   use type MAT.Types.Target_Thread_Ref;
   package Memory_Info_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Thread_Ref,
                                      Element_Type => Memory_Info);

   subtype Memory_Info_Map is Memory_Info_Maps.Map;
   subtype Memory_Info_Cursor is Memory_Info_Maps.Cursor;

   type Frame_Info is record
      Thread : MAT.Types.Target_Thread_Ref;
      Memory : Memory_Info;
   end record;

   --  Define a map of <tt>Frame_Info</tt> keyed by the backtrace function address
   --  that performed the memory allocation directly or indirectly.
   package Frame_Info_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Addr,
                                      Element_Type => Frame_Info);

   subtype Frame_Info_Map is Frame_Info_Maps.Map;
   subtype Frame_Info_Cursor is Frame_Info_Maps.Cursor;

   package Region_Info_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Addr,
                                      Element_Type => Region_Info);

   subtype Region_Info_Map is Region_Info_Maps.Map;
   subtype Region_Info_Cursor is Region_Info_Maps.Cursor;

end MAT.Memory;
