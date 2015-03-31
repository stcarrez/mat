-----------------------------------------------------------------------
--  Memory clients - Client info related to its memory
--  Copyright (C) 2014, 2015 Stephane Carrez
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

with MAT.Frames;
with MAT.Events.Probes;
with MAT.Memory.Tools;
with MAT.Expressions;
package MAT.Memory.Targets is

   --  Define some global statistics about the memory slots.
   type Memory_Stat is record
      Thread_Count  : Natural := 0;
      Total_Alloc   : MAT.Types.Target_Size := 0;
      Total_Free    : MAT.Types.Target_Size := 0;
      Malloc_Count  : Natural := 0;
      Free_Count    : Natural := 0;
      Realloc_Count : Natural := 0;
   end record;

   type Target_Memory is tagged limited private;
   type Client_Memory_Ref is access all Target_Memory;

   --  Initialize the target memory object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory events.
   procedure Initialize (Memory  : in out Target_Memory;
                         Manager : in out MAT.Events.Probes.Probe_Manager_Type'Class);

   --  Add the memory region from the list of memory region managed by the program.
   procedure Add_Region (Memory : in out Target_Memory;
                         Region : in Region_Info);

   --  Find the memory region that intersect the given section described by <tt>From</tt>
   --  and <tt>To</tt>.  Each memory region that intersects is added to the <tt>Into</tt>
   --  map.
   procedure Find (Memory : in out Target_Memory;
                   From   : in MAT.Types.Target_Addr;
                   To     : in MAT.Types.Target_Addr;
                   Into   : in out MAT.Memory.Region_Info_Map);

   --  Take into account a malloc probe.  The memory slot [Addr .. Slot.Size] is inserted
   --  in the used slots map.  The freed slots that intersect the malloc'ed region are
   --  removed from the freed map.
   procedure Probe_Malloc (Memory : in out Target_Memory;
                           Addr   : in MAT.Types.Target_Addr;
                           Slot   : in Allocation);

   --  Take into account a free probe.  Add the memory slot in the freed map and remove
   --  the slot from the used slots map.
   procedure Probe_Free (Memory : in out Target_Memory;
                         Addr   : in MAT.Types.Target_Addr;
                         Slot   : in Allocation);

   --  Take into account a realloc probe.  The old memory slot represented by Old_Addr is
   --  removed from the used slots maps and the new memory slot [Addr .. Slot.Size] is
   --  inserted in the used slots map.
   procedure Probe_Realloc (Memory   : in out Target_Memory;
                            Addr     : in MAT.Types.Target_Addr;
                            Old_Addr : in MAT.Types.Target_Addr;
                            Slot     : in Allocation);

   --  Insert in the frame tree the new stack frame represented by <tt>Pc</tt>.
   --  If the frame is already known, the frame reference counter is incremented.
   --  The frame represented by <tt>Pc</tt> is returned in <tt>Result</tt>.
   procedure Create_Frame (Memory : in out Target_Memory;
                           Pc     : in MAT.Frames.Frame_Table;
                           Result : out MAT.Frames.Frame_Type);

   --  Collect the information about memory slot sizes allocated by the application.
   procedure Size_Information (Memory : in out Target_Memory;
                               Sizes  : in out MAT.Memory.Tools.Size_Info_Map);

   --  Collect the information about threads and the memory allocations they've made.
   procedure Thread_Information (Memory  : in out Target_Memory;
                                 Threads : in out Memory_Info_Map);

   --  Collect the information about frames and the memory allocations they've made.
   procedure Frame_Information (Memory : in out Target_Memory;
                                Level  : in Natural;
                                Frames : in out Frame_Info_Map);

   --  Get the global memory and allocation statistics.
   procedure Stat_Information (Memory : in out Target_Memory;
                               Result : out Memory_Stat);

   --  Find from the <tt>Memory</tt> map the memory slots whose address intersects
   --  the region [From .. To] and which is selected by the given filter expression.
   --  Add the memory slot in the <tt>Into</tt> list if it does not already contains
   --  the memory slot.
   procedure Find (Memory : in out Target_Memory;
                   From   : in MAT.Types.Target_Addr;
                   To     : in MAT.Types.Target_Addr;
                   Filter : in MAT.Expressions.Expression_Type;
                   Into   : in out MAT.Memory.Allocation_Map);

private

   protected type Memory_Allocator is

      --  Add the memory region from the list of memory region managed by the program.
      procedure Add_Region (Region : in Region_Info);

      --  Find the memory region that intersect the given section described by <tt>From</tt>
      --  and <tt>To</tt>.  Each memory region that intersects is added to the <tt>Into</tt>
      --  map.
      procedure Find (From : in MAT.Types.Target_Addr;
                      To   : in MAT.Types.Target_Addr;
                      Into : in out MAT.Memory.Region_Info_Map);

      --  Take into account a malloc probe.  The memory slot [Addr .. Slot.Size] is inserted
      --  in the used slots map.  The freed slots that intersect the malloc'ed region are
      --  removed from the freed map.
      procedure Probe_Malloc (Addr   : in MAT.Types.Target_Addr;
                              Slot   : in Allocation);

      --  Take into account a free probe.  Add the memory slot in the freed map and remove
      --  the slot from the used slots map.
      procedure Probe_Free (Addr   : in MAT.Types.Target_Addr;
                            Slot   : in Allocation);

      --  Take into account a realloc probe.  The old memory slot represented by Old_Addr is
      --  removed from the used slots maps and the new memory slot [Addr .. Slot.Size] is
      --  inserted in the used slots map.
      procedure Probe_Realloc (Addr     : in MAT.Types.Target_Addr;
                               Old_Addr : in MAT.Types.Target_Addr;
                               Slot     : in Allocation);

      --  Insert in the frame tree the new stack frame represented by <tt>Pc</tt>.
      --  If the frame is already known, the frame reference counter is incremented.
      --  The frame represented by <tt>Pc</tt> is returned in <tt>Result</tt>.
      procedure Create_Frame (Pc     : in MAT.Frames.Frame_Table;
                              Result : out MAT.Frames.Frame_Type);

      --  Collect the information about memory slot sizes allocated by the application.
      procedure Size_Information (Sizes  : in out MAT.Memory.Tools.Size_Info_Map);

      --  Collect the information about threads and the memory allocations they've made.
      procedure Thread_Information (Threads : in out Memory_Info_Map);

      --  Collect the information about frames and the memory allocations they've made.
      procedure Frame_Information (Level  : in Natural;
                                   Frames : in out Frame_Info_Map);

      --  Find from the <tt>Memory</tt> map the memory slots whose address intersects
      --  the region [From .. To] and which is selected by the given filter expression.
      --  Add the memory slot in the <tt>Into</tt> list if it does not already contains
      --  the memory slot.
      procedure Find (From   : in MAT.Types.Target_Addr;
                      To     : in MAT.Types.Target_Addr;
                      Filter : in MAT.Expressions.Expression_Type;
                      Into   : in out MAT.Memory.Allocation_Map);

      --  Get the global memory and allocation statistics.
      procedure Stat_Information (Result : out Memory_Stat);

   private
      Used_Slots    : Allocation_Map;
      Freed_Slots   : Allocation_Map;
      Regions       : Region_Info_Map;
      Stats         : Memory_Stat;
      Frames        : MAT.Frames.Frame_Type := MAT.Frames.Create_Root;
   end Memory_Allocator;

   type Target_Memory is tagged limited record
      Manager       : MAT.Events.Probes.Probe_Manager_Type_Access;
      Memory        : Memory_Allocator;
   end record;

end MAT.Memory.Targets;
