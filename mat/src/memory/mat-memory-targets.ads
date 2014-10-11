-----------------------------------------------------------------------
--  Memory clients - Client info related to its memory
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
with Ada.Containers.Ordered_Maps;

with Util.Events;

with MAT.Frames;
with MAT.Readers;
with MAT.Memory.Events;
with MAT.Memory.Tools;
package MAT.Memory.Targets is

   type Target_Memory is tagged limited private;
   type Client_Memory_Ref is access all Target_Memory;

   --  Initialize the target memory object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory events.
   procedure Initialize (Memory : in out Target_Memory;
                         Reader : in out MAT.Readers.Manager_Base'Class);

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

private

   protected type Memory_Allocator is

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

      --  Find from the memory map the memory slots whose address intersects
      --  the region [From .. To] and add the memory slot in the <tt>Into</tt> list if
      --  it does not already contains the memory slot.
      procedure Find (From   : in MAT.Types.Target_Addr;
                      To     : in MAT.Types.Target_Addr;
                      Into   : in out MAT.Memory.Allocation_Map);

   private
      Used_Slots    : Allocation_Map;
      Freed_Slots   : Allocation_Map;
      Frames        : MAT.Frames.Frame_Type := MAT.Frames.Create_Root;
   end Memory_Allocator;

   type Target_Memory is tagged limited record
      Reader        : MAT.Readers.Reader_Access;
      Memory        : Memory_Allocator;
   end record;

end MAT.Memory.Targets;
