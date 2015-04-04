-----------------------------------------------------------------------
--  Memory Events - Definition and Analysis of memory events
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
with Util.Log.Loggers;

with MAT.Memory.Probes;
package body MAT.Memory.Targets is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Memory.Targets");

   --  ------------------------------
   --  Initialize the target memory object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory events.
   --  ------------------------------
   procedure Initialize (Memory  : in out Target_Memory;
                         Manager : in out MAT.Events.Probes.Probe_Manager_Type'Class) is
      Memory_Probe : constant MAT.Memory.Probes.Memory_Probe_Type_Access
        := new MAT.Memory.Probes.Memory_Probe_Type;
   begin
      Memory_Probe.Data := Memory'Unrestricted_Access;
      MAT.Memory.Probes.Register (Manager, Memory_Probe);
   end Initialize;

   --  ------------------------------
   --  Add the memory region from the list of memory region managed by the program.
   --  ------------------------------
   procedure Add_Region (Memory : in out Target_Memory;
                         Region : in Region_Info) is
   begin
      Log.Info ("Add region [" & MAT.Types.Hex_Image (Region.Start_Addr) & "-"
                  & MAT.Types.Hex_Image (Region.End_Addr) & "] - {0}", Region.Path);
      Memory.Memory.Add_Region (Region);
   end Add_Region;

   --  ------------------------------
   --  Find the memory region that intersect the given section described by <tt>From</tt>
   --  and <tt>To</tt>.  Each memory region that intersects is added to the <tt>Into</tt>
   --  map.
   --  ------------------------------
   procedure Find (Memory : in out Target_Memory;
                   From   : in MAT.Types.Target_Addr;
                   To     : in MAT.Types.Target_Addr;
                   Into   : in out MAT.Memory.Region_Info_Map) is
   begin
      Memory.Memory.Find (From, To, Into);
   end Find;

   --  ------------------------------
   --  Take into account a malloc probe.  The memory slot [Addr .. Slot.Size] is inserted
   --  in the used slots map.  The freed slots that intersect the malloc'ed region are
   --  removed from the freed map.
   --  ------------------------------
   procedure Probe_Malloc (Memory : in out Target_Memory;
                           Addr   : in MAT.Types.Target_Addr;
                           Slot   : in Allocation) is
   begin
      Memory.Memory.Probe_Malloc (Addr, Slot);
   end Probe_Malloc;

   --  ------------------------------
   --  Take into account a free probe.  Add the memory slot in the freed map and remove
   --  the slot from the used slots map.
   --  ------------------------------
   procedure Probe_Free (Memory : in out Target_Memory;
                         Addr   : in MAT.Types.Target_Addr;
                         Slot   : in Allocation) is
   begin
      Memory.Memory.Probe_Free (Addr, Slot);
   end Probe_Free;

   --  ------------------------------
   --  Take into account a realloc probe.  The old memory slot represented by Old_Addr is
   --  removed from the used slots maps and the new memory slot [Addr .. Slot.Size] is
   --  inserted in the used slots map.
   --  ------------------------------
   procedure Probe_Realloc (Memory : in out Target_Memory;
                            Addr     : in MAT.Types.Target_Addr;
                            Old_Addr : in MAT.Types.Target_Addr;
                            Slot     : in Allocation) is
   begin
      Memory.Memory.Probe_Realloc (Addr, Old_Addr, Slot);
   end Probe_Realloc;

   --  ------------------------------
   --  Insert in the frame tree the new stack frame represented by <tt>Pc</tt>.
   --  If the frame is already known, the frame reference counter is incremented.
   --  The frame represented by <tt>Pc</tt> is returned in <tt>Result</tt>.
   --  ------------------------------
   procedure Create_Frame (Memory : in out Target_Memory;
                           Pc     : in MAT.Frames.Frame_Table;
                           Result : out MAT.Frames.Frame_Type) is
   begin
      Memory.Memory.Create_Frame (Pc, Result);
   end Create_Frame;

   --  ------------------------------
   --  Collect the information about memory slot sizes allocated by the application.
   --  ------------------------------
   procedure Size_Information (Memory : in out Target_Memory;
                               Sizes  : in out MAT.Memory.Tools.Size_Info_Map) is
   begin
      Memory.Memory.Size_Information (Sizes);
   end Size_Information;

   --  ------------------------------
   --  Collect the information about threads and the memory allocations they've made.
   --  ------------------------------
   procedure Thread_Information (Memory  : in out Target_Memory;
                                 Threads : in out Memory_Info_Map) is
   begin
      Memory.Memory.Thread_Information (Threads);
   end Thread_Information;

   --  ------------------------------
   --  Collect the information about frames and the memory allocations they've made.
   --  ------------------------------
   procedure Frame_Information (Memory : in out Target_Memory;
                                Level  : in Natural;
                                Frames : in out Frame_Info_Map) is
   begin
      Memory.Memory.Frame_Information (Level, Frames);
   end Frame_Information;

   --  ------------------------------
   --  Get the global memory and allocation statistics.
   --  ------------------------------
   procedure Stat_Information (Memory : in out Target_Memory;
                               Result : out Memory_Stat) is
   begin
      Memory.Memory.Stat_Information (Result);
   end Stat_Information;

   --  ------------------------------
   --  Find from the <tt>Memory</tt> map the memory slots whose address intersects
   --  the region [From .. To] and which is selected by the given filter expression.
   --  Add the memory slot in the <tt>Into</tt> list if it does not already contains
   --  the memory slot.
   --  ------------------------------
   procedure Find (Memory : in out Target_Memory;
                   From   : in MAT.Types.Target_Addr;
                   To     : in MAT.Types.Target_Addr;
                   Filter : in MAT.Expressions.Expression_Type;
                   Into   : in out MAT.Memory.Allocation_Map) is
   begin
      Memory.Memory.Find (From, To, Filter, Into);
   end Find;

   protected body Memory_Allocator is

      --  ------------------------------
      --  Add the memory region from the list of memory region managed by the program.
      --  ------------------------------
      procedure Add_Region (Region : in Region_Info) is
      begin
         Regions.Insert (Region.Start_Addr, Region);
      end Add_Region;

      --  ------------------------------
      --  Find the memory region that intersect the given section described by <tt>From</tt>
      --  and <tt>To</tt>.  Each memory region that intersects is added to the <tt>Into</tt>
      --  map.
      --  ------------------------------
      procedure Find (From : in MAT.Types.Target_Addr;
                      To   : in MAT.Types.Target_Addr;
                      Into : in out MAT.Memory.Region_Info_Map) is
         Iter : Region_Info_Cursor;
      begin
         Iter := Regions.Ceiling (From);
         while Region_Info_Maps.Has_Element (Iter) loop
            declare
               Start  : constant MAT.Types.Target_Addr := Region_Info_Maps.Key (Iter);
               Region : Region_Info;
            begin
               exit when Start > To;

               Region := Region_Info_Maps.Element (Iter);
               if Region.End_Addr >= From then
                  if not Into.Contains (Start) then
                     Into.Insert (Start, Region);
                  end if;
               end if;
               Region_Info_Maps.Next (Iter);
            end;
         end loop;
      end Find;

      --  ------------------------------
      --  Remove the memory region [Addr .. Addr + Size] from the free list.
      --  ------------------------------
      procedure Remove_Free (Addr : in MAT.Types.Target_Addr;
                             Size : in MAT.Types.Target_Size) is
         Iter : Allocation_Cursor;
         Last : constant MAT.Types.Target_Addr := Addr + MAT.Types.Target_Addr (Size);
         Slot : Allocation;
      begin
         --  Walk the list of free blocks and remove all the blocks which intersect the region
         --  [Addr .. Addr + Slot.Size].  We start walking at the first block below and near
         --  the address.  Slots are then removed when they intersect the malloc'ed region.
         Iter := Freed_Slots.Floor (Addr);
         while Allocation_Maps.Has_Element (Iter) loop
            declare
               Freed_Addr : constant MAT.Types.Target_Addr := Allocation_Maps.Key (Iter);
            begin
               exit when Freed_Addr > Last;
               Slot := Allocation_Maps.Element (Iter);
               if Freed_Addr + MAT.Types.Target_Addr (Slot.Size) > Addr then
                  if Stats.Total_Free > Slot.Size then
                     Stats.Total_Free := Stats.Total_Free - Slot.Size;
                  else
                     Stats.Total_Free := 0;
                  end if;
                  Freed_Slots.Delete (Iter);
                  Iter := Freed_Slots.Floor (Addr);
               else
                  Allocation_Maps.Next (Iter);
               end if;
            end;
         end loop;
      end Remove_Free;

      --  ------------------------------
      --  Take into account a malloc probe.  The memory slot [Addr .. Slot.Size] is inserted
      --  in the used slots map.  The freed slots that intersect the malloc'ed region are
      --  removed from the freed map.
      --  ------------------------------
      procedure Probe_Malloc (Addr   : in MAT.Types.Target_Addr;
                              Slot   : in Allocation) is
      begin
         if Log.Get_Level = Util.Log.DEBUG_LEVEL then
            Log.Debug ("Malloc at {0} size {1}", MAT.Types.Hex_Image (Addr),
                       MAT.Types.Target_Size'Image (Slot.Size));
         end if;
         Stats.Malloc_Count := Stats.Malloc_Count + 1;
         if Addr /= 0 then
            Stats.Total_Alloc := Stats.Total_Alloc + Slot.Size;
            Remove_Free (Addr, Slot.Size);
            Used_Slots.Insert (Addr, Slot);
         end if;
      end Probe_Malloc;

      --  ------------------------------
      --  Take into account a free probe.  Add the memory slot in the freed map and remove
      --  the slot from the used slots map.
      --  ------------------------------
      procedure Probe_Free (Addr   : in MAT.Types.Target_Addr;
                            Slot   : in Allocation) is
         Item : Allocation;
         Iter : Allocation_Cursor;
      begin
         if Log.Get_Level = Util.Log.DEBUG_LEVEL then
            Log.Debug ("Free {0}", MAT.Types.Hex_Image (Addr));
         end if;
         Stats.Free_Count := Stats.Free_Count + 1;
         Iter := Used_Slots.Find (Addr);
         if Allocation_Maps.Has_Element (Iter) then
            Item := Allocation_Maps.Element (Iter);
            if Stats.Total_Alloc >= Item.Size then
               Stats.Total_Alloc := Stats.Total_Alloc - Item.Size;
            else
               Stats.Total_Alloc := 0;
            end if;
            Stats.Total_Free := Stats.Total_Free + Item.Size;
            MAT.Frames.Release (Item.Frame);
            Used_Slots.Delete (Iter);
            Item.Frame := Slot.Frame;
            Iter := Freed_Slots.Find (Addr);
            if not Allocation_Maps.Has_Element (Iter) then
               Freed_Slots.Insert (Addr, Item);
            end if;
         end if;

      exception
         when others =>
            Log.Error ("Free {0} raised some exception", MAT.Types.Hex_Image (Addr));
            raise;
      end Probe_Free;

      --  ------------------------------
      --  Take into account a realloc probe.  The old memory slot represented by Old_Addr is
      --  removed from the used slots maps and the new memory slot [Addr .. Slot.Size] is
      --  inserted in the used slots map.
      --  ------------------------------
      procedure Probe_Realloc (Addr     : in MAT.Types.Target_Addr;
                               Old_Addr : in MAT.Types.Target_Addr;
                               Slot     : in Allocation) is
         procedure Update_Size (Key     : in MAT.Types.Target_Addr;
                                Element : in out Allocation);

         procedure Update_Size (Key     : in MAT.Types.Target_Addr;
                                Element : in out Allocation) is
            pragma Unreferenced (Key);
         begin
            if Stats.Total_Alloc >= Element.Size then
               Stats.Total_Alloc := Stats.Total_Alloc - Element.Size;
            else
               Stats.Total_Alloc := 0;
            end if;
            Element.Size := Slot.Size;
            MAT.Frames.Release (Element.Frame);
            Element.Frame := Slot.Frame;
         end Update_Size;

         Pos      : Allocation_Cursor;
      begin
         if Log.Get_Level = Util.Log.DEBUG_LEVEL then
            Log.Debug ("Realloc {0} to {1} size {2}", MAT.Types.Hex_Image (Old_Addr),
                       MAT.Types.Hex_Image (Addr), MAT.Types.Target_Size'Image (Slot.Size));
         end if;
         Stats.Realloc_Count := Stats.Realloc_Count + 1;
         if Addr /= 0 then
            Stats.Total_Alloc := Stats.Total_Alloc + Slot.Size;
            Pos := Used_Slots.Find (Old_Addr);
            if Allocation_Maps.Has_Element (Pos) then
               if Addr = Old_Addr then
                  Used_Slots.Update_Element (Pos, Update_Size'Access);
               else
                  Used_Slots.Delete (Pos);
                  Used_Slots.Insert (Addr, Slot);
               end if;
            else
               Used_Slots.Insert (Addr, Slot);
            end if;
            Remove_Free (Addr, Slot.Size);
         end if;
      end Probe_Realloc;

      --  ------------------------------
      --  Insert in the frame tree the new stack frame represented by <tt>Pc</tt>.
      --  If the frame is already known, the frame reference counter is incremented.
      --  The frame represented by <tt>Pc</tt> is returned in <tt>Result</tt>.
      --  ------------------------------
      procedure Create_Frame (Pc     : in MAT.Frames.Frame_Table;
                              Result : out MAT.Frames.Frame_Type) is
      begin
         MAT.Frames.Insert (Frames, Pc, Result);
      end Create_Frame;

      --  ------------------------------
      --  Collect the information about memory slot sizes allocated by the application.
      --  ------------------------------
      procedure Size_Information (Sizes  : in out MAT.Memory.Tools.Size_Info_Map) is
      begin
         MAT.Memory.Tools.Size_Information (Used_Slots, Sizes);
      end Size_Information;

      --  ------------------------------
      --  Collect the information about threads and the memory allocations they've made.
      --  ------------------------------
      procedure Thread_Information (Threads : in out Memory_Info_Map) is
      begin
         MAT.Memory.Tools.Thread_Information (Used_Slots, Threads);
      end Thread_Information;

      --  ------------------------------
      --  Collect the information about frames and the memory allocations they've made.
      --  ------------------------------
      procedure Frame_Information (Level  : in Natural;
                                   Frames : in out Frame_Info_Map) is
      begin
         MAT.Memory.Tools.Frame_Information (Used_Slots, Level, Frames);
      end Frame_Information;

      --  ------------------------------
      --  Find from the <tt>Memory</tt> map the memory slots whose address intersects
      --  the region [From .. To] and which is selected by the given filter expression.
      --  Add the memory slot in the <tt>Into</tt> list if it does not already contains
      --  the memory slot.
      --  ------------------------------
      procedure Find (From   : in MAT.Types.Target_Addr;
                      To     : in MAT.Types.Target_Addr;
                      Filter : in MAT.Expressions.Expression_Type;
                      Into   : in out MAT.Memory.Allocation_Map) is
      begin
         MAT.Memory.Tools.Find (Used_Slots, From, To, Filter, Into);
      end Find;

      --  ------------------------------
      --  Get the global memory and allocation statistics.
      --  ------------------------------
      procedure Stat_Information (Result : out Memory_Stat) is
      begin
         Result := Stats;
      end Stat_Information;

   end Memory_Allocator;

end MAT.Memory.Targets;
