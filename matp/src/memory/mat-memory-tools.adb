-----------------------------------------------------------------------
--  mat-memory-tools - Tools for memory maps
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

package body MAT.Memory.Tools is

   --  ------------------------------
   --  Collect the information about memory slot sizes for the memory slots in the map.
   --  ------------------------------
   procedure Size_Information (Memory : in MAT.Memory.Allocation_Map;
                               Sizes  : in out Size_Info_Map) is
      procedure Update_Count (Size : in MAT.Types.Target_Size;
                              Info : in out Size_Info_Type);
      procedure Collect (Addr : in MAT.Types.Target_Addr;
                         Slot : in Allocation);

      procedure Update_Count (Size : in MAT.Types.Target_Size;
                              Info : in out Size_Info_Type) is
         pragma Unreferenced (Size);
      begin
         Info.Count := Info.Count + 1;
      end Update_Count;

      procedure Collect (Addr : in MAT.Types.Target_Addr;
                         Slot : in Allocation) is
         pragma Unreferenced (Addr);

         Pos : constant Size_Info_Cursor := Sizes.Find (Slot.Size);
      begin
         if Size_Info_Maps.Has_Element (Pos) then
            Sizes.Update_Element (Pos, Update_Count'Access);
         else
            declare
               Info : Size_Info_Type;
            begin
               Info.Count := 1;
               Sizes.Insert (Slot.Size, Info);
            end;
         end if;
      end Collect;

      Iter : Allocation_Cursor := Memory.First;

   begin
      while Allocation_Maps.Has_Element (Iter) loop
         Allocation_Maps.Query_Element (Iter, Collect'Access);
         Allocation_Maps.Next (Iter);
      end loop;
   end Size_Information;

   --  ------------------------------
   --  Collect the information about threads and the memory allocations they've made.
   --  ------------------------------
   procedure Thread_Information (Memory  : in MAT.Memory.Allocation_Map;
                                 Threads : in out Memory_Info_Map) is
      procedure Collect (Addr : in MAT.Types.Target_Addr;
                         Slot : in Allocation);

      procedure Collect (Addr : in MAT.Types.Target_Addr;
                         Slot : in Allocation) is

         procedure Update_Count (Thread : in MAT.Types.Target_Thread_Ref;
                                 Info   : in out Memory_Info);


         procedure Update_Count (Thread : in MAT.Types.Target_Thread_Ref;
                                 Info   : in out Memory_Info) is
            pragma Unreferenced (Thread);

            Size : constant MAT.Types.Target_Size := Slot.Size;
         begin
            Info.Alloc_Count := Info.Alloc_Count + 1;
            if Size < Info.Min_Slot_Size then
               Info.Min_Slot_Size := Size;
            end if;
            if Size > Info.Max_Slot_Size then
               Info.Max_Slot_Size := Size;
            end if;
            if Addr < Info.Min_Addr then
               Info.Min_Addr := Addr;
            end if;
            if Addr + Size > Info.Max_Addr then
               Info.Max_Addr := Addr + Size;
            end if;
            Info.Total_Size := Info.Total_Size + Size;
         end Update_Count;

         Pos : constant Memory_Info_Cursor := Threads.Find (Slot.Thread);
      begin
         if Memory_Info_Maps.Has_Element (Pos) then
            Threads.Update_Element (Pos, Update_Count'Access);
         else
            declare
               Info : Memory_Info;
            begin
               Info.Total_Size := Slot.Size;
               Info.Alloc_Count := 1;
               Info.Min_Slot_Size := Slot.Size;
               Info.Max_Slot_Size := Slot.Size;
               Info.Min_Addr      := Addr;
               Info.Max_Addr      := Addr + Slot.Size;
               Threads.Insert (Slot.Thread, Info);
            end;
         end if;
      end Collect;

      Iter : Allocation_Cursor := Memory.First;
   begin
      while Allocation_Maps.Has_Element (Iter) loop
         Allocation_Maps.Query_Element (Iter, Collect'Access);
         Allocation_Maps.Next (Iter);
      end loop;
   end Thread_Information;

   --  ------------------------------
   --  Collect the information about frames and the memory allocations they've made.
   --  ------------------------------
   procedure Frame_Information (Memory : in MAT.Memory.Allocation_Map;
                                Level  : in Natural;
                                Frames : in out Frame_Info_Map) is
      procedure Collect (Addr : in MAT.Types.Target_Addr;
                         Slot : in Allocation);

      procedure Collect (Addr : in MAT.Types.Target_Addr;
                         Slot : in Allocation) is

         procedure Update_Count (Frame_Addr : in MAT.Types.Target_Addr;
                                 Info       : in out Frame_Info);


         procedure Update_Count (Frame_Addr : in MAT.Types.Target_Addr;
                                 Info       : in out Frame_Info) is
            pragma Unreferenced (Frame_Addr);

            Size : constant MAT.Types.Target_Size := Slot.Size;
         begin
            Info.Memory.Alloc_Count := Info.Memory.Alloc_Count + 1;
            if Size < Info.Memory.Min_Slot_Size then
               Info.Memory.Min_Slot_Size := Size;
            end if;
            if Size > Info.Memory.Max_Slot_Size then
               Info.Memory.Max_Slot_Size := Size;
            end if;
            if Addr < Info.Memory.Min_Addr then
               Info.Memory.Min_Addr := Addr;
            end if;
            if Addr + Size > Info.Memory.Max_Addr then
               Info.Memory.Max_Addr := Addr + Size;
            end if;
            Info.Memory.Total_Size := Info.Memory.Total_Size + Size;
         end Update_Count;

         Frame : constant MAT.Frames.Frame_Table := MAT.Frames.Backtrace (Slot.Frame, Level);
         Pos   : Frame_Info_Cursor;
      begin
         for I in Frame'Range loop
            Pos := Frames.Find (Frame (I));
            if Frame_Info_Maps.Has_Element (Pos) then
               Frames.Update_Element (Pos, Update_Count'Access);
            else
               declare
                  Info : Frame_Info;
               begin
                  Info.Thread               := Slot.Thread;
                  Info.Memory.Total_Size    := Slot.Size;
                  Info.Memory.Alloc_Count   := 1;
                  Info.Memory.Min_Slot_Size := Slot.Size;
                  Info.Memory.Max_Slot_Size := Slot.Size;
                  Info.Memory.Min_Addr      := Addr;
                  Info.Memory.Max_Addr      := Addr + Slot.Size;
                  Frames.Insert (Frame (I), Info);
               end;
            end if;
         end loop;
      end Collect;

      Iter : Allocation_Cursor := Memory.First;
   begin
      while Allocation_Maps.Has_Element (Iter) loop
         Allocation_Maps.Query_Element (Iter, Collect'Access);
         Allocation_Maps.Next (Iter);
      end loop;
   end Frame_Information;

   --  ------------------------------
   --  Find from the <tt>Memory</tt> map the memory slots whose address intersects
   --  the region [From .. To] and which is selected by the given filter expression.
   --  Add the memory slot in the <tt>Into</tt> list if it does not already contains
   --  the memory slot.
   --  ------------------------------
   procedure Find (Memory : in MAT.Memory.Allocation_Map;
                   From   : in MAT.Types.Target_Addr;
                   To     : in MAT.Types.Target_Addr;
                   Filter : in MAT.Expressions.Expression_Type;
                   Into   : in out MAT.Memory.Allocation_Map) is
      procedure Collect (Addr : in MAT.Types.Target_Addr;
                         Slot : in Allocation);

      procedure Collect (Addr : in MAT.Types.Target_Addr;
                         Slot : in Allocation) is
      begin
         if MAT.Expressions.Is_Selected (Filter, Addr, Slot) then
            Into.Insert (Addr, Slot);
         end if;
      end Collect;

      Iter : MAT.Memory.Allocation_Cursor := Memory.Ceiling (From);
      Pos  : MAT.Memory.Allocation_Cursor;
      Addr : MAT.Types.Target_Addr;
      Size : MAT.Types.Target_Size;
   begin
      --  If there was no slot with the ceiling From address, we have to start from the last
      --  node because the memory slot may intersect our region.
      if not Allocation_Maps.Has_Element (Iter) then
         Iter := Memory.Last;
         if not Allocation_Maps.Has_Element (Iter) then
            return;
         end if;
         Addr := Allocation_Maps.Key (Iter);
         Size := Allocation_Maps.Element (Iter).Size;
         if Addr + Size < From then
            return;
         end if;
      end if;

      --  Move backward until the previous memory slot does not overlap anymore our region.
      --  In theory, going backward once is enough but if there was a target malloc issue,
      --  several malloc may overlap the same region (which is bad for the target).
      loop
         Pos := Allocation_Maps.Previous (Iter);
         exit when not Allocation_Maps.Has_Element (Pos);
         Addr := Allocation_Maps.Key (Pos);
         Size := Allocation_Maps.Element (Pos).Size;
         exit when Addr + Size < From;
         Iter := Pos;
      end loop;

      --  Add the memory slots until we moved to the end of the region.
      while Allocation_Maps.Has_Element (Iter) loop
         Addr := Allocation_Maps.Key (Iter);
         exit when Addr > To;
         Pos := Into.Find (Addr);
         if not Allocation_Maps.Has_Element (Pos) then
            Allocation_Maps.Query_Element (Iter, Collect'Access);
         end if;
         Allocation_Maps.Next (Iter);
      end loop;
   end Find;

end MAT.Memory.Tools;
