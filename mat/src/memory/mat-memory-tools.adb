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
      Iter : Allocation_Cursor := Memory.First;

      procedure Update_Count (Size : in MAT.Types.Target_Size;
                              Info : in out Size_Info_Type) is
      begin
         Info.Count := Info.Count + 1;
      end Update_Count;

      procedure Collect (Addr : in MAT.Types.Target_Addr;
                         Slot : in Allocation) is
         Pos : Size_Info_Cursor := Sizes.Find (Slot.Size);
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

   begin
      while Allocation_Maps.Has_Element (Iter) loop
         Allocation_Maps.Query_Element (Iter, Collect'Access);
         Allocation_Maps.Next (Iter);
      end loop;
   end Size_Information;

   --  ------------------------------
   --  Find from the <tt>Memory</tt> map the memory slots whose address intersects
   --  the region [From .. To] and add the memory slot in the <tt>Into</tt> list if
   --  it does not already contains the memory slot.
   --  ------------------------------
   procedure Find (Memory : in MAT.Memory.Allocation_Map;
                   From   : in MAT.Types.Target_Addr;
                   To     : in MAT.Types.Target_Addr;
                   Into   : in out MAT.Memory.Allocation_Map) is
      Iter : MAT.Memory.Allocation_Cursor := Memory.Ceiling (From);
      Pos  : MAT.Memory.Allocation_Cursor;
   begin
      while Allocation_Maps.Has_Element (Iter) loop
         declare
            Addr : constant MAT.Types.Target_Addr := Allocation_Maps.Key (Iter);
         begin
            exit when Addr > To;
            Pos := Into.Find (Addr);
            if not Allocation_Maps.Has_Element (Pos) then
               Into.Insert (Addr, Allocation_Maps.Element (Iter));
            end if;
         end;
         Allocation_Maps.Next (Iter);
      end loop;
   end Find;

end MAT.Memory.Tools;
