-----------------------------------------------------------------------
--  Memory Events - Definition and Analysis of memory events
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
with Util.Log.Loggers;

with MAT.Memory.Readers;
package body MAT.Memory.Targets is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Memory.Targets");

   --  ------------------------------
   --  Initialize the target memory object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory events.
   --  ------------------------------
   procedure Initialize (Memory : in out Target_Memory;
                         Reader : in out MAT.Readers.Manager_Base'Class) is
      Memory_Reader : constant MAT.Memory.Readers.Memory_Reader_Access
        := new MAT.Memory.Readers.Memory_Servant;
   begin
      Memory.Reader := Memory_Reader.all'Access;
      Memory_Reader.Data := Memory'Unrestricted_Access;
      Memory.Frames := Frames.Create_Root;
      MAT.Memory.Readers.Register (Reader, Memory_Reader);
   end Initialize;

   --  ------------------------------
   --  Collect the information about memory slot sizes allocated by the application.
   --  ------------------------------
   procedure Size_Information (Memory : in Target_Memory;
                               Sizes  : in out Size_Info_Map) is
      Iter : Allocation_Cursor := Memory.Memory_Slots.First;

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

   protected body Memory_Allocator is

      --  ------------------------------
      --  Take into account a malloc probe.  The memory slot [Addr .. Slot.Size] is inserted
      --  in the used slots map.  The freed slots that intersect the malloc'ed region are
      --  removed from the freed map.
      --  ------------------------------
      procedure Probe_Malloc (Addr   : in MAT.Types.Target_Addr;
                              Slot   : in Allocation) is
         Iter : Allocation_Cursor;
         Last : constant MAT.Types.Target_Addr := Addr + MAT.Types.Target_Addr (Slot.Size);
         Item : Allocation;
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
               Item := Allocation_Maps.Element (Iter);
               if Freed_Addr + MAT.Types.Target_Addr (Item.Size) > Addr then
                  Freed_Slots.Delete (Iter);
                  Iter := Freed_Slots.Floor (Addr);
               else
                  Allocation_Maps.Next (Iter);
               end if;
            end;
         end loop;
         Used_Slots.Insert (Addr, Slot);
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
         Iter := Used_Slots.Find (Addr);
         if Allocation_Maps.Has_Element (Iter) then
            Item := Allocation_Maps.Element (Iter);
            MAT.Frames.Release (Item.Frame);
            Used_Slots.Delete (Iter);
            Item.Frame := Slot.Frame;
            Freed_Slots.Insert (Addr, Item);
         end if;
      end Probe_Free;

   end Memory_Allocator;

end MAT.Memory.Targets;
