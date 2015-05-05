-----------------------------------------------------------------------
--  mat-events-timelines - Timelines
--  Copyright (C) 2015 Stephane Carrez
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
package body MAT.Events.Timelines is

   use MAT.Events.Targets;

   ITERATE_COUNT : constant MAT.Events.Event_Id_Type := 10_000;

   procedure Extract (Target : in out MAT.Events.Targets.Target_Events'Class;
                      Into   : in out Timeline_Info_Vector) is
      use type MAT.Types.Target_Time;
      use type MAT.Types.Target_Size;
      procedure Collect (Event : in MAT.Events.Target_Event_Type);

      First_Event : MAT.Events.Target_Event_Type;
      Last_Event  : MAT.Events.Target_Event_Type;
      Prev_Event  : MAT.Events.Target_Event_Type;
      Info        : Timeline_Info;
      First_Id    : MAT.Events.Event_Id_Type;

      procedure Collect (Event : in MAT.Events.Target_Event_Type) is
         Dt : constant MAT.Types.Target_Time := Event.Time - Prev_Event.Time;
      begin
         if Dt > 500_000 then
            Into.Append (Info);
            Info.Malloc_Count := 0;
            Info.Realloc_Count := 0;
            Info.Free_Count := 0;
            Info.First_Event := Event;
            Info.Free_Size := 0;
            Info.Alloc_Size := 0;
            Prev_Event := Event;
         end if;
         Info.Last_Event := Event;
         if Event.Event = 2 then
            Info.Malloc_Count := Info.Malloc_Count + 1;
            Info.Alloc_Size := Info.Alloc_Size + Event.Size;
         elsif Event.Event = 3 then
            Info.Realloc_Count := Info.Realloc_Count + 1;
            Info.Alloc_Size := Info.Alloc_Size + Event.Size;
            Info.Free_Size := Info.Free_Size + Event.Old_Size;
         elsif Event.Event = 4 then
            Info.Free_Count := Info.Free_Count + 1;
            Info.Free_Size := Info.Free_Size + Event.Size;
         end if;
      end Collect;

   begin
      Target.Get_Limits (First_Event, Last_Event);
      Prev_Event      := First_Event;
      Info.First_Event := First_Event;
      First_Id := First_Event.Id;
      while First_Id < Last_Event.Id loop
         Target.Iterate (Start   => First_Id,
                         Finish  => First_Id + ITERATE_COUNT,
                         Process => Collect'Access);
         First_Id := First_Id + ITERATE_COUNT;
      end loop;
   end Extract;

   --  ------------------------------
   --  Find in the events stream the events which are associated with a given event.
   --  When the <tt>Event</tt> is a memory allocation, find the associated reallocation
   --  and free events.  When the event is a free, find the associated allocations.
   --  Collect at most <tt>Max</tt> events.
   --  ------------------------------
   procedure Find_Related (Target : in out MAT.Events.Targets.Target_Events'Class;
                           Event  : in MAT.Events.Target_Event_Type;
                           Max    : in Positive;
                           List   : in out MAT.Events.Tools.Target_Event_Vector) is

      procedure Collect_Free (Event : in MAT.Events.Target_Event_Type);
      procedure Collect_Alloc (Event : in MAT.Events.Target_Event_Type);

      First_Id    : MAT.Events.Event_Id_Type;
      Last_Id     : MAT.Events.Event_Id_Type;
      First_Event : MAT.Events.Target_Event_Type;
      Last_Event  : MAT.Events.Target_Event_Type;
      Addr        : MAT.Types.Target_Addr := Event.Addr;

      Done : exception;

      procedure Collect_Free (Event : in MAT.Events.Target_Event_Type) is
      begin
         if Event.Index = MAT.Events.MSG_FREE and then Event.Addr = Addr then
            List.Append (Event);
            raise Done;
         end if;
         if Event.Index = MAT.Events.MSG_REALLOC and then Event.Old_Addr = Addr then
            List.Append (Event);
            if Positive (List.Length) >= Max then
               raise Done;
            end if;
            Addr := Event.Addr;
         end if;
      end Collect_Free;

      procedure Collect_Alloc (Event : in MAT.Events.Target_Event_Type) is
      begin
         if Event.Index = MAT.Events.MSG_MALLOC and then Event.Addr = Addr then
            List.Append (Event);
            raise Done;
         end if;
         if Event.Index = MAT.Events.MSG_REALLOC and then Event.Addr = Addr then
            List.Append (Event);
            if Positive (List.Length) >= Max then
               raise Done;
            end if;
            Addr := Event.Old_Addr;
         end if;
      end Collect_Alloc;

   begin
      Target.Get_Limits (First_Event, Last_Event);
      First_Id := Event.Id;
      if Event.Index = MAT.Events.MSG_FREE then
         --  Search backward for MSG_MALLOC and MSG_REALLOC.
         First_Id := First_Id - 1;
         while First_Id > First_Event.Id loop
            if First_Id > ITERATE_COUNT then
               Last_Id := First_Id - ITERATE_COUNT;
            else
               Last_Id := First_Event.Id;
            end if;
            Target.Iterate (Start   => First_Id,
                            Finish  => Last_Id,
                            Process => Collect_Alloc'Access);
            First_Id := Last_Id;
         end loop;
      else
         --  Search forward for MSG_REALLOC and MSG_FREE
         First_Id := First_Id + 1;
         while First_Id < Last_Event.Id loop
            Target.Iterate (Start   => First_Id,
                            Finish  => First_Id + ITERATE_COUNT,
                            Process => Collect_Free'Access);
            First_Id := First_Id + ITERATE_COUNT;
         end loop;
      end if;

   exception
      when Done =>
         null;
   end Find_Related;

   --  ------------------------------
   --  Find the sizes of malloc and realloc events which is selected by the given filter.
   --  Update the <tt>Sizes</tt> map to keep track of the first event and last event and
   --  the number of events found for the corresponding size.
   --  ------------------------------
   procedure Find_Sizes (Target : in out MAT.Events.Targets.Target_Events'Class;
                         Filter : in MAT.Expressions.Expression_Type;
                         Sizes  : in out MAT.Events.Targets.Size_Event_Info_Map) is
      procedure Collect_Event (Event : in MAT.Events.Target_Event_Type);

      procedure Collect_Event (Event : in MAT.Events.Target_Event_Type) is
         procedure Update_Size (Size : in MAT.Types.Target_Size;
                                Info : in out MAT.Events.Targets.Event_Info_Type);

         procedure Update_Size (Size : in MAT.Types.Target_Size;
                                Info : in out MAT.Events.Targets.Event_Info_Type) is
            pragma Unreferenced (Size);
         begin
            Info.Count      := Info.Count + 1;
            Info.Last_Event := Event;
            if Event.Index = MAT.Events.MSG_MALLOC then
               Info.Alloc_Size := Info.Alloc_Size + Event.Size;
            elsif Event.Index = MAT.Events.MSG_REALLOC then
               Info.Alloc_Size := Info.Alloc_Size + Event.Size;
               Info.Free_Size := Info.Alloc_Size + Event.Old_Size;
            else
               Info.Free_Size := Info.Alloc_Size + Event.Size;
            end if;
         end Update_Size;

      begin
         --  Look for malloc or realloc events which are selected by the filter.
         if (Event.Index /= MAT.Events.MSG_MALLOC
             and Event.Index /= MAT.Events.MSG_FREE
             and Event.Index /= MAT.Events.MSG_REALLOC)
           or else not Filter.Is_Selected (Event)
         then
            return;
         end if;
         declare
            Pos : constant MAT.Events.Targets.Size_Event_Info_Cursor := Sizes.Find (Event.Size);
         begin
            if MAT.Events.Targets.Size_Event_Info_Maps.Has_Element (Pos) then
               --  Increment the count and update the last event.
               Sizes.Update_Element (Pos, Update_Size'Access);
            else
               declare
                  Info : Event_Info_Type;
               begin
                  --  Insert a new size with the event.
                  Info.First_Event := Event;
                  Info.Last_Event := Event;
                  Info.Count := 1;
                  if Event.Index = MAT.Events.MSG_MALLOC then
                     Info.Alloc_Size := Event.Size;
                  elsif Event.Index = MAT.Events.MSG_REALLOC then
                     Info.Alloc_Size := Event.Size;
                     Info.Free_Size := Event.Old_Size;
                  else
                     Info.Free_Size := Event.Size;
                  end if;
                  Sizes.Insert (Event.Size, Info);
               end;
            end if;
         end;
      end Collect_Event;

   begin
      Target.Iterate (Process => Collect_Event'Access);
   end Find_Sizes;

   --  ------------------------------
   --  Find the function address from the call event frames for the events which is selected
   --  by the given filter.  The function addresses are collected up to the given frame depth.
   --  Update the <tt>Frames</tt> map to keep track of the first event and last event and
   --  the number of events found for the corresponding frame address.
   --  ------------------------------
   procedure Find_Frames (Target : in out MAT.Events.Targets.Target_Events'Class;
                          Filter : in MAT.Expressions.Expression_Type;
                          Depth  : in Natural;
                          Frames : in out MAT.Events.Targets.Frame_Event_Info_Map) is
      procedure Collect_Event (Event : in MAT.Events.Target_Event_Type);

      procedure Collect_Event (Event : in MAT.Events.Target_Event_Type) is
         procedure Update_Size (Size : in MAT.Types.Target_Size;
                                Info : in out MAT.Events.Targets.Event_Info_Type);

         procedure Update_Size (Size : in MAT.Types.Target_Size;
                                Info : in out MAT.Events.Targets.Event_Info_Type) is
            pragma Unreferenced (Size);
         begin
            Info.Count      := Info.Count + 1;
            Info.Last_Event := Event;
         end Update_Size;

      begin
         --  Look for events which are selected by the filter.
         if not Filter.Is_Selected (Event) then
            return;
         end if;
         declare
            Backtrace : constant MAT.Frames.Frame_Table := MAT.Frames.Backtrace (Event.Frame);
         begin
            for I in Backtrace'Range loop
               exit when I > Depth;
               declare
                  Pos : constant MAT.Events.Targets.Frame_Event_Info_Cursor
                    := Frames.Find (Backtrace (I));
               begin
                  if MAT.Events.Targets.Frame_Event_Info_Maps.Has_Element (Pos) then
                     --  Increment the count and update the last event.
                     Frames.Update_Element (Pos, Update_Size'Access);
                  else
                     declare
                        Info : Event_Info_Type;
                     begin
                        --  Insert a new size with the event.
                        Info.First_Event := Event;
                        Info.Last_Event := Event;
                        Info.Frame_Addr := Backtrace (I);
                        Info.Count := 1;
                        Frames.Insert (Backtrace (I), Info);
                     end;
                  end if;
               end;
            end loop;
         end;
      end Collect_Event;

   begin
      Target.Iterate (Process => Collect_Event'Access);
   end Find_Frames;

end MAT.Events.Timelines;
