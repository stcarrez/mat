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

package body MAT.Events.Timelines is

   use MAT.Events.Targets;

   ITERATE_COUNT : constant MAT.Events.Targets.Event_Id_Type := 10_000;

   procedure Extract (Target : in out MAT.Events.Targets.Target_Events;
                      Into   : in out Timeline_Info_Vector) is
      use type MAT.Types.Target_Time;
      procedure Collect (Event : in MAT.Events.Targets.Probe_Event_Type);

      First_Event : MAT.Events.Targets.Probe_Event_Type;
      Last_Event  : MAT.Events.Targets.Probe_Event_Type;
      Prev_Event  : MAT.Events.Targets.Probe_Event_Type;
      Info        : Timeline_Info;
      First_Id    : MAT.Events.Targets.Event_Id_Type;

      procedure Collect (Event : in MAT.Events.Targets.Probe_Event_Type) is
         Dt : constant MAT.Types.Target_Time := Event.Time - Prev_Event.Time;
      begin
         if Dt > 500_000 then
            Into.Append (Info);
            Info.Malloc_Count := 0;
            Info.Realloc_Count := 0;
            Info.Free_Count := 0;
            Info.Start_Id := Event.Id;
            Info.Start_Time := Event.Time;
         end if;
         Info.End_Id := Event.Id;
         Info.End_Time := Event.Time;
         if Event.Event = 2 then
            Info.Malloc_Count := Info.Malloc_Count + 1;
         elsif Event.Event = 3 then
            Info.Realloc_Count := Info.Realloc_Count + 1;
         elsif Event.Event = 4 then
            Info.Free_Count := Info.Free_Count + 1;
         end if;
      end Collect;

   begin
      Target.Get_Limits (First_Event, Last_Event);
      Prev_Event      := First_Event;
      Info.Start_Id   := First_Event.Id;
      Info.Start_Time := First_Event.Time;
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
                           Event  : in MAT.Events.Targets.Probe_Event_Type;
                           Max    : in Positive;
                           List   : in out MAT.Events.Targets.Target_Event_Vector) is

      procedure Collect_Free (Event : in MAT.Events.Targets.Probe_Event_Type);
      procedure Collect_Alloc (Event : in MAT.Events.Targets.Probe_Event_Type);

      First_Id    : MAT.Events.Targets.Event_Id_Type;
      Last_Id     : MAT.Events.Targets.Event_Id_Type;
      First_Event : MAT.Events.Targets.Probe_Event_Type;
      Last_Event  : MAT.Events.Targets.Probe_Event_Type;
      Addr        : MAT.Types.Target_Addr := Event.Addr;

      Done : exception;

      procedure Collect_Free (Event : in MAT.Events.Targets.Probe_Event_Type) is
      begin
         if Event.Index = MAT.Events.Targets.MSG_FREE and then Event.Addr = Addr then
            List.Append (Event);
            raise Done;
         end if;
         if Event.Index = MAT.Events.Targets.MSG_REALLOC and then Event.Old_Addr = Addr then
            List.Append (Event);
            if Positive (List.Length) >= Max then
               raise Done;
            end if;
            Addr := Event.Addr;
         end if;
      end Collect_Free;

      procedure Collect_Alloc (Event : in MAT.Events.Targets.Probe_Event_Type) is
      begin
         if Event.Index = MAT.Events.Targets.MSG_MALLOC and then Event.Addr = Addr then
            List.Append (Event);
            raise Done;
         end if;
         if Event.Index = MAT.Events.Targets.MSG_REALLOC and then Event.Addr = Addr then
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
      if Event.Index = MAT.Events.Targets.MSG_FREE then
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

end MAT.Events.Timelines;
