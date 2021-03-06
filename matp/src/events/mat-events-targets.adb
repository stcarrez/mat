-----------------------------------------------------------------------
--  mat-events-targets - Events received and collected from a target
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
with Ada.Unchecked_Deallocation;
package body MAT.Events.Targets is

   ITERATE_COUNT : constant Event_Id_Type := 10_000;

   --  ------------------------------
   --  Add the event in the list of events and increment the event counter.
   --  Update the event instance to allocate the event Id.
   --  ------------------------------
   procedure Insert (Target : in out Target_Events;
                     Event  : in out Target_Event_Type) is
   begin
      Target.Events.Insert (Event);
      Util.Concurrent.Counters.Increment (Target.Event_Count);
   end Insert;

   --  ------------------------------
   --  Update the Size and Prev_Id information in the event identified by <tt>Id</tt>.
   --  Update the event represented by <tt>Prev_Id</tt> so that its Next_Id refers
   --  to the <tt>Id</tt> event.
   --  ------------------------------
   procedure Update_Event (Target  : in out Target_Events;
                           Id      : in Event_Id_Type;
                           Size    : in MAT.Types.Target_Size;
                           Prev_Id : in Event_Id_Type) is
   begin
      Target.Events.Update_Event (Id, Size, Prev_Id);
   end Update_Event;

   procedure Get_Events (Target : in out Target_Events;
                         Start  : in MAT.Types.Target_Time;
                         Finish : in MAT.Types.Target_Time;
                         Into   : in out MAT.Events.Tools.Target_Event_Vector) is
   begin
      Target.Events.Get_Events (Start, Finish, Into);
   end Get_Events;

   --  ------------------------------
   --  Get the start and finish time for the events that have been received.
   --  ------------------------------
   procedure Get_Time_Range (Target : in out Target_Events;
                             Start  : out MAT.Types.Target_Time;
                             Finish : out MAT.Types.Target_Time) is
   begin
      Target.Events.Get_Time_Range (Start, Finish);
   end Get_Time_Range;

   --  ------------------------------
   --  Get the first and last event that have been received.
   --  ------------------------------
   procedure Get_Limits (Target : in out Target_Events;
                         First  : out Target_Event_Type;
                         Last   : out Target_Event_Type) is
   begin
      Target.Events.Get_Limits (First, Last);
   end Get_Limits;

   --  ------------------------------
   --  Get the probe event with the given allocated unique id.
   --  ------------------------------
   function Get_Event (Target : in Target_Events;
                       Id     : in Event_Id_Type) return Target_Event_Type is
   begin
      return Target.Events.Get_Event (Id);
   end Get_Event;

   --  ------------------------------
   --  Get the current event counter.
   --  ------------------------------
   function Get_Event_Counter (Target : in Target_Events) return Integer is
   begin
      return Util.Concurrent.Counters.Value (Target.Event_Count);
   end Get_Event_Counter;

   --  ------------------------------
   --  Iterate over the events starting from the <tt>Start</tt> event and until the
   --  <tt>Finish</tt> event is found (inclusive).  Execute the <tt>Process</tt> procedure
   --  with each event instance.
   --  ------------------------------
   procedure Iterate (Target  : in out Target_Events;
                      Start   : in Event_Id_Type;
                      Finish  : in Event_Id_Type;
                      Process : access procedure (Event : in Target_Event_Type)) is
   begin
      Target.Events.Iterate (Start, Finish, Process);
   end Iterate;

   --  ------------------------------
   --  Iterate over the events starting from first first event up to the last event collected.
   --  Execute the <tt>Process</tt> procedure with each event instance.
   --  ------------------------------
   procedure Iterate (Target  : in out Target_Events;
                      Process : access procedure (Event : in Target_Event_Type)) is
      First_Event : Target_Event_Type;
      Last_Event  : Target_Event_Type;
      First_Id    : Event_Id_Type;
   begin
      Target.Get_Limits (First_Event, Last_Event);
      First_Id := First_Event.Id;
      while First_Id < Last_Event.Id loop
         --  Iterate over the events in groups of 10_000 to release the lock and give some
         --  opportunity to the server thread to add new events.
         Target.Iterate (Start   => First_Id,
                         Finish  => First_Id + ITERATE_COUNT,
                         Process => Process);
         First_Id := First_Id + ITERATE_COUNT;
      end loop;
   end Iterate;

   --  ------------------------------
   --  Release the storage allocated for the events.
   --  ------------------------------
   overriding
   procedure Finalize (Target : in out Target_Events) is
   begin
      Target.Events.Clear;
   end Finalize;

   protected body Event_Collector is

      procedure Update (Id      : in Event_Id_Type;
                        Process : not null access procedure (Event : in out Target_Event_Type));

      --  ------------------------------
      --  Internal operation to update the event represented by <tt>Id</tt>.
      --  ------------------------------
      procedure Update (Id      : in Event_Id_Type;
                        Process : not null access procedure (Event : in out Target_Event_Type)) is
         Iter  : constant Event_Id_Cursor := Ids.Floor (Id);
         Block : Event_Block_Access;
         Pos   : Event_Id_Type;
      begin
         if Event_Id_Maps.Has_Element (Iter) then
            Block := Event_Id_Maps.Element (Iter);
            Pos := Id - Block.Events (Block.Events'First).Id + Block.Events'First;
            if Pos <= Block.Count then
               Process (Block.Events (Pos));
            end if;
         end if;
      end Update;

      --  ------------------------------
      --  Update the Size and Prev_Id information in the event identified by <tt>Id</tt>.
      --  Update the event represented by <tt>Prev_Id</tt> so that its Next_Id refers
      --  to the <tt>Id</tt> event.
      --  ------------------------------
      procedure Update_Event (Id      : in Event_Id_Type;
                              Size    : in MAT.Types.Target_Size;
                              Prev_Id : in Event_Id_Type) is
         procedure Update_Size (Event : in out Target_Event_Type);
         procedure Update_Next (Event : in out Target_Event_Type);

         procedure Update_Size (Event : in out Target_Event_Type) is
         begin
            if Event.Index = MSG_REALLOC then
               Event.Old_Size := Size;
            else
               Event.Size    := Size;
            end if;
            Event.Prev_Id := Prev_Id;
         end Update_Size;

         procedure Update_Next (Event : in out Target_Event_Type) is
         begin
            Event.Next_Id := Id;
         end Update_Next;

      begin
         Update (Id, Update_Size'Access);
         Update (Prev_Id, Update_Next'Access);
      end Update_Event;

      --  ------------------------------
      --  Add the event in the list of events.
      --  Update the event instance to allocate the event Id.
      --  ------------------------------
      procedure Insert (Event : in out Target_Event_Type) is
      begin
         if Current = null then
            Current := new Event_Block;
            Current.Start := Event.Time;
            Events.Insert (Event.Time, Current);
            Ids.Insert (Last_Id, Current);
         end if;
         Event.Id := Last_Id;
         Current.Count := Current.Count + 1;
         Current.Events (Current.Count) := Event;
         Last_Id := Last_Id + 1;
         Current.Finish := Event.Time;
         if Current.Count = Current.Events'Last then
            Current := null;
         end if;
      end Insert;

      procedure Get_Events (Start  : in MAT.Types.Target_Time;
                            Finish : in MAT.Types.Target_Time;
                            Into   : in out MAT.Events.Tools.Target_Event_Vector) is
         Iter  : Event_Cursor := Events.Floor (Start);
         Block : Event_Block_Access;
      begin
         while Event_Maps.Has_Element (Iter) loop
            Block := Event_Maps.Element (Iter);
            exit when Block.Start > Finish;
            for I in Block.Events'First .. Block.Count loop
               exit when Block.Events (I).Time > Finish;
               if Block.Events (I).Time >= Start then
                  Into.Append (Block.Events (I));
               end if;
            end loop;
            Event_Maps.Next (Iter);
         end loop;
      end Get_Events;

      --  ------------------------------
      --  Get the start and finish time for the events that have been received.
      --  ------------------------------
      procedure Get_Time_Range (Start  : out MAT.Types.Target_Time;
                                Finish : out MAT.Types.Target_Time) is
         First : constant Event_Block_Access := Events.First_Element;
         Last  : constant Event_Block_Access := Events.Last_Element;
      begin
         Start  := First.Events (First.Events'First).Time;
         Finish := Last.Events (Last.Count).Time;
      end Get_Time_Range;

      --  ------------------------------
      --  Get the first and last event that have been received.
      --  ------------------------------
      procedure Get_Limits (First : out Target_Event_Type;
                            Last  : out Target_Event_Type) is
         First_Block : constant Event_Block_Access := Events.First_Element;
         Last_Block  : constant Event_Block_Access := Events.Last_Element;
      begin
         First := First_Block.Events (First_Block.Events'First);
         Last  := Last_Block.Events (Last_Block.Count);
      end Get_Limits;

      --  ------------------------------
      --  Get the probe event with the given allocated unique id.
      --  ------------------------------
      function Get_Event (Id : in Event_Id_Type) return Target_Event_Type is
         Iter  : Event_Id_Cursor := Ids.Floor (Id);
         Block : Event_Block_Access;
         Pos   : Event_Id_Type;
      begin
         while Event_Id_Maps.Has_Element (Iter) loop
            Block := Event_Id_Maps.Element (Iter);
            exit when Id < Block.Events (Block.Events'First).Id;
            Pos := Id - Block.Events (Block.Events'First).Id + Block.Events'First;
            if Pos <= Block.Count then
               return Block.Events (Pos);
            end if;
            Event_Id_Maps.Next (Iter);
         end loop;
         raise MAT.Events.Tools.Not_Found;
      end Get_Event;

      --  ------------------------------
      --  Iterate over the events starting from the <tt>Start</tt> event and until the
      --  <tt>Finish</tt> event is found (inclusive).  Execute the <tt>Process</tt> procedure
      --  with each event instance.
      --  ------------------------------
      procedure Iterate (Start   : in Event_Id_Type;
                         Finish  : in Event_Id_Type;
                         Process : access procedure (Event : in Target_Event_Type)) is
         Iter  : Event_Id_Cursor := Ids.Floor (Start);
         Block : Event_Block_Access;
         Pos   : Event_Id_Type;
         Id    : Event_Id_Type := Start;
      begin
         --  First, find the block and position of the first event.
         while Event_Id_Maps.Has_Element (Iter) loop
            Block := Event_Id_Maps.Element (Iter);
            exit when Id < Block.Events (Block.Events'First).Id;
            Pos := Id - Block.Events (Block.Events'First).Id + Block.Events'First;
            if Start <= Finish then
               if Pos <= Block.Count then

                  --  Second, iterate over the events moving to the next event block
                  --  until we reach the last event.
                  loop
                     Process (Block.Events (Pos));
                     exit when Id > Finish;
                     Pos := Pos + 1;
                     Id := Id + 1;
                     if Pos > Block.Count then
                        Event_Id_Maps.Next (Iter);
                        exit when not Event_Id_Maps.Has_Element (Iter);
                        Block := Event_Id_Maps.Element (Iter);
                        Pos := Block.Events'First;
                     end if;
                  end loop;
               end if;
               Event_Id_Maps.Next (Iter);
            else
               if Pos <= Block.Count then

                  --  Second, iterate over the events moving to the next event block
                  --  until we reach the last event.
                  loop
                     Process (Block.Events (Pos));
                     exit when Id <= Finish;
                     Id := Id - 1;
                     if Pos = Block.Events'First then
                        Event_Id_Maps.Previous (Iter);
                        exit when not Event_Id_Maps.Has_Element (Iter);
                        Block := Event_Id_Maps.Element (Iter);
                        Pos := Block.Count;
                     else
                        Pos := Pos - 1;
                     end if;
                  end loop;
               end if;
               Event_Id_Maps.Previous (Iter);
            end if;
         end loop;
      end Iterate;

      --  ------------------------------
      --  Clear the events.
      --  ------------------------------
      procedure Clear is
         procedure Free is
            new Ada.Unchecked_Deallocation (Event_Block, Event_Block_Access);
      begin
         while not Events.Is_Empty loop
            declare
               Block : Event_Block_Access := Events.First_Element;
            begin
               Free (Block);
               Events.Delete_First;
            end;
         end loop;
         Current := null;
         Last_Id := 0;
         Ids.Clear;
      end Clear;

   end Event_Collector;

end MAT.Events.Targets;
