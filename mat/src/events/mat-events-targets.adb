-----------------------------------------------------------------------
--  mat-events-targets - Events received and collected from a target
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

package body MAT.Events.Targets is

   --  ------------------------------
   --  Add the event in the list of events and increment the event counter.
   --  ------------------------------
   procedure Insert (Target : in out Target_Events;
                     Event  : in Probe_Event_Type) is
   begin
      Target.Events.Insert (Event);
      Util.Concurrent.Counters.Increment (Target.Event_Count);
   end Insert;

   procedure Get_Events (Target : in out Target_Events;
                         Start  : in MAT.Types.Target_Time;
                         Finish : in MAT.Types.Target_Time;
                         Into   : in out Target_Event_Vector) is
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
   --  Get the current event counter.
   --  ------------------------------
   function Get_Event_Counter (Target : in Target_Events) return Integer is
   begin
      return Util.Concurrent.Counters.Value (Target.Event_Count);
   end Get_Event_Counter;

   protected body Event_Collector is

      --  ------------------------------
      --  Add the event in the list of events.
      --  ------------------------------
      procedure Insert (Event : in Probe_Event_Type) is
         Info : Target_Event;
      begin
         if Current = null then
            Current := new Event_Block;
            Current.Start := Event.Time;
            Events.Insert (Event.Time, Current);
         end if;
         Current.Count := Current.Count + 1;
         Current.Events (Current.Count) := Event;
         Current.Events (Current.Count).Id := Last_Id;
         Last_Id := Last_Id + 1;
         Current.Finish := Event.Time;
         if Current.Count = Current.Events'Last then
            Current := null;
         end if;
      end Insert;

      procedure Get_Events (Start  : in MAT.Types.Target_Time;
                            Finish : in MAT.Types.Target_Time;
                            Into   : in out Target_Event_Vector) is
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

   end Event_Collector;

end MAT.Events.Targets;
