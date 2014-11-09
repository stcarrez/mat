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
                     Event  : in MAT.Types.Uint16;
                     Frame  : in MAT.Events.Frame_Info) is
   begin
      Target.Events.Insert (Event, Frame);
      Util.Concurrent.Counters.Increment (Target.Event_Count);
   end Insert;

   --  ------------------------------
   --  Get the current event counter.
   --  ------------------------------
   function Get_Event_Counter (Target : in Target_Events) return Integer is
   begin
      return Util.Concurrent.Counters.Value (Target.Event_Count);
   end Get_EVent_Counter;

   protected body Event_Collector is

      --  ------------------------------
      --  Add the event in the list of events.
      --  ------------------------------
      procedure Insert (Event  : in MAT.Types.Uint16;
                        Frame  : in MAT.Events.Frame_Info) is
      begin
         null;
      end Insert;

   end Event_Collector;

end MAT.Events.Targets;
