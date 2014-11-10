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
with Ada.Containers.Ordered_Maps;

with Util.Concurrent.Counters;

with MAT.Frames;
package MAT.Events.Targets is

   type Target_Event is record
      Event : MAT.Types.Uint16;
      Frame : MAT.Frames.Frame_Type;
   end record;

   type Target_Events is tagged limited private;
   type Target_Events_Access is access all Target_Events'Class;

   --  Add the event in the list of events and increment the event counter.
   procedure Insert (Target : in out Target_Events;
                     Event  : in MAT.Types.Uint16;
                     Frame  : in MAT.Events.Frame_Info);

   --  Get the current event counter.
   function Get_Event_Counter (Target : in Target_Events) return Integer;

private

   use type MAT.Types.Target_Time;
   package Event_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Time,
                                      Element_Type => Target_Event);

   subtype Event_Map is Event_Maps.Map;
   subtype Event_Cursor is Event_Maps.Cursor;

   protected type Event_Collector is

      --  Add the event in the list of events.
      procedure Insert (Event  : in MAT.Types.Uint16;
                        Frame  : in MAT.Events.Frame_Info);

   private
      Events        : Event_Map;
   end Event_Collector;

   type Target_Events is tagged limited record
      Events      : Event_Collector;
      Event_Count : Util.Concurrent.Counters.Counter;
   end record;

end MAT.Events.Targets;