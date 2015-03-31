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
with Ada.Containers.Vectors;

with MAT.Events.Targets;
package MAT.Events.Timelines is

   --  Describe a section of the timeline.  The section has a starting and ending
   --  event that marks the boundary of the section within the collected events.
   --  The timeline section gives the duration and some statistics about memory
   --  allocation made in the section.
   type Timeline_Info is record
      Start_Id       : MAT.Events.Targets.Event_Id_Type := 0;
      Start_Time     : MAT.Types.Target_Tick_Ref := 0;
      End_Id         : MAT.Events.Targets.Event_Id_Type := 0;
      End_Time       : MAT.Types.Target_Tick_Ref := 0;
      Duration       : MAT.Types.Target_Time := 0;
      Malloc_Count   : Natural := 0;
      Realloc_Count  : Natural := 0;
      Free_Count     : Natural := 0;
   end record;

   package Timeline_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Timeline_Info);

   subtype Timeline_Info_Vector is Timeline_Info_Vectors.Vector;
   subtype Timeline_Info_Cursor is Timeline_Info_Vectors.Cursor;

   procedure Extract (Target : in out MAT.Events.Targets.Target_Events;
                      Into   : in out Timeline_Info_Vector);

   --  Find in the events stream the events which are associated with a given event.
   --  When the <tt>Event</tt> is a memory allocation, find the associated reallocation
   --  and free events.  When the event is a free, find the associated allocations.
   --  Collect at most <tt>Max</tt> events.
   procedure Find_Related (Target : in out MAT.Events.Targets.Target_Events'Class;
                           Event  : in MAT.Events.Targets.Probe_Event_Type;
                           Max    : in Positive;
                           List   : in out MAT.Events.Targets.Target_Event_Vector);

end MAT.Events.Timelines;
