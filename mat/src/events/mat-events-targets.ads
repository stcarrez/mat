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
with Ada.Containers.Vectors;

with Util.Concurrent.Counters;

with MAT.Frames;
package MAT.Events.Targets is

   type Event_Type is mod 16;
   type Probe_Index_Type is mod 16;

   type Probe_Event_Type is record
      Event    : MAT.Types.Uint16;
      Index    : Probe_Index_Type;
      Time     : MAT.Types.Target_Time;
      Thread   : MAT.Types.Target_Thread_Ref;
      Frame    : MAT.Frames.Frame_Type;
      Addr     : MAT.Types.Target_Addr;
      Size     : MAT.Types.Target_Size;
      Old_Addr : MAT.Types.Target_Addr;
   end record;

   subtype Target_Event is Probe_Event_Type;

   package Target_Event_Vectors is
     new Ada.Containers.Vectors (Positive, Target_Event);

   subtype Target_Event_Vector is Target_Event_Vectors.Vector;
   subtype Target_Event_Cursor is Target_Event_Vectors.Cursor;

   type Target_Events is tagged limited private;
   type Target_Events_Access is access all Target_Events'Class;

   --  Add the event in the list of events and increment the event counter.
   procedure Insert (Target : in out Target_Events;
                     Event  : in Probe_Event_Type);

   procedure Get_Events (Target : in out Target_Events;
                         Start  : in MAT.Types.Target_Time;
                         Finish : in MAT.Types.Target_Time;
                         Into   : in out Target_Event_Vector);

   --  Get the current event counter.
   function Get_Event_Counter (Target : in Target_Events) return Integer;

private

   EVENT_BLOCK_SIZE : constant Positive := 1024;

   type Probe_Event_Array is array (1 .. EVENT_BLOCK_SIZE) of Probe_Event_Type;

   type Event_Block is record
      Start  : MAT.Types.Target_Time;
      Finish : MAT.Types.Target_Time;
      Count  : Natural := 0;
      Events : Probe_Event_Array;
   end record;
   type Event_Block_Access is access all Event_Block;

   use type MAT.Types.Target_Time;
   package Event_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Time,
                                      Element_Type => Event_Block_Access);

   subtype Event_Map is Event_Maps.Map;
   subtype Event_Cursor is Event_Maps.Cursor;

   protected type Event_Collector is

      --  Add the event in the list of events.
      procedure Insert (Event  : in Probe_Event_Type);

      procedure Get_Events (Start  : in MAT.Types.Target_Time;
                            Finish : in MAT.Types.Target_Time;
                            Into   : in out Target_Event_Vector);

   private
      Current       : Event_Block_Access := null;
      Events        : Event_Map;
   end Event_Collector;

   type Target_Events is tagged limited record
      Events      : Event_Collector;
      Event_Count : Util.Concurrent.Counters.Counter;
   end record;

end MAT.Events.Targets;
