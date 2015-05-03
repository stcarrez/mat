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
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Finalization;

with Util.Concurrent.Counters;

with MAT.Frames;
package MAT.Events.Targets is

   Not_Found : exception;

   type Event_Type is mod 16;
   type Probe_Index_Type is (MSG_BEGIN,
                             MSG_END,
                             MSG_LIBRARY,
                             MSG_MALLOC,
                             MSG_FREE,
                             MSG_REALLOC
                             );

   type Event_Id_Type is new Natural;

   type Probe_Event_Type is record
      Id       : Event_Id_Type;
      Next_Id  : Event_Id_Type := 0;
      Prev_Id  : Event_Id_Type := 0;
      Event    : MAT.Types.Uint16;
      Index    : Probe_Index_Type;
      Time     : MAT.Types.Target_Tick_Ref;
      Thread   : MAT.Types.Target_Thread_Ref;
      Frame    : MAT.Frames.Frame_Type;
      Addr     : MAT.Types.Target_Addr;
      Size     : MAT.Types.Target_Size;
      Old_Addr : MAT.Types.Target_Addr;
      Old_Size : MAT.Types.Target_Size;
   end record;

   subtype Target_Event is Probe_Event_Type;

   package Target_Event_Vectors is
     new Ada.Containers.Vectors (Positive, Target_Event);

   subtype Target_Event_Vector is Target_Event_Vectors.Vector;
   subtype Target_Event_Cursor is Target_Event_Vectors.Cursor;

   --  Find in the list the first event with the given type.
   --  Raise <tt>Not_Found</tt> if the list does not contain such event.
   function Find (List : in Target_Event_Vector;
                  Kind : in Probe_Index_Type) return Probe_Event_Type;

   type Event_Info_Type is record
      First_Event : Target_Event;
      Last_Event  : Target_Event;
      Frame_Addr  : MAT.Types.Target_Addr;
      Count       : Natural;
   end record;

   package Size_Event_Info_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Size,
                                      Element_Type => Event_Info_Type);
   subtype Size_Event_Info_Map is Size_Event_Info_Maps.Map;
   subtype Size_Event_Info_Cursor is Size_Event_Info_Maps.Cursor;

   package Frame_Event_Info_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Addr,
                                      Element_Type => Event_Info_Type);
   subtype Frame_Event_Info_Map is Frame_Event_Info_Maps.Map;
   subtype Frame_Event_Info_Cursor is Frame_Event_Info_Maps.Cursor;

   package Event_Info_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Event_Info_Type);
   subtype Event_Info_Vector is Event_Info_Vectors.Vector;
   subtype Event_Info_Cursor is Event_Info_Vectors.Cursor;

   --  Extract from the frame info map, the list of event info sorted on the count.
   procedure Build_Event_Info (Map : in Frame_Event_Info_Map;
                               List : in out Event_Info_Vector);

   type Target_Events is tagged limited private;
   type Target_Events_Access is access all Target_Events'Class;

   --  Add the event in the list of events and increment the event counter.
   --  Update the event instance to allocate the event Id.
   procedure Insert (Target : in out Target_Events;
                     Event  : in out Probe_Event_Type);

   procedure Get_Events (Target : in out Target_Events;
                         Start  : in MAT.Types.Target_Time;
                         Finish : in MAT.Types.Target_Time;
                         Into   : in out Target_Event_Vector);

   --  Get the start and finish time for the events that have been received.
   procedure Get_Time_Range (Target : in out Target_Events;
                             Start  : out MAT.Types.Target_Time;
                             Finish : out MAT.Types.Target_Time);

   --  Get the probe event with the given allocated unique id.
   function Get_Event (Target : in Target_Events;
                       Id     : in Event_Id_Type) return Probe_Event_Type;

   --  Get the first and last event that have been received.
   procedure Get_Limits (Target : in out Target_Events;
                         First  : out Probe_Event_Type;
                         Last   : out Probe_Event_Type);

   --  Get the current event counter.
   function Get_Event_Counter (Target : in Target_Events) return Integer;

   --  Iterate over the events starting from the <tt>Start</tt> event and until the
   --  <tt>Finish</tt> event is found (inclusive).  Execute the <tt>Process</tt> procedure
   --  with each event instance.
   procedure Iterate (Target  : in out Target_Events;
                      Start   : in Event_Id_Type;
                      Finish  : in Event_Id_Type;
                      Process : access procedure (Event : in Probe_Event_Type));

   --  Iterate over the events starting from first first event up to the last event collected.
   --  Execute the <tt>Process</tt> procedure with each event instance.
   procedure Iterate (Target  : in out Target_Events;
                      Process : access procedure (Event : in Target_Event));

private

   EVENT_BLOCK_SIZE : constant Event_Id_Type := 1024;

   type Probe_Event_Array is array (1 .. EVENT_BLOCK_SIZE) of Probe_Event_Type;

   type Event_Block is record
      Start  : MAT.Types.Target_Time;
      Finish : MAT.Types.Target_Time;
      Count  : Event_Id_Type := 0;
      Events : Probe_Event_Array;
   end record;
   type Event_Block_Access is access all Event_Block;

   use type MAT.Types.Target_Time;
   package Event_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Time,
                                      Element_Type => Event_Block_Access);

   subtype Event_Map is Event_Maps.Map;
   subtype Event_Cursor is Event_Maps.Cursor;

   package Event_Id_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => Event_Id_Type,
                                      Element_Type => Event_Block_Access);

   subtype Event_Id_Map is Event_Id_Maps.Map;
   subtype Event_Id_Cursor is Event_Id_Maps.Cursor;

   protected type Event_Collector is

      --  Update the Size and Prev_Id information in the event identified by <tt>Id</tt>.
      --  Update the event represented by <tt>Prev_Id</tt> so that its Next_Id refers
      --  to the <tt>Id</tt> event.
      procedure Update_Event (Id      : in Event_Id_Type;
                              Size    : in MAT.Types.Target_Size;
                              Prev_Id : in Event_Id_Type);

      --  Add the event in the list of events.
      --  Update the event instance to allocate the event Id.
      procedure Insert (Event  : in out Probe_Event_Type);

      procedure Get_Events (Start  : in MAT.Types.Target_Time;
                            Finish : in MAT.Types.Target_Time;
                            Into   : in out Target_Event_Vector);

      --  Get the first and last event that have been received.
      procedure Get_Limits (First : out Probe_Event_Type;
                            Last  : out Probe_Event_Type);

      --  Get the start and finish time for the events that have been received.
      procedure Get_Time_Range (Start  : out MAT.Types.Target_Time;
                                Finish : out MAT.Types.Target_Time);

      --  Get the probe event with the given allocated unique id.
      function Get_Event (Id : in Event_Id_Type) return Probe_Event_Type;

      --  Iterate over the events starting from the <tt>Start</tt> event and until the
      --  <tt>Finish</tt> event is found (inclusive).  Execute the <tt>Process</tt> procedure
      --  with each event instance.
      procedure Iterate (Start   : in Event_Id_Type;
                         Finish  : in Event_Id_Type;
                         Process : access procedure (Event : in Probe_Event_Type));

      --  Clear the events.
      procedure Clear;

   private
      Current       : Event_Block_Access := null;
      Events        : Event_Map;
      Ids           : Event_Id_Map;
      Last_Id       : Event_Id_Type := 0;
   end Event_Collector;

   type Target_Events is new Ada.Finalization.Limited_Controlled with record
      Events      : Event_Collector;
      Event_Count : Util.Concurrent.Counters.Counter;
   end record;

   --  Release the storage allocated for the events.
   overriding
   procedure Finalize (Target : in out Target_Events);

end MAT.Events.Targets;
