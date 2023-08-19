-----------------------------------------------------------------------
--  mat-events-tools - Profiler Events Description
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
package MAT.Events.Tools is

   Not_Found : exception;

   package Target_Event_Vectors is
     new Ada.Containers.Vectors (Positive, Target_Event_Type);

   subtype Target_Event_Vector is Target_Event_Vectors.Vector;
   subtype Target_Event_Cursor is Target_Event_Vectors.Cursor;

   --  Find in the list the first event with the given type.
   --  Raise <tt>Not_Found</tt> if the list does not contain such event.
   function Find (List : in Target_Event_Vector;
                  Kind : in Probe_Index_Type) return Target_Event_Type;

   type Event_Info_Type is record
      First_Event    : Target_Event_Type;
      Last_Event     : Target_Event_Type;
      Count          : Natural := 0;
      Malloc_Count   : Natural := 0;
      Realloc_Count  : Natural := 0;
      Free_Count     : Natural := 0;
      Alloc_Size     : MAT.Types.Target_Size := 0;
      Free_Size      : MAT.Types.Target_Size := 0;
   end record;

   --  Collect statistics information about events.
   procedure Collect_Info (Into  : in out Event_Info_Type;
                           Event : in MAT.Events.Target_Event_Type);

   package Size_Event_Info_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Size,
                                      Element_Type => Event_Info_Type);
   subtype Size_Event_Info_Map is Size_Event_Info_Maps.Map;
   subtype Size_Event_Info_Cursor is Size_Event_Info_Maps.Cursor;


   --  The frame key is composed of the frame address and the frame level.
   type Frame_Key_Type is record
      Addr  : MAT.Types.Target_Addr;
      Level : Natural;
   end record;
   function "<" (Left, Right : in Frame_Key_Type) return Boolean;

   --  Ordered map to collect event info statistics by <frame, level> pair.
   package Frame_Event_Info_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => Frame_Key_Type,
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

   type Frame_Info_Type is record
      Key  : Frame_Key_Type;
      Info : Event_Info_Type;
   end record;

   package Frame_Info_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Frame_Info_Type);
   subtype Frame_Info_Vector is Frame_Info_Vectors.Vector;
   subtype Frame_Info_Cursor is Frame_Info_Vectors.Cursor;

   --  Extract from the frame info map, the list of frame event info sorted
   --  on the frame level and slot size.
   procedure Build_Frame_Info (Map  : in Frame_Event_Info_Map;
                               List : in out Frame_Info_Vector);

end MAT.Events.Tools;
