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
package body MAT.Events.Tools is

   --  ------------------------------
   --  Find in the list the first event with the given type.
   --  Raise <tt>Not_Found</tt> if the list does not contain such event.
   --  ------------------------------
   function Find (List : in Target_Event_Vector;
                  Kind : in Probe_Index_Type) return Target_Event_Type is
      Iter  : Target_Event_Cursor := List.First;
      Event : Target_Event_Type;
   begin
      while Target_Event_Vectors.Has_Element (Iter) loop
         Event := Target_Event_Vectors.Element (Iter);
         if Event.Index = Kind then
            return Event;
         end if;
         Target_Event_Vectors.Next (Iter);
      end loop;
      raise Not_Found;
   end Find;

   function "<" (Left, Right : in Frame_Key_Type) return Boolean is
   begin
      if Left.Addr < Right.Addr then
         return True;
      elsif Left.Addr > Right.Addr then
         return False;
      else
         return Left.Level < Right.Level;
      end if;
   end "<";

   --  ------------------------------
   --  Extract from the frame info map, the list of event info sorted on the count.
   --  ------------------------------
   procedure Build_Event_Info (Map  : in Frame_Event_Info_Map;
                               List : in out Event_Info_Vector) is

      function "<" (Left, Right : in Event_Info_Type) return Boolean is
      begin
         return Left.Count < Right.Count;
      end "<";

      package Sort_Event_Info is new Event_Info_Vectors.Generic_Sorting;
      Iter : Frame_Event_Info_Cursor := Map.First;
      Item : Event_Info_Type;
   begin
      while Frame_Event_Info_Maps.Has_Element (Iter) loop
         Item := Frame_Event_Info_Maps.Element (Iter);
         List.Append (Item);
         Frame_Event_Info_Maps.Next (Iter);
      end loop;
      Sort_Event_Info.Sort (List);
   end Build_Event_Info;

end MAT.Events.Tools;
