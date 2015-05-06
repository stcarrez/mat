-----------------------------------------------------------------------
--  mat-formats - Format various types for the console or GUI interface
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
with Ada.Strings.Unbounded;

with MAT.Types;
with MAT.Memory;
with MAT.Events.Tools;
package MAT.Formats is

   type Format_Type is (BRIEF, NORMAL);

   --  Format the PID into a string.
   function Pid (Value : in MAT.Types.Target_Process_Ref) return String;

   --  Format the address into a string.
   function Addr (Value : in MAT.Types.Target_Addr) return String;

   --  Format the size into a string.
   function Size (Value : in MAT.Types.Target_Size) return String;

   --  Format the memory growth size into a string.
   function Size (Alloced : in MAT.Types.Target_Size;
                  Freed   : in MAT.Types.Target_Size) return String;

   --  Format the time relative to the start time.
   function Time (Value : in MAT.Types.Target_Tick_Ref;
                  Start : in MAT.Types.Target_Tick_Ref) return String;

   --  Format the duration in seconds, milliseconds or microseconds.
   function Duration (Value : in MAT.Types.Target_Tick_Ref) return String;

   --  Format a file, line, function information into a string.
   function Location (File : in Ada.Strings.Unbounded.Unbounded_String;
                      Line : in Natural;
                      Func : in Ada.Strings.Unbounded.Unbounded_String) return String;

   --  Format an event range description.
   function Event (First : in MAT.Events.Target_Event_Type;
                   Last  : in MAT.Events.Target_Event_Type) return String;

   --  Format a short description of the event.
   function Event (Item : in MAT.Events.Target_Event_Type;
                   Mode : in Format_Type := NORMAL) return String;

   --  Format a short description of the event.
   function Event (Item       : in MAT.Events.Target_Event_Type;
                   Related    : in MAT.Events.Tools.Target_Event_Vector;
                   Start_Time : in MAT.Types.Target_Tick_Ref) return String;

   --  Format a short description of the memory allocation slot.
   function Slot (Value      : in MAT.Types.Target_Addr;
                  Item       : in MAT.Memory.Allocation;
                  Start_Time : in MAT.Types.Target_Tick_Ref) return String;

end MAT.Formats;
