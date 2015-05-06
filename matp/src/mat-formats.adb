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
with Util.Strings;
package body MAT.Formats is

   use type MAT.Types.Target_Tick_Ref;

   Hex_Prefix : Boolean := True;

   Conversion : constant String (1 .. 10) := "0123456789";

   function Location (File : in Ada.Strings.Unbounded.Unbounded_String) return String;

   function Event_Malloc (Item       : in MAT.Events.Target_Event_Type;
                          Related    : in MAT.Events.Tools.Target_Event_Vector;
                          Start_Time : in MAT.Types.Target_Tick_Ref) return String;

   function Event_Free (Item       : in MAT.Events.Target_Event_Type;
                        Related    : in MAT.Events.Tools.Target_Event_Vector;
                        Start_Time : in MAT.Types.Target_Tick_Ref) return String;

   --  ------------------------------
   --  Format the PID into a string.
   --  ------------------------------
   function Pid (Value : in MAT.Types.Target_Process_Ref) return String is
   begin
      return Util.Strings.Image (Natural (Value));
   end Pid;

   --  ------------------------------
   --  Format the address into a string.
   --  ------------------------------
   function Addr (Value : in MAT.Types.Target_Addr) return String is
      Hex : constant String := MAT.Types.Hex_Image (Value);
   begin
      if Hex_Prefix then
         return "0x" & Hex;
      else
         return Hex;
      end if;
   end Addr;

   --  ------------------------------
   --  Format the size into a string.
   --  ------------------------------
   function Size (Value : in MAT.Types.Target_Size) return String is
      Result : constant String := MAT.Types.Target_Size'Image (Value);
   begin
      if Result (Result'First) = ' ' then
         return Result (Result'First + 1 .. Result'Last);
      else
         return Result;
      end if;
   end Size;

   --  ------------------------------
   --  Format the memory growth size into a string.
   --  ------------------------------
   function Size (Alloced : in MAT.Types.Target_Size;
                  Freed   : in MAT.Types.Target_Size) return String is
      use type MAT.Types.Target_Size;
   begin
      if Alloced > Freed then
         return Size (Alloced - Freed);
      elsif Alloced < Freed then
         return "-" & Size (Freed - Alloced);
      else
         return "=" & Size (Alloced);
      end if;
   end Size;

   --  ------------------------------
   --  Format the time relative to the start time.
   --  ------------------------------
   function Time (Value : in MAT.Types.Target_Tick_Ref;
                  Start : in MAT.Types.Target_Tick_Ref) return String is
      T    : constant MAT.Types.Target_Tick_Ref := Value - Start;
      Sec  : constant MAT.Types.Target_Tick_Ref := T / 1_000_000;
      Usec : constant MAT.Types.Target_Tick_Ref := T mod 1_000_000;
      Msec : Natural := Natural (Usec / 1_000);
      Frac : String (1 .. 5);
   begin
      Frac (5) := 's';
      Frac (4) := Conversion (Msec mod 10 + 1);
      Msec := Msec / 10;
      Frac (3) := Conversion (Msec mod 10 + 1);
      Msec := Msec / 10;
      Frac (2) := Conversion (Msec mod 10 + 1);
      Frac (1) := '.';
      return MAT.Types.Target_Tick_Ref'Image (Sec) & Frac;
   end Time;

   --  ------------------------------
   --  Format the duration in seconds, milliseconds or microseconds.
   --  ------------------------------
   function Duration (Value : in MAT.Types.Target_Tick_Ref) return String is

      Sec  : constant MAT.Types.Target_Tick_Ref := Value / 1_000_000;
      Usec : constant MAT.Types.Target_Tick_Ref := Value mod 1_000_000;
      Msec : constant Natural := Natural (Usec / 1_000);
      Val  : Natural;
      Frac : String (1 .. 5);
   begin
      if Sec = 0 and Msec = 0 then
         return Util.Strings.Image (Integer (Usec)) & "us";
      elsif Sec = 0 then
         Val := Natural (Usec mod 1_000);
         Frac (5) := 's';
         Frac (4) := 'm';
         Frac (3) := Conversion (Val mod 10 + 1);
         Val := Val / 10;
         Frac (3) := Conversion (Val mod 10 + 1);
         Val := Val / 10;
         Frac (2) := Conversion (Val mod 10 + 1);
         Frac (1) := '.';
         return Util.Strings.Image (Integer (Msec)) & Frac;
      else
         Val := Msec;
         Frac (4) := 's';
         Frac (3) := Conversion (Val mod 10 + 1);
         Val := Val / 10;
         Frac (3) := Conversion (Val mod 10 + 1);
         Val := Val / 10;
         Frac (2) := Conversion (Val mod 10 + 1);
         Frac (1) := '.';
         return Util.Strings.Image (Integer (Sec)) & Frac (1 .. 4);
      end if;
   end Duration;

   function Location (File : in Ada.Strings.Unbounded.Unbounded_String) return String is
      Pos : constant Natural := Ada.Strings.Unbounded.Index (File, "/", Ada.Strings.Backward);
      Len : constant Natural := Ada.Strings.Unbounded.Length (File);
   begin
      if Pos /= 0 then
         return Ada.Strings.Unbounded.Slice (File, Pos + 1, Len);
      else
         return Ada.Strings.Unbounded.To_String (File);
      end if;
   end Location;

   --  ------------------------------
   --  Format a file, line, function information into a string.
   --  ------------------------------
   function Location (File : in Ada.Strings.Unbounded.Unbounded_String;
                      Line : in Natural;
                      Func : in Ada.Strings.Unbounded.Unbounded_String) return String is
   begin
      if Ada.Strings.Unbounded.Length (File) = 0 then
         return Ada.Strings.Unbounded.To_String (Func);
      elsif Line > 0 then
         declare
            Num : constant String := Natural'Image (Line);
         begin
            return Ada.Strings.Unbounded.To_String (Func) & " ("
              & Location (File) & ":" & Num (Num'First + 1 .. Num'Last) & ")";
         end;
      else
         return Ada.Strings.Unbounded.To_String (Func) & " (" & Location (File) & ")";
      end if;
   end Location;

   --  ------------------------------
   --  Format an event range description.
   --  ------------------------------
   function Event (First : in MAT.Events.Target_Event_Type;
                   Last  : in MAT.Events.Target_Event_Type) return String is
      use type MAT.Events.Event_Id_Type;

      Id1 : constant String := MAT.Events.Event_Id_Type'Image (First.Id);
      Id2 : constant String := MAT.Events.Event_Id_Type'Image (Last.Id);
   begin
      if First.Id = Last.Id then
         return Id1 (Id1'First + 1 .. Id1'Last);
      else
         return Id1 (Id1'First + 1 .. Id1'Last) & ".." & Id2 (Id2'First + 1 .. Id2'Last);
      end if;
   end Event;

   --  ------------------------------
   --  Format a short description of the event.
   --  ------------------------------
   function Event (Item : in MAT.Events.Target_Event_Type;
                   Mode : in Format_Type := NORMAL) return String is
      use type MAT.Types.Target_Addr;
   begin
      case Item.Index is
         when MAT.Events.MSG_MALLOC =>
            if Mode = BRIEF then
               return "malloc";
            else
               return "malloc(" & Size (Item.Size) & ") = " & Addr (Item.Addr);
            end if;

         when MAT.Events.MSG_REALLOC =>
            if Mode = BRIEF then
               if Item.Old_Addr = 0 then
                  return "realloc";
               else
                  return "realloc";
               end if;
            else
               if Item.Old_Addr = 0 then
                  return "realloc(0," & Size (Item.Size) & ") = "
                    & Addr (Item.Addr);
               else
                  return "realloc(" & Addr (Item.Old_Addr) & "," & Size (Item.Size) & ") = "
                    & Addr (Item.Addr);
               end if;
            end if;

         when MAT.Events.MSG_FREE =>
            if Mode = BRIEF then
               return "free";
            else
               return "free(" & Addr (Item.Addr) & "), " & Size (Item.Size);
            end if;

         when MAT.Events.MSG_BEGIN =>
            return "begin";

         when MAT.Events.MSG_END =>
            return "end";

         when MAT.Events.MSG_LIBRARY =>
            return "library";

      end case;
   end Event;

   function Event_Malloc (Item       : in MAT.Events.Target_Event_Type;
                          Related    : in MAT.Events.Tools.Target_Event_Vector;
                          Start_Time : in MAT.Types.Target_Tick_Ref) return String is
      Free_Event : MAT.Events.Target_Event_Type;
   begin
      Free_Event := MAT.Events.Tools.Find (Related, MAT.Events.MSG_FREE);

      return Size (Item.Size) & " bytes allocated after " & Duration (Item.Time - Start_Time)
        & ", freed " & Duration (Free_Event.Time - Item.Time)
        & " after by event" & MAT.Events.Event_Id_Type'Image (Free_Event.Id)
      ;

   exception
      when MAT.Events.Tools.Not_Found =>
         return Size (Item.Size) & " bytes allocated (never freed)";

   end Event_Malloc;

   function Event_Free (Item       : in MAT.Events.Target_Event_Type;
                        Related    : in MAT.Events.Tools.Target_Event_Vector;
                        Start_Time : in MAT.Types.Target_Tick_Ref) return String is
      Alloc_Event : MAT.Events.Target_Event_Type;
   begin
      Alloc_Event := MAT.Events.Tools.Find (Related, MAT.Events.MSG_MALLOC);

      return Size (Alloc_Event.Size) & " bytes freed after " & Duration (Item.Time - Start_Time)
        & ", alloc'ed for " & Duration (Item.Time - Alloc_Event.Time)
        & " by event" & MAT.Events.Event_Id_Type'Image (Alloc_Event.Id);

   exception
      when MAT.Events.Tools.Not_Found =>
         return Size (Item.Size) & " bytes freed";

   end Event_Free;

   --  ------------------------------
   --  Format a short description of the event.
   --  ------------------------------
   function Event (Item       : in MAT.Events.Target_Event_Type;
                   Related    : in MAT.Events.Tools.Target_Event_Vector;
                   Start_Time : in MAT.Types.Target_Tick_Ref) return String is
   begin
      case Item.Index is
         when MAT.Events.MSG_MALLOC =>
            return Event_Malloc (Item, Related, Start_Time);

         when MAT.Events.MSG_REALLOC =>
            return Size (Item.Size) & " bytes reallocated";

         when MAT.Events.MSG_FREE =>
            return Event_Free (Item, Related, Start_Time);

         when MAT.Events.MSG_BEGIN =>
            return "Begin event";

         when MAT.Events.MSG_END =>
            return "End event";

         when MAT.Events.MSG_LIBRARY =>
            return "Library information event";

      end case;
   end Event;

   --  ------------------------------
   --  Format a short description of the memory allocation slot.
   --  ------------------------------
   function Slot (Value      : in MAT.Types.Target_Addr;
                  Item       : in MAT.Memory.Allocation;
                  Start_Time : in MAT.Types.Target_Tick_Ref) return String is
   begin
      return Addr (Value) & " is " & Size (Item.Size)
        & " bytes allocated after " & Duration (Item.Time - Start_Time)
        & " by event" & MAT.Events.Event_Id_Type'Image (Item.Event);
   end Slot;

end MAT.Formats;
