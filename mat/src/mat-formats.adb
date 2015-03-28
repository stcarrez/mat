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

package body MAT.Formats is

   Hex_Prefix : Boolean := True;

   Conversion : constant String (1 .. 10) := "0123456789";

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
   --  Format the time relative to the start time.
   --  ------------------------------
   function Time (Value : in MAT.Types.Target_Tick_Ref;
                  Start : in MAT.Types.Target_Tick_Ref) return String is
      use type MAT.Types.Target_Tick_Ref;

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

   function Event (Item : in MAT.Events.Targets.Probe_Event_Type) return String is
   begin
      case Item.Index is
         when MAT.Events.Targets.MSG_MALLOC =>
            return Size (Item.Size) & " bytes allocated";

         when MAT.Events.Targets.MSG_REALLOC =>
            return Size (Item.Size) & " bytes reallocated";

         when MAT.Events.Targets.MSG_FREE =>
            return Size (Item.Size) & " bytes freed";

         when others =>
            return "unknown";

      end case;
   end Event;

   function Event_Malloc (Item    : in MAT.Events.Targets.Probe_Event_Type;
                          Related : in MAT.Events.Targets.Target_Event_Vector) return String is
      Iter : MAT.Events.Targets.Target_Event_Cursor := Related.First;
      Free_Event : MAT.Events.Targets.Probe_Event_Type;
   begin
      Free_Event := MAT.Events.Targets.Find (Related, MAT.Events.Targets.MSG_FREE);

      return Size (Item.Size) & " bytes allocated, " & Time (Free_Event.Time, Item.Time);

   exception
      when MAT.Events.Targets.Not_Found =>
         return Size (Item.Size) & " bytes allocated";

   end Event_Malloc;

   --  Format a short description of the event.
   function Event (Item    : in MAT.Events.Targets.Probe_Event_Type;
                   Related : in MAT.Events.Targets.Target_Event_Vector) return String is
   begin
      case Item.Index is
         when MAT.Events.Targets.MSG_MALLOC =>
            return Event_Malloc (Item, Related);

         when MAT.Events.Targets.MSG_REALLOC =>
            return Size (Item.Size) & " bytes reallocated";

         when MAT.Events.Targets.MSG_FREE =>
            return Size (Item.Size) & " bytes freed";

         when MAT.Events.Targets.MSG_BEGIN =>
            return "Begin event";

         when MAT.Events.Targets.MSG_END =>
            return "End event";

         when MAT.Events.Targets.MSG_LIBRARY =>
            return "Library information event";

         when others =>
            return "unknown";

      end case;
   end Event;

end MAT.Formats;
