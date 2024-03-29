-----------------------------------------------------------------------
--  mat-formats - Format various types for the console or GUI interface
--  Copyright (C) 2015, 2021, 2023 Stephane Carrez
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

   Hex_Prefix : constant Boolean := True;

   Hex_Length : Positive := 16;

   Conversion : constant String (1 .. 10) := "0123456789";

   function Location (File : in Ada.Strings.Unbounded.Unbounded_String) return String;

   --  Format a short description of a malloc event.
   function Event_Malloc (Item       : in MAT.Events.Target_Event_Type;
                          Related    : in MAT.Events.Tools.Target_Event_Vector;
                          Start_Time : in MAT.Types.Target_Tick_Ref) return String;

   --  Format a short description of a realloc event.
   function Event_Realloc (Item       : in MAT.Events.Target_Event_Type;
                           Related    : in MAT.Events.Tools.Target_Event_Vector;
                           Start_Time : in MAT.Types.Target_Tick_Ref) return String;

   --  Format a short description of a free event.
   function Event_Free (Item       : in MAT.Events.Target_Event_Type;
                        Related    : in MAT.Events.Tools.Target_Event_Vector;
                        Start_Time : in MAT.Types.Target_Tick_Ref) return String;

   --  Format a short description of a secondary stack mark event.
   function Event_Secondary_Mark (Item       : in MAT.Events.Target_Event_Type;
                                  Related    : in MAT.Events.Tools.Target_Event_Vector;
                                  Start_Time : in MAT.Types.Target_Tick_Ref) return String;

   --  Format a short description of a secondary allocation.
   function Event_Secondary_Allocate (Item       : in MAT.Events.Target_Event_Type;
                                      Related    : in MAT.Events.Tools.Target_Event_Vector;
                                      Start_Time : in MAT.Types.Target_Tick_Ref) return String;

   --  Format a short description of a secondary stack mark event.
   function Event_Secondary_Release (Item       : in MAT.Events.Target_Event_Type;
                                     Related    : in MAT.Events.Tools.Target_Event_Vector;
                                     Start_Time : in MAT.Types.Target_Tick_Ref) return String;

   --  ------------------------------
   --  Set the size of a target address to format them.
   --  ------------------------------
   procedure Set_Address_Size (Size : in Positive) is
   begin
      Hex_Length := Size;
   end Set_Address_Size;

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
      Hex : constant String := MAT.Types.Hex_Image (Value, Hex_Length);
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
         return "+" & Size (Alloced - Freed);
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

         when MAT.Events.MSG_SECONDARY_STACK_MARK =>
            if Mode = BRIEF then
               return "smark";
            else
               return "smark(" & Addr (Item.Addr) & "), " & Size (Item.Size);
            end if;

         when MAT.Events.MSG_SECONDARY_STACK_ALLOC =>
            if Mode = BRIEF then
               return "salloc";
            else
               return "salloc(" & Size (Item.Size) & ")";
            end if;

         when MAT.Events.MSG_SECONDARY_STACK_RELEASE =>
            if Mode = BRIEF then
               return "srelease";
            else
               return "srelease(" & Addr (Item.Addr) & "), " & Size (Item.Size);
            end if;

         when MAT.Events.MSG_BEGIN =>
            return "begin";

         when MAT.Events.MSG_END =>
            return "end";

         when MAT.Events.MSG_LIBRARY =>
            return "library";

      end case;
   end Event;

   --  ------------------------------
   --  Format a short description of a malloc event.
   --  ------------------------------
   function Event_Malloc (Item       : in MAT.Events.Target_Event_Type;
                          Related    : in MAT.Events.Tools.Target_Event_Vector;
                          Start_Time : in MAT.Types.Target_Tick_Ref) return String is
      Free_Event : MAT.Events.Target_Event_Type;
      Slot_Addr  : constant String := Addr (Item.Addr);
   begin
      Free_Event := MAT.Events.Tools.Find (Related, MAT.Events.MSG_FREE);

      return Size (Item.Size) & " bytes allocated at "
        & Slot_Addr
        & " after " & Duration (Item.Time - Start_Time)
        & ", freed " & Duration (Free_Event.Time - Item.Time)
        & " after by event" & MAT.Events.Event_Id_Type'Image (Free_Event.Id)
      ;

   exception
      when MAT.Events.Tools.Not_Found =>
         return Size (Item.Size) & " bytes allocated at " & Slot_Addr & " (never freed)";

   end Event_Malloc;

   --  ------------------------------
   --  Format a short description of a realloc event.
   --  ------------------------------
   function Event_Realloc (Item       : in MAT.Events.Target_Event_Type;
                           Related    : in MAT.Events.Tools.Target_Event_Vector;
                           Start_Time : in MAT.Types.Target_Tick_Ref) return String is
      use type MAT.Events.Event_Id_Type;
      Free_Event : MAT.Events.Target_Event_Type;
      Slot_Addr  : constant String := Addr (Item.Addr);
   begin
      if Item.Next_Id = 0 and Item.Prev_Id = 0 then
         return Size (Item.Size) & " bytes reallocated at " & Slot_Addr
           & " after " & Duration (Item.Time - Start_Time)
           & " (never freed)";
      end if;

      Free_Event := MAT.Events.Tools.Find (Related, MAT.Events.MSG_FREE);
      return Size (Item.Size) & " bytes reallocated at " & Slot_Addr
        & " after " & Duration (Item.Time - Start_Time)
        & ", freed " & Duration (Free_Event.Time - Item.Time)
        & " after by event" & MAT.Events.Event_Id_Type'Image (Free_Event.Id)
        & " " & Size (Item.Size, Item.Old_Size) & " bytes";

   exception
      when MAT.Events.Tools.Not_Found =>
         return Size (Item.Size) & " bytes reallocated at " & Slot_Addr
           & " after " & Duration (Item.Time - Start_Time)
           & " (never freed) " & Size (Item.Size, Item.Old_Size) & " bytes";

   end Event_Realloc;

   --  ------------------------------
   --  Format a short description of a free event.
   --  ------------------------------
   function Event_Free (Item       : in MAT.Events.Target_Event_Type;
                        Related    : in MAT.Events.Tools.Target_Event_Vector;
                        Start_Time : in MAT.Types.Target_Tick_Ref) return String is
      Alloc_Event : MAT.Events.Target_Event_Type;
      Slot_Addr   : constant String := Addr (Item.Addr);
   begin
      Alloc_Event := MAT.Events.Tools.Find (Related, MAT.Events.MSG_MALLOC);

      return Size (Alloc_Event.Size) & " bytes freed at " & Slot_Addr
        & " after " & Duration (Item.Time - Start_Time)
        & ", alloc'ed for " & Duration (Item.Time - Alloc_Event.Time)
        & " by event" & MAT.Events.Event_Id_Type'Image (Alloc_Event.Id);

   exception
      when MAT.Events.Tools.Not_Found =>
         return Size (Item.Size) & " bytes freed at " & Slot_Addr;

   end Event_Free;

   --  ------------------------------
   --  Format a short description of a secondary stack mark event.
   --  ------------------------------
   function Event_Secondary_Mark (Item       : in MAT.Events.Target_Event_Type;
                                  Related    : in MAT.Events.Tools.Target_Event_Vector;
                                  Start_Time : in MAT.Types.Target_Tick_Ref) return String is
      Release_Event : MAT.Events.Target_Event_Type;
      Mark_Addr     : constant String := Addr (Item.Addr);
   begin
      Release_Event := MAT.Events.Tools.Find (Related, MAT.Events.MSG_SECONDARY_STACK_RELEASE);
      return "smark at stack "
        & Mark_Addr
        & ", released " & Duration (Release_Event.Time - Item.Time)
        & " after by event" & MAT.Events.Event_Id_Type'Image (Release_Event.Id)
        & ", mark size " & Size (Item.Size) & " release size " & Size (Release_Event.Size)
        & " after " & Duration (Item.Time - Start_Time);

   exception
      when MAT.Events.Tools.Not_Found =>
         return "smark at stack "
           & Mark_Addr
           & ", not released "
           & " mark size " & Size (Item.Size)
           & " after " & Duration (Item.Time - Start_Time);
   end Event_Secondary_Mark;

   --  ------------------------------
   --  Format a short description of a secondary stack allocation event.
   --  ------------------------------
   function Event_Secondary_Allocate (Item       : in MAT.Events.Target_Event_Type;
                                      Related    : in MAT.Events.Tools.Target_Event_Vector;
                                      Start_Time : in MAT.Types.Target_Tick_Ref) return String is
      Mark_Event : MAT.Events.Target_Event_Type;
      Slot_Addr  : constant String := Addr (Item.Addr);
   begin
      Mark_Event := MAT.Events.Tools.Find (Related, MAT.Events.MSG_SECONDARY_STACK_MARK);
      return Size (Item.Size) & " bytes allocated in secondary stack (from mark"
        & MAT.Events.Event_Id_Type'Image (Mark_Event.Id)
        & ") at "
        & Slot_Addr
        & " after " & Duration (Item.Time - Start_Time);

   exception
      when MAT.Events.Tools.Not_Found =>
         return Size (Item.Size) & " bytes allocated in secondary stack at "
           & Slot_Addr
           & " after " & Duration (Item.Time - Start_Time);
   end Event_Secondary_Allocate;

   --  ------------------------------
   --  Format a short description of a secondary stack mark event.
   --  ------------------------------
   function Event_Secondary_Release (Item       : in MAT.Events.Target_Event_Type;
                                     Related    : in MAT.Events.Tools.Target_Event_Vector;
                                     Start_Time : in MAT.Types.Target_Tick_Ref) return String is
      Mark_Event : MAT.Events.Target_Event_Type;
      Mark_Addr  : constant String := Addr (Item.Addr);
   begin
      Mark_Event := MAT.Events.Tools.Find (Related, MAT.Events.MSG_SECONDARY_STACK_MARK);
      return "srelease at stack "
        & Mark_Addr
        & ", marked " & Duration (Item.Time - Mark_Event.Time)
        & " before by event" & MAT.Events.Event_Id_Type'Image (Mark_Event.Id)
        & ", mark size " & Size (Mark_Event.Size) & " release size " & Size (Item.Size)
        & " after " & Duration (Item.Time - Start_Time);

   exception
      when MAT.Events.Tools.Not_Found =>
         return "srelease at stack "
           & Mark_Addr
           & ", not marked "
           & " release size " & Size (Item.Size)
           & " after " & Duration (Item.Time - Start_Time);
   end Event_Secondary_Release;

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
            return Event_Realloc (Item, Related, Start_Time);

         when MAT.Events.MSG_FREE =>
            return Event_Free (Item, Related, Start_Time);

         when MAT.Events.MSG_SECONDARY_STACK_MARK =>
            return Event_Secondary_Mark (Item, Related, Start_Time);

         when MAT.Events.MSG_SECONDARY_STACK_ALLOC =>
            return Event_Secondary_Allocate (Item, Related, Start_Time);

         when MAT.Events.MSG_SECONDARY_STACK_RELEASE =>
            return Event_Secondary_Release (Item, Related, Start_Time);

         when MAT.Events.MSG_BEGIN =>
            return "Begin event";

         when MAT.Events.MSG_END =>
            return "End event";

         when MAT.Events.MSG_LIBRARY =>
            return "Library information event";

      end case;
   end Event;

   --  ------------------------------
   --  Format the difference between two event IDs (offset).
   --  ------------------------------
   function Offset (First  : in MAT.Events.Event_Id_Type;
                    Second : in MAT.Events.Event_Id_Type) return String is
      use type MAT.Events.Event_Id_Type;
   begin
      if First = Second or First = 0 or Second = 0 then
         return "";
      elsif First > Second then
         return "+" & Util.Strings.Image (Natural (First - Second));
      else
         return "-" & Util.Strings.Image (Natural (Second - First));
      end if;
   end Offset;

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
