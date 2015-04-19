-----------------------------------------------------------------------
--  mat-interp -- Command interpreter
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
with Util.Strings;
with Util.Log.Loggers;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with GNAT.Command_Line;

with Bfd;

with ELF;
with MAT.Types;
with MAT.Readers.Streams.Files;
with MAT.Memory.Tools;
with MAT.Memory.Targets;
with MAT.Symbols.Targets;
with MAT.Expressions;
with MAT.Frames;
with MAT.Events.Targets;
with MAT.Consoles;
with MAT.Formats;
with MAT.Events.Timelines;
package body MAT.Commands is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Commands");

   function Get_Command (Line : in String) return String;
   procedure Slot_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);
   procedure Frames_Command (Target : in out MAT.Targets.Target_Type'Class;
                             Args   : in String);
   procedure Exit_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);
   procedure Open_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);
   procedure Help_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);
   procedure Event_Sizes_Command (Target : in out MAT.Targets.Target_Type'Class;
                                  Args   : in String);

   --  Maps command to dump the memory maps of the program.
   procedure Maps_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);

   --  Print the stack frame description in the console.
   procedure Print_Frame (Console : in MAT.Consoles.Console_Access;
                          Frame   : in MAT.Frames.Frame_Type;
                          Symbols : in MAT.Symbols.Targets.Target_Symbols_Ref);

   package Command_Map is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Command_Handler,
                                                Equivalent_Keys => "=",
                                                Hash            => Ada.Strings.Hash);

   Commands : Command_Map.Map;

   --  ------------------------------
   --  Print the stack frame description in the console.
   --  ------------------------------
   procedure Print_Frame (Console : in MAT.Consoles.Console_Access;
                          Frame   : in MAT.Frames.Frame_Type;
                          Symbols : in MAT.Symbols.Targets.Target_Symbols_Ref) is
      Backtrace : constant MAT.Frames.Frame_Table := MAT.Frames.Backtrace (Frame);

      Symbol    : MAT.Symbols.Targets.Symbol_Info;
   begin
      for I in Backtrace'Range loop
         Console.Start_Row;
         Console.Print_Field (MAT.Consoles.F_FRAME_ID, I, MAT.Consoles.J_RIGHT);
         Console.Print_Field (MAT.Consoles.F_FRAME_ADDR, Backtrace (I));
         MAT.Symbols.Targets.Find_Nearest_Line (Symbols => Symbols.Value.all,
                                                Addr    => Backtrace (I),
                                                Symbol  => Symbol);
         Console.Print_Field (MAT.Consoles.F_FUNCTION_NAME,
                              MAT.Formats.Location (Symbol.File, Symbol.Line, Symbol.Name),
                              MAT.Consoles.J_RIGHT_NO_FILL);
         Console.End_Row;
      end loop;
   end Print_Frame;

   --  ------------------------------
   --  Sizes command.
   --  Collect statistics about the used memory slots and report the different slot
   --  sizes with count.
   --  ------------------------------
   procedure Slot_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String) is
      procedure Print (Addr : in MAT.Types.Target_Addr;
                       Slot : in MAT.Memory.Allocation);

      Slots   : MAT.Memory.Allocation_Map;
      Iter    : MAT.Memory.Allocation_Cursor;
      Symbols : constant MAT.Symbols.Targets.Target_Symbols_Ref := Target.Process.Symbols;
      Console : constant MAT.Consoles.Console_Access := Target.Console;

      procedure Print (Addr : in MAT.Types.Target_Addr;
                       Slot : in MAT.Memory.Allocation) is
         use type MAT.Frames.Frame_Type;
         Backtrace : constant MAT.Frames.Frame_Table := MAT.Frames.Backtrace (Slot.Frame);

         Symbol : MAT.Symbols.Targets.Symbol_Info;
      begin
         Ada.Text_IO.Put (MAT.Types.Hex_Image (Addr));
         Ada.Text_IO.Set_Col (14);
         Ada.Text_IO.Put (MAT.Types.Target_Size'Image (Slot.Size));
         Ada.Text_IO.Set_Col (30);
         Ada.Text_IO.Put (MAT.Types.Target_Thread_Ref'Image (Slot.Thread));
         Ada.Text_IO.Set_Col (50);
         Ada.Text_IO.Put (MAT.Types.Target_Tick_Ref'Image (Slot.Time));
         Ada.Text_IO.New_Line;
         for I in Backtrace'Range loop
            Console.Start_Row;
            Console.Print_Field (Consoles.F_ID, I);
            Console.Print_Field (Consoles.F_ADDR, MAT.Formats.Addr (Backtrace (I)));
            MAT.Symbols.Targets.Find_Nearest_Line (Symbols => Symbols.Value.all,
                                                   Addr    => Backtrace (I),
                                                   Symbol  => Symbol);
            Console.Print_Field (Consoles.F_FUNCTION_NAME,
                                 MAT.Formats.Location (Symbol.File, Symbol.Line, Symbol.Name));
            Console.End_Row;
         end loop;
      end Print;

      Filter  : MAT.Expressions.Expression_Type;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
   begin
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_ID, "Id", 4);
      Console.Print_Title (MAT.Consoles.F_ADDR, "Address", 22);
      Console.Print_Title (MAT.Consoles.F_FUNCTION_NAME, "Function", 80);
      Console.End_Title;

      Filter := MAT.Expressions.Parse (Args);
      Process.Memory.Find (From   => MAT.Types.Target_Addr'First,
                           To     => MAT.Types.Target_Addr'Last,
                           Filter => Filter,
                           Into   => Slots);
      Iter := Slots.First;
      while MAT.Memory.Allocation_Maps.Has_Element (Iter) loop
         MAT.Memory.Allocation_Maps.Query_Element (Iter, Print'Access);
         MAT.Memory.Allocation_Maps.Next (Iter);
      end loop;

   exception
      when E : others =>
         Log.Error ("Exception when evaluating " & Args, E);
         Target.Console.Error ("Invalid selection");
   end Slot_Command;

   --  ------------------------------
   --  Sizes command.
   --  Collect statistics about the used memory slots and report the different slot
   --  sizes with count.
   --  ------------------------------
   procedure Sizes_Command (Target : in out MAT.Targets.Target_Type'Class;
                            Args   : in String) is
      pragma Unreferenced (Args);

      Sizes   : MAT.Memory.Tools.Size_Info_Map;
      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Iter    : MAT.Memory.Tools.Size_Info_Cursor;
   begin
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_SIZE, "Slot size", 25);
      Console.Print_Title (MAT.Consoles.F_COUNT, "Count", 15);
      Console.Print_Title (MAT.Consoles.F_TOTAL_SIZE, "Total size", 15);
      Console.End_Title;

      MAT.Memory.Targets.Size_Information (Memory => Process.Memory,
                                           Sizes  => Sizes);
      Iter := Sizes.First;
      while MAT.Memory.Tools.Size_Info_Maps.Has_Element (Iter) loop
         declare
            use MAT.Memory.Tools;
            use type MAT.Types.Target_Size;

            Size  : constant MAT.Types.Target_Size := MAT.Memory.Tools.Size_Info_Maps.Key (Iter);
            Info  : constant Size_Info_Type := Memory.Tools.Size_Info_Maps.Element (Iter);
            Total : constant MAT.Types.Target_Size := Size * MAT.Types.Target_Size (Info.Count);
         begin
            Console.Start_Row;
            Console.Print_Size (MAT.Consoles.F_SIZE, Size);
            Console.Print_Field (MAT.Consoles.F_COUNT, Info.Count);
            Console.Print_Size (MAT.Consoles.F_TOTAL_SIZE, Total);
            Console.End_Row;
         end;
         MAT.Memory.Tools.Size_Info_Maps.Next (Iter);
      end loop;
   end Sizes_Command;

   --  ------------------------------
   --  Threads command.
   --  Collect statistics about the threads and their allocation.
   --  ------------------------------
   procedure Threads_Command (Target : in out MAT.Targets.Target_Type'Class;
                              Args   : in String) is
      pragma Unreferenced (Args);

      Threads : MAT.Memory.Memory_Info_Map;
      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Iter    : MAT.Memory.Memory_Info_Cursor;
   begin
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_THREAD, "Thread", 10);
      Console.Print_Title (MAT.Consoles.F_COUNT, "# Allocation", 12);
      Console.Print_Title (MAT.Consoles.F_TOTAL_SIZE, "Total size", 15);
      Console.Print_Title (MAT.Consoles.F_MIN_SIZE, "Min slot size", 15);
      Console.Print_Title (MAT.Consoles.F_MAX_SIZE, "Max slot size", 15);
      Console.Print_Title (MAT.Consoles.F_MIN_ADDR, "Low address", 15);
      Console.Print_Title (MAT.Consoles.F_MAX_ADDR, "High address", 15);
      Console.End_Title;

      MAT.Memory.Targets.Thread_Information (Memory  => Process.Memory,
                                             Threads => Threads);
      Iter := Threads.First;
      while MAT.Memory.Memory_Info_Maps.Has_Element (Iter) loop
         declare
            use type MAT.Types.Target_Size;

            Thread : constant Types.Target_Thread_Ref := MAT.Memory.Memory_Info_Maps.Key (Iter);
            Info   : constant Memory.Memory_Info := MAT.Memory.Memory_Info_Maps.Element (Iter);
         begin
            Console.Start_Row;
            Console.Print_Thread (MAT.Consoles.F_THREAD, Thread);
            Console.Print_Field (MAT.Consoles.F_COUNT, Info.Alloc_Count);
            Console.Print_Size (MAT.Consoles.F_TOTAL_SIZE, Info.Total_Size);
            Console.Print_Size (MAT.Consoles.F_MIN_SIZE, Info.Min_Slot_Size);
            Console.Print_Size (MAT.Consoles.F_MAX_SIZE, Info.Max_Slot_Size);
            Console.Print_Field (MAT.Consoles.F_MIN_ADDR, Info.Min_Addr);
            Console.Print_Field (MAT.Consoles.F_MAX_ADDR, Info.Max_Addr);
            Console.End_Row;
         end;
         MAT.Memory.Memory_Info_Maps.Next (Iter);
      end loop;
   end Threads_Command;

   --  ------------------------------
   --  Frames command.
   --  Collect statistics about the frames and their allocation.
   --  ------------------------------
   procedure Frames_Command (Target : in out MAT.Targets.Target_Type'Class;
                             Args   : in String) is
      Frames  : MAT.Memory.Frame_Info_Map;
      Level   : Positive := 3;
      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Iter    : MAT.Memory.Frame_Info_Cursor;
   begin
      if Args'Length > 0 then
         Level := Positive'Value (Args);
      end if;
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_FUNCTION_NAME, "Function", 20);
      Console.Print_Title (MAT.Consoles.F_COUNT, "# Slots", 10);
      Console.Print_Title (MAT.Consoles.F_TOTAL_SIZE, "Total size", 12);
      Console.Print_Title (MAT.Consoles.F_MIN_SIZE, "Min size", 10);
      Console.Print_Title (MAT.Consoles.F_MAX_SIZE, "Max size", 10);
      Console.Print_Title (MAT.Consoles.F_MIN_ADDR, "Low addr", 10);
      Console.Print_Title (MAT.Consoles.F_MAX_ADDR, "High addr", 10);
      Console.End_Title;

      MAT.Memory.Targets.Frame_Information (Memory => Process.Memory,
                                            Level  => Level,
                                            Frames => Frames);
      Iter := Frames.First;
      while MAT.Memory.Frame_Info_Maps.Has_Element (Iter) loop
         declare
            use type MAT.Types.Target_Size;

            Func   : constant Types.Target_Addr := MAT.Memory.Frame_Info_Maps.Key (Iter);
            Info   : constant Memory.Frame_Info := MAT.Memory.Frame_Info_Maps.Element (Iter);
            Symbol : MAT.Symbols.Targets.Symbol_Info;
         begin
            MAT.Symbols.Targets.Find_Nearest_Line (Symbols => Process.Symbols.Value.all,
                                                   Addr    => Func,
                                                   Symbol  => Symbol);
            Console.Start_Row;
            Console.Print_Field (MAT.Consoles.F_FUNCTION_NAME,
                                 MAT.Formats.Location (Symbol.File, Symbol.Line, Symbol.Name));
            Console.Print_Field (MAT.Consoles.F_COUNT, Info.Memory.Alloc_Count);
            Console.Print_Size (MAT.Consoles.F_TOTAL_SIZE, Info.Memory.Total_Size);
            Console.Print_Size (MAT.Consoles.F_MIN_SIZE, Info.Memory.Min_Slot_Size);
            Console.Print_Size (MAT.Consoles.F_MAX_SIZE, Info.Memory.Max_Slot_Size);
            Console.Print_Field (MAT.Consoles.F_MIN_ADDR, Info.Memory.Min_Addr);
            Console.Print_Field (MAT.Consoles.F_MAX_ADDR, Info.Memory.Max_Addr);
            Console.End_Row;
         end;
         MAT.Memory.Frame_Info_Maps.Next (Iter);
      end loop;
   end Frames_Command;

   --  ------------------------------
   --  Event size command.
   --  Print the size used by malloc/realloc events.
   --  ------------------------------
   procedure Event_Frames_Command (Target : in out MAT.Targets.Target_Type'Class;
                                  Args   : in String) is
      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Frames  : MAT.Events.Targets.Frame_Event_Info_Map;
      List    : MAT.Events.Targets.Event_Info_Vector;
      Iter    : MAT.Events.Targets.Event_Info_Cursor;
      Filter  : MAT.Expressions.Expression_Type;
      Depth   : Natural := 3;
      Symbol  : MAT.Symbols.Targets.Symbol_Info;
      Info    : MAT.Events.Targets.Event_Info_Type;
   begin
      if Args'Length > 0 then
         Filter := MAT.Expressions.Parse (Args);
      end if;
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_COUNT, "Count", 8);
      Console.Print_Title (MAT.Consoles.F_EVENT, "Event", 9);
      Console.Print_Title (MAT.Consoles.F_SIZE, "Size", 10);
      Console.Print_Title (MAT.Consoles.F_FUNCTION_NAME, "Function", 50);
      Console.Print_Title (MAT.Consoles.F_ID, "Id", 40);
      Console.End_Title;

      MAT.Events.Timelines.Find_Frames (Target => Process.Events.all,
                                        Filter => Filter,
                                        Depth  => Depth,
                                        Frames => Frames);
      MAT.Events.Targets.Build_Event_Info (Frames, List);
      Iter := List.First;
      while MAT.Events.Targets.Event_Info_Vectors.Has_Element (Iter) loop
         Info := MAT.Events.Targets.Event_Info_Vectors.Element (Iter);
         MAT.Symbols.Targets.Find_Nearest_Line (Symbols => Process.Symbols.Value.all,
                                                Addr    => Info.Frame_Addr,
                                                Symbol  => Symbol);

         Console.Start_Row;
         Console.Print_Field (MAT.Consoles.F_COUNT, Natural'Image (Info.Count));
         Console.Print_Field (MAT.Consoles.F_EVENT,
                              MAT.Formats.Event (Info.First_Event, MAT.Formats.BRIEF));
         Console.Print_Size (MAT.Consoles.F_SIZE, Info.First_Event.Size);
         Console.Print_Field (MAT.Consoles.F_FUNCTION_NAME,
                              MAT.Formats.Location (Symbol.File, Symbol.Line, Symbol.Name));
         Console.Print_Field (MAT.Consoles.F_ID,
                              MAT.Formats.Event (Info.First_Event, Info.Last_Event));
         Console.End_Row;
         MAT.Events.Targets.Event_Info_Vectors.Next (Iter);
      end loop;

   exception
      when E : others =>
         Log.Error ("Exception when evaluating " & Args, E);
         Target.Console.Error ("Invalid selection");
   end Event_Frames_Command;

   --  ------------------------------
   --  Event size command.
   --  Print the size used by malloc/realloc events.
   --  ------------------------------
   procedure Event_Sizes_Command (Target : in out MAT.Targets.Target_Type'Class;
                                  Args   : in String) is
      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Start, Finish : MAT.Types.Target_Tick_Ref;
      Sizes   : MAT.Events.Targets.Size_Event_Info_Map;
      Filter  : MAT.Expressions.Expression_Type;
      Iter    : MAT.Events.Targets.Size_Event_Info_Cursor;
   begin
      if Args'Length > 0 then
         Filter := MAT.Expressions.Parse (Args);
      end if;
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_ID, "Event Id range", 30);
      Console.Print_Title (MAT.Consoles.F_TIME, "Time", 10);
      Console.Print_Title (MAT.Consoles.F_EVENT, "Event", 20);
      Console.Print_Title (MAT.Consoles.F_SIZE, "Size", 12);
      Console.Print_Title (MAT.Consoles.F_COUNT, "Count", 8);
      Console.End_Title;

      Process.Events.Get_Time_Range (Start, Finish);
      MAT.Events.Timelines.Find_Sizes (Target => Process.Events.all,
                                       Filter => Filter,
                                       Sizes  => Sizes);
      Iter := Sizes.First;
      while MAT.Events.Targets.Size_Event_Info_Maps.Has_Element (Iter) loop
         declare
            use type MAT.Types.Target_Tick_Ref;

            Size  : constant MAT.Types.Target_Size
              := MAT.Events.Targets.Size_Event_Info_Maps.Key (Iter);
            Info  : constant MAT.Events.Targets.Event_Info_Type
              := MAT.Events.Targets.Size_Event_Info_Maps.Element (Iter);
            Time  : constant MAT.Types.Target_Tick_Ref := Info.First_Event.Time - Start;
         begin
            Console.Start_Row;
            Console.Print_Field (MAT.Consoles.F_ID,
                                 MAT.Formats.Event (Info.First_Event, Info.Last_Event));
            Console.Print_Duration (MAT.Consoles.F_TIME, Time);
            Console.Print_Field (MAT.Consoles.F_EVENT,
                                 MAT.Formats.Event (Info.First_Event, MAT.Formats.BRIEF));
            Console.Print_Size (MAT.Consoles.F_SIZE, Size);
            Console.Print_Field (MAT.Consoles.F_COUNT, Natural'Image (Info.Count));
            Console.End_Row;
         end;
         MAT.Events.Targets.Size_Event_Info_Maps.Next (Iter);
      end loop;

   exception
      when E : others =>
         Log.Error ("Exception when evaluating " & Args, E);
         Target.Console.Error ("Invalid selection");
   end Event_Sizes_Command;

   --  ------------------------------
   --  Events command.
   --  Print the probe events.
   --  ------------------------------
   procedure Events_Command (Target : in out MAT.Targets.Target_Type'Class;
                             Args   : in String) is
      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Start, Finish : MAT.Types.Target_Tick_Ref;
      Events  : MAT.Events.Targets.Target_Event_Vector;
      Filter  : MAT.Expressions.Expression_Type;
      Iter    : MAT.Events.Targets.Target_Event_Cursor;
   begin
      Filter := MAT.Expressions.Parse (Args);
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_ID, "Id", 10);
      Console.Print_Title (MAT.Consoles.F_TIME, "Time", 10);
      Console.Print_Title (MAT.Consoles.F_EVENT, "Event", 60);
      Console.End_Title;

      Process.Events.Get_Time_Range (Start, Finish);
      Process.Events.Get_Events (Start, Finish, Events);
      Iter := Events.First;
      while MAT.Events.Targets.Target_Event_Vectors.Has_Element (Iter) loop
         declare
            use type MAT.Types.Target_Tick_Ref;

            Event : constant MAT.Events.Targets.Probe_Event_Type
              := MAT.Events.Targets.Target_Event_Vectors.Element (Iter);
            Time  : constant MAT.Types.Target_Tick_Ref := Event.Time - Start;
         begin
            if Filter.Is_Selected (Event) then
               Console.Start_Row;
               Console.Print_Field (MAT.Consoles.F_ID,
                                    MAT.Events.Targets.Event_Id_Type'Image (Event.Id));
               Console.Print_Duration (MAT.Consoles.F_TIME, Time);
               Console.Print_Field (MAT.Consoles.F_EVENT, MAT.Formats.Event (Event));
               Console.End_Row;
            end if;
         end;
         MAT.Events.Targets.Target_Event_Vectors.Next (Iter);
      end loop;

   exception
      when E : others =>
         Log.Error ("Exception when evaluating " & Args, E);
         Target.Console.Error ("Invalid selection");
   end Events_Command;

   --  ------------------------------
   --  Event command.
   --  Print the probe event with the stack frame.
   --  ------------------------------
   procedure Event_Command (Target : in out MAT.Targets.Target_Type'Class;
                            Args   : in String) is
      use Consoles;
      use type MAT.Types.Target_Tick_Ref;

      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Id      : MAT.Events.Targets.Event_Id_Type;
      Event   : MAT.Events.Targets.Probe_Event_Type;
      Start, Finish : MAT.Types.Target_Tick_Ref;
      Related : MAT.Events.Targets.Target_Event_Vector;
   begin
      Id := MAT.Events.Targets.Event_Id_Type'Value (Args);
      Event := Process.Events.Get_Event (Id);
      MAT.Events.Timelines.Find_Related (Process.Events.all, Event, 10, Related);
      Process.Events.Get_Time_Range (Start, Finish);

      Console.Notice (N_EVENT_ID, MAT.Formats.Event (Event, Related, Start));
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_FRAME_ID, "Id", 3);
      Console.Print_Title (MAT.Consoles.F_FRAME_ADDR, "Frame Address", 22);
      Console.Print_Title (MAT.Consoles.F_FUNCTION_NAME, "Function", 80);
      Console.End_Title;

      Print_Frame (Console, Event.Frame, Process.Symbols);

   exception
      when MAT.Events.Targets.Not_Found =>
         Console.Error ("Event " & Args & " not found");

   end Event_Command;

   --  ------------------------------
   --  Maps command to dump the memory maps of the program.
   --  ------------------------------
   procedure Maps_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String) is
      pragma Unreferenced (Args);
      use type ELF.Elf32_Word;

      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Maps    : MAT.Memory.Region_Info_Map;
      Iter    : MAT.Memory.Region_Info_Cursor;
      Region  : MAT.Memory.Region_Info;
   begin
      MAT.Memory.Targets.Find (Memory => Process.Memory,
                               From   => MAT.Types.Target_Addr'First,
                               To     => MAT.Types.Target_Addr'Last,
                               Into   => Maps);
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_RANGE_ADDR, "Address range", 39);
      Console.Print_Title (MAT.Consoles.F_MODE, "Flags", 6);
      Console.Print_Title (MAT.Consoles.F_FILE_NAME, "Path", 40);
      Console.End_Title;

      Iter := Maps.First;
      while MAT.Memory.Region_Info_Maps.Has_Element (Iter) loop
         Region := MAT.Memory.Region_Info_Maps.Element (Iter);
         declare
            Flags  : String (1 .. 3) := "---";
         begin
            if (Region.Flags and ELF.PF_R) /= 0 then
               Flags (1) := 'r';
            end if;
            if (Region.Flags and ELF.PF_W) /= 0 then
               Flags (2) := 'w';
            end if;
            if (Region.Flags and ELF.PF_X) /= 0 then
               Flags (3) := 'x';
            end if;
            Console.Start_Row;
            Console.Print_Field (MAT.Consoles.F_RANGE_ADDR, Region.Start_Addr, Region.End_Addr);
            Console.Print_Field (MAT.Consoles.F_MODE, Flags);
            Console.Print_Field (MAT.Consoles.F_FILE_NAME, Region.Path);
            Console.End_Row;
         end;
         MAT.Memory.Region_Info_Maps.Next (Iter);
      end loop;
   end Maps_Command;

   --  ------------------------------
   --  Symbol command.
   --  Load the symbols from the binary file.
   --  ------------------------------
   procedure Symbol_Command (Target : in out MAT.Targets.Target_Type'Class;
                             Args   : in String) is
      use type ELF.Elf32_Word;

      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Maps    : MAT.Memory.Region_Info_Map;
      Iter    : MAT.Memory.Region_Info_Cursor;
   begin
      MAT.Memory.Targets.Find (Memory => Process.Memory,
                               From   => MAT.Types.Target_Addr'First,
                               To     => MAT.Types.Target_Addr'Last,
                               Into   => Maps);
      Iter := Maps.First;
      while MAT.Memory.Region_Info_Maps.Has_Element (Iter) loop
         declare
            Region : MAT.Memory.Region_Info := MAT.Memory.Region_Info_Maps.Element (Iter);
            Offset : MAT.Types.Target_Addr;
         begin
            if (Region.Flags and ELF.PF_X) /= 0 then
               if Ada.Strings.Unbounded.Length (Region.Path) = 0 then
                  Region.Path := Ada.Strings.Unbounded.To_Unbounded_String (Args);
                  Offset := 0;
               else
                  Offset := Region.Start_Addr;
               end if;
               MAT.Symbols.Targets.Load_Symbols (Process.Symbols.Value.all, Region, Offset);
            end if;

         exception
            when Bfd.OPEN_ERROR =>
               Target.Console.Error ("Cannot open symbol library file '"
                                     & Ada.Strings.Unbounded.To_String (Region.Path) & "'");
         end;
         MAT.Memory.Region_Info_Maps.Next (Iter);
      end loop;
      MAT.Symbols.Targets.Open (Process.Symbols.Value.all, Args);

   exception
      when Bfd.OPEN_ERROR =>
         Target.Console.Error ("Cannot open symbol file '" & Args & "'");
   end Symbol_Command;

   --  ------------------------------
   --  Exit command.
   --  ------------------------------
   procedure Exit_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String) is
   begin
      raise Stop_Interp;
   end Exit_Command;

   --  ------------------------------
   --  Open a MAT file and read the events.
   --  ------------------------------
   procedure Open_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String) is
      use type MAT.Types.Target_Tick_Ref;

      Reader        : MAT.Readers.Streams.Files.File_Reader_Type;
      Start, Finish : MAT.Types.Target_Tick_Ref;
      Duration      : MAT.Types.Target_Tick_Ref;
      Count         : Integer;
   begin
      Target.Initialize (Reader);
      Reader.Open (Args);
      Reader.Read_All;
      Target.Process.Events.Get_Time_Range (Start, Finish);
      Count := Target.Process.Events.Get_Event_Counter;
      Duration := Finish - Start;
      Target.Console.Notice (MAT.Consoles.N_DURATION,
                             "Loaded" & Integer'Image (Count) & " events, duration "
                             & MAT.Formats.Duration (Duration));

   exception
      when E : Ada.IO_Exceptions.Name_Error =>
         Log.Error ("Cannot open {0}: {1}", Args, Ada.Exceptions.Exception_Message (E));

      when E : others =>
         Log.Error ("Invalid format for " & Args, E, True);

   end Open_Command;

   --  ------------------------------
   --  Print some help for available commands.
   --  ------------------------------
   procedure Help_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String) is
      pragma Unreferenced (Args);
      use MAT.Consoles;

      Console : constant MAT.Consoles.Console_Access := Target.Console;
   begin
      Console.Notice (N_HELP, "Available commands");
      Console.Notice (N_HELP, "exit                --  Exit the tool");
      Console.Notice (N_HELP, "events <selection>  --  List the events filtered by the selection");
      Console.Notice (N_HELP, "event id            --  Print the event ID");
      Console.Notice (N_HELP, "threads             --  List the threads");
      Console.Notice (N_HELP, "slots <selection>   --  List the memory slots"
                      & " filtered by the selection");
      Console.Notice (N_HELP, "sizes               --  ");
      Console.Notice (N_HELP, "frames <level>      --  Print the stack frames up"
                      & " to the given level");
      Console.Notice (N_HELP, "open file           --  Load the mat file to analyze");
      Console.Notice (N_HELP, "symbol file         --  Load the executable symbol file");
   end Help_Command;

   function Get_Command (Line : in String) return String is
      Pos : constant Natural := Util.Strings.Index (Line, ' ');
   begin
      if Pos = 0 then
         return Line;
      else
         return Line (Line'First .. Pos - 1);
      end if;
   end Get_Command;

   --  ------------------------------
   --  Execute the command given in the line.
   --  ------------------------------
   procedure Execute (Target : in out MAT.Targets.Target_Type'Class;
                      Line   : in String) is
      Command : constant String := Get_Command (Line);
      Index   : constant Natural := Util.Strings.Index (Line, ' ');
      Pos     : constant Command_Map.Cursor := Commands.Find (Command);
   begin
      if Command_Map.Has_Element (Pos) then
         Command_Map.Element (Pos) (Target, Line (Index + 1 .. Line'Last));
      elsif Command'Length > 0 then
         Target.Console.Error ("Command '" & Command & "' not found");
      end if;

   exception
      when Stop_Interp =>
         raise;

      when E : others =>
         Log.Error ("Exception: ", E, True);
         Target.Console.Error ("Exception while processing command");

   end Execute;

   --  ------------------------------
   --  Initialize the process targets by loading the MAT files.
   --  ------------------------------
   procedure Initialize_Files (Target  : in out MAT.Targets.Target_Type'Class) is
   begin
      loop
         declare
            Path : constant String := GNAT.Command_Line.Get_Argument;
         begin
            exit when Path'Length = 0;
            Open_Command (Target, Path);
         end;
      end loop;
   end Initialize_Files;

begin
   Commands.Insert ("exit", Exit_Command'Access);
   Commands.Insert ("quit", Exit_Command'Access);
   Commands.Insert ("open", Open_Command'Access);
   Commands.Insert ("malloc-sizes", Sizes_Command'Access);
   Commands.Insert ("symbol", Symbol_Command'Access);
   Commands.Insert ("slots", Slot_Command'Access);
   Commands.Insert ("threads", Threads_Command'Access);
   Commands.Insert ("frames", Frames_Command'Access);
   Commands.Insert ("events", Events_Command'Access);
   Commands.Insert ("event", Event_Command'Access);
   Commands.Insert ("sizes", Event_Sizes_Command'Access);
   Commands.Insert ("event-frames", Event_Frames_Command'Access);
   Commands.Insert ("help", Help_Command'Access);
   Commands.Insert ("maps", Maps_Command'Access);
end MAT.Commands;
