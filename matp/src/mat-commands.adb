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
with Util.Strings.Tokenizers;
with Util.Log.Loggers;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Exceptions;
with Ada.IO_Exceptions;
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
with MAT.Consoles;
with MAT.Formats;
with MAT.Events.Tools;
with MAT.Events.Timelines;
package body MAT.Commands is

   use type MAT.Types.Target_Size;

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
   procedure Event_Frames_Command (Target : in out MAT.Targets.Target_Type'Class;
                                   Args   : in String);

   --  Maps command to dump the memory maps of the program.
   procedure Maps_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);

   --  Print the stack frame description in the console.
   procedure Print_Frame (Console : in MAT.Consoles.Console_Access;
                          Frame   : in MAT.Frames.Frame_Type;
                          Symbols : in MAT.Symbols.Targets.Target_Symbols_Ref);

   --  Print the memory regions described by the Regions map.
   procedure Print_Regions (Console : in MAT.Consoles.Console_Access;
                            Regions : in MAT.Memory.Region_Info_Map);

   --  Print the events with a short description.
   procedure Print_Events (Console : in MAT.Consoles.Console_Access;
                           Events  : in MAT.Events.Tools.Target_Event_Vector;
                           Start   : in MAT.Types.Target_Tick_Ref);

   --  Print the full description of a memory slot that is currently allocated.
   procedure Print_Slot (Console : in MAT.Consoles.Console_Access;
                         Addr    : in MAT.Types.Target_Addr;
                         Slot    : in MAT.Memory.Allocation;
                         Symbols : in MAT.Symbols.Targets.Target_Symbols_Ref;
                         Start   : in MAT.Types.Target_Tick_Ref);

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
   --  Print the memory regions described by the Regions map.
   --  ------------------------------
   procedure Print_Regions (Console : in MAT.Consoles.Console_Access;
                            Regions : in MAT.Memory.Region_Info_Map) is
      use type ELF.Elf32_Word;
      Iter    : MAT.Memory.Region_Info_Cursor;
      Region  : MAT.Memory.Region_Info;
   begin
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_RANGE_ADDR, "Address range", 39);
      Console.Print_Title (MAT.Consoles.F_MODE, "Flags", 6);
      Console.Print_Title (MAT.Consoles.F_FILE_NAME, "Path", 40);
      Console.End_Title;

      Iter := Regions.First;
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
   end Print_Regions;

   --  ------------------------------
   --  Print the events with a short description.
   --  ------------------------------
   procedure Print_Events (Console : in MAT.Consoles.Console_Access;
                           Events  : in MAT.Events.Tools.Target_Event_Vector;
                           Start   : in MAT.Types.Target_Tick_Ref) is
      Iter : MAT.Events.Tools.Target_Event_Cursor;
   begin
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_PREVIOUS, "Previous", 10);
      Console.Print_Title (MAT.Consoles.F_ID, "Id", 10);
      Console.Print_Title (MAT.Consoles.F_NEXT, "Next", 10);
      Console.Print_Title (MAT.Consoles.F_TIME, "Time", 10);
      Console.Print_Title (MAT.Consoles.F_EVENT, "Event", 60);
      Console.End_Title;

      Iter := Events.First;
      while MAT.Events.Tools.Target_Event_Vectors.Has_Element (Iter) loop
         declare
            use type MAT.Types.Target_Tick_Ref;

            Event : constant MAT.Events.Target_Event_Type
              := MAT.Events.Tools.Target_Event_Vectors.Element (Iter);
            Time  : constant MAT.Types.Target_Tick_Ref := Event.Time - Start;
         begin
            Console.Start_Row;
            Console.Print_Field (MAT.Consoles.F_PREVIOUS,
                                 MAT.Formats.Offset (Event.Prev_Id, Event.Id));
            Console.Print_Field (MAT.Consoles.F_ID,
                                 MAT.Events.Event_Id_Type'Image (Event.Id));
            Console.Print_Field (MAT.Consoles.F_NEXT,
                                 MAT.Formats.Offset (Event.Next_Id, Event.Id));
            Console.Print_Duration (MAT.Consoles.F_TIME, Time);
            Console.Print_Field (MAT.Consoles.F_EVENT, MAT.Formats.Event (Event));
            Console.End_Row;
         end;
         MAT.Events.Tools.Target_Event_Vectors.Next (Iter);
      end loop;
   end Print_Events;

   --  ------------------------------
   --  Print the full description of a memory slot that is currently allocated.
   --  ------------------------------
   procedure Print_Slot (Console : in MAT.Consoles.Console_Access;
                         Addr    : in MAT.Types.Target_Addr;
                         Slot    : in MAT.Memory.Allocation;
                         Symbols : in MAT.Symbols.Targets.Target_Symbols_Ref;
                         Start   : in MAT.Types.Target_Tick_Ref) is
      use type MAT.Frames.Frame_Type;
      Backtrace : constant MAT.Frames.Frame_Table := MAT.Frames.Backtrace (Slot.Frame);

      Symbol : MAT.Symbols.Targets.Symbol_Info;
   begin
      if Console.Get_Field_Count = 0 then
         Console.Start_Title;
         Console.Print_Title (MAT.Consoles.F_ID, "Id", 4);
         Console.Print_Title (MAT.Consoles.F_ADDR, "Address", 22);
         Console.Print_Title (MAT.Consoles.F_FUNCTION_NAME, "Function", 80);
         Console.End_Title;
      end if;

      Console.Start_Row;
      Console.Notice (Consoles.N_EVENT_ID, MAT.Formats.Slot (Addr, Slot, Start));
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
      Console.End_Row;
   end Print_Slot;

   procedure Get_Arguments (Args       : in String;
                            Long_Flag  : out Boolean;
                            Count_Flag : out Boolean;
                            Pos        : out Natural) is
      procedure Check_Argument (Token : in String;
                                Done  : out Boolean);

      procedure Check_Argument (Token : in String;
                                Done  : out Boolean) is
      begin
         if Token'Length = 0 or else Token (Token'First) /= '-' then
            Done := True;
         elsif Token = "-l" then
            Long_Flag := True;
            Done := False;
            Pos := Pos + Token'Length + 1;
         elsif Token = "-c" then
            Count_Flag := True;
            Done := False;
            Pos := Pos + Token'Length + 1;
         else
            Done := True;
         end if;
      end Check_Argument;

   begin
      Long_Flag := False;
      Count_Flag := False;
      Pos := Args'First;
      Util.Strings.Tokenizers.Iterate_Tokens (Content => Args,
                                              Pattern => " ",
                                              Process => Check_Argument'Access);
   end Get_Arguments;

   --  ------------------------------
   --  Sizes command.
   --  Collect statistics about the used memory slots and report the different slot
   --  sizes with count.
   --  ------------------------------
   procedure Slot_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String) is
      Slots      : MAT.Memory.Allocation_Map;
      Iter       : MAT.Memory.Allocation_Cursor;
      Symbols    : constant MAT.Symbols.Targets.Target_Symbols_Ref := Target.Process.Symbols;
      Console    : constant MAT.Consoles.Console_Access := Target.Console;
      Process    : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Start, Finish : MAT.Types.Target_Tick_Ref;
      Filter     : MAT.Expressions.Expression_Type;
      Pos        : Natural;
      Long_Flag  : Boolean;
      Count_Flag : Boolean;
   begin
      Get_Arguments (Args, Long_Flag, Count_Flag, Pos);

      if Pos < Args'Last then
         Filter := MAT.Expressions.Parse (Args (Pos .. Args'Last), Process.all'Access);
      end if;
      Process.Memory.Find (From   => MAT.Types.Target_Addr'First,
                           To     => MAT.Types.Target_Addr'Last,
                           Filter => Filter,
                           Into   => Slots);
      if Count_Flag then
         Console.Notice (Consoles.N_INFO, "Found"
                         & Natural'Image (Natural (Slots.Length))
                         & " memory slots");
         return;
      end if;

      Process.Events.Get_Time_Range (Start, Finish);
      Iter := Slots.First;
      while MAT.Memory.Allocation_Maps.Has_Element (Iter) loop
         declare
            Addr : constant MAT.Types.Target_Addr := MAT.Memory.Allocation_Maps.Key (Iter);
            Slot : constant MAT.Memory.Allocation := MAT.Memory.Allocation_Maps.Element (Iter);
         begin
            if not Long_Flag then
               Console.Notice (Consoles.N_EVENT_ID, MAT.Formats.Slot (Addr, Slot, Start));
            else
               Print_Slot (Console, Addr, Slot, Symbols, Start);
            end if;
         end;
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
      Frames  : MAT.Events.Tools.Frame_Event_Info_Map;
      List    : MAT.Events.Tools.Frame_Info_Vector;
      Iter    : MAT.Events.Tools.Frame_Info_Cursor;
      Filter  : MAT.Expressions.Expression_Type;
      Depth   : Natural := 3;
      Symbol  : MAT.Symbols.Targets.Symbol_Info;
      Info    : MAT.Events.Tools.Frame_Info_Type;
      Exact_Depth : Boolean := False;
      Pos         : Natural := Util.Strings.Index (Args, ' ');
   begin
      if Pos = 0 then
         Pos := Args'Last;
      end if;
      if Pos < Args'Last then
         --  Parse the number that identifies the frame depth to print.
         --  When that number is preceeded by '=', only that frame depth is reported.
         begin
            if Args (Args'First) = '=' then
               Exact_Depth := True;
               Depth := Positive'Value (Args (Args'First + 1 .. Pos - 1));
            else
               Depth := Positive'Value (Args (Args'First .. Pos - 1));
            end if;

         exception
            when Constraint_Error =>
               Target.Console.Error ("Invalid frame depth '" & Args (Args'First .. Pos - 1));
               return;
         end;

         --  Skip spaces before the optional filter expression.
         while Pos < Args'Last loop
            exit when Args (Pos) /= ' ';
            Pos := Pos + 1;
         end loop;
         if Pos < Args'Last then
            Filter := MAT.Expressions.Parse (Args (Pos .. Args'Last), Process.all'Access);
         end if;
      end if;

      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_LEVEL, "Level", 6);
      Console.Print_Title (MAT.Consoles.F_SIZE, "Size", 10);
      Console.Print_Title (MAT.Consoles.F_COUNT, "Count", 8);
      Console.Print_Title (MAT.Consoles.F_FUNCTION_NAME, "Function", 60);
      Console.End_Title;

      MAT.Events.Timelines.Find_Frames (Target => Process.Events.all,
                                        Filter => Filter,
                                        Depth  => Depth,
                                        Exact  => Exact_Depth,
                                        Frames => Frames);
      MAT.Events.Tools.Build_Frame_Info (Frames, List);
      Iter := List.First;
      while MAT.Events.Tools.Frame_Info_Vectors.Has_Element (Iter) loop
         Info := MAT.Events.Tools.Frame_Info_Vectors.Element (Iter);
         MAT.Symbols.Targets.Find_Nearest_Line (Symbols => Process.Symbols.Value.all,
                                                Addr    => Info.Key.Addr,
                                                Symbol  => Symbol);

         Console.Start_Row;
         Console.Print_Field (MAT.Consoles.F_LEVEL, Natural'Image (Info.Key.Level));
         Console.Print_Field (MAT.Consoles.F_SIZE,
                              MAT.Formats.Size (Info.Info.Alloc_Size, Info.Info.Free_Size));
         Console.Print_Field (MAT.Consoles.F_COUNT,
                              Info.Info.Count);
         Console.Print_Field (MAT.Consoles.F_FUNCTION_NAME,
                              MAT.Formats.Location (Symbol.File, Symbol.Line, Symbol.Name));

         Console.End_Row;
         MAT.Events.Tools.Frame_Info_Vectors.Next (Iter);
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
      Sizes   : MAT.Events.Tools.Size_Event_Info_Map;
      Filter  : MAT.Expressions.Expression_Type;
      Iter    : MAT.Events.Tools.Size_Event_Info_Cursor;
   begin
      if Args'Length > 0 then
         Filter := MAT.Expressions.Parse (Args, Process.all'Access);
      end if;
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_ID, "Event Id range", 30);
      Console.Print_Title (MAT.Consoles.F_TIME, "Time", 10);
      Console.Print_Title (MAT.Consoles.F_EVENT, "Event", 20);
      Console.Print_Title (MAT.Consoles.F_SIZE, "Size", 12);
      Console.Print_Title (MAT.Consoles.F_COUNT, "Count", 8);
      Console.Print_Title (MAT.Consoles.F_TOTAL_SIZE, "Total size", 12);
      Console.Print_Title (MAT.Consoles.F_GROW_SIZE, "Memory", 12);
      Console.End_Title;

      Process.Events.Get_Time_Range (Start, Finish);
      MAT.Events.Timelines.Find_Sizes (Target => Process.Events.all,
                                       Filter => Filter,
                                       Sizes  => Sizes);
      Iter := Sizes.First;
      while MAT.Events.Tools.Size_Event_Info_Maps.Has_Element (Iter) loop
         declare
            use type MAT.Types.Target_Tick_Ref;

            Size  : constant MAT.Types.Target_Size
              := MAT.Events.Tools.Size_Event_Info_Maps.Key (Iter);
            Info  : constant MAT.Events.Tools.Event_Info_Type
              := MAT.Events.Tools.Size_Event_Info_Maps.Element (Iter);
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
            Console.Print_Field (MAT.Consoles.F_TOTAL_SIZE,
                                 MAT.Formats.Size (MAT.Types.Target_Size (Info.Count) * Size));
            if Info.Alloc_Size > Info.Free_Size then
               Console.Print_Field (MAT.Consoles.F_GROW_SIZE,
                                    "+" & MAT.Formats.Size (Info.Alloc_Size - Info.Free_Size));
            elsif Info.Alloc_Size < Info.Free_Size then
               Console.Print_Field (MAT.Consoles.F_GROW_SIZE,
                                    "-" & MAT.Formats.Size (Info.Free_Size - Info.Alloc_Size));
            end if;
            Console.End_Row;
         end;
         MAT.Events.Tools.Size_Event_Info_Maps.Next (Iter);
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
      use type MAT.Events.Event_Id_Type;

      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Start, Finish : MAT.Types.Target_Tick_Ref;
      Events  : MAT.Events.Tools.Target_Event_Vector;
      Filter  : MAT.Expressions.Expression_Type;
      Pos        : Natural;
      Long_Flag  : Boolean;
      Count_Flag : Boolean;
   begin
      Get_Arguments (Args, Long_Flag, Count_Flag, Pos);
      if Pos < Args'Last then
         Filter := MAT.Expressions.Parse (Args (Pos .. Args'Last), Process.all'Access);
      end if;

      MAT.Events.Timelines.Filter_Events (Process.Events.all, Filter, Events);
      if Count_Flag then
         Console.Notice (Consoles.N_INFO, "Found"
                         & Natural'Image (Natural (Events.Length))
                         & " events");
         return;
      end if;

      Process.Events.Get_Time_Range (Start, Finish);
      Print_Events (Console, Events, Start);

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
      Id      : MAT.Events.Event_Id_Type;
      Event   : MAT.Events.Target_Event_Type;
      Start, Finish : MAT.Types.Target_Tick_Ref;
      Related : MAT.Events.Tools.Target_Event_Vector;
   begin
      Id := MAT.Events.Event_Id_Type'Value (Args);
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
      when Constraint_Error =>
         Console.Error ("Invalid event '" & Args & "'");

      when MAT.Events.Tools.Not_Found =>
         Console.Error ("Event " & Args & " not found");

   end Event_Command;

   --  ------------------------------
   --  Timeline command.
   --  Identify the interesting timeline groups in the events and display them.
   --  ------------------------------
   procedure Timeline_Command (Target : in out MAT.Targets.Target_Type'Class;
                               Args   : in String) is
      use type MAT.Types.Target_Tick_Ref;

      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Start, Finish : MAT.Types.Target_Tick_Ref;
      Groups  : MAT.Events.Timelines.Timeline_Info_Vector;
      Iter    : MAT.Events.Timelines.Timeline_Info_Cursor;
      Level   : Positive := 1;
   begin
      if Args'Length > 0 then
         begin
            Level := Positive'Value (Args);

         exception
            when Constraint_Error =>
               Console.Error ("Invalid level '" & Args & "'");
               return;
         end;
      end if;
      MAT.Events.Timelines.Extract (Process.Events.all, Level, Groups);

      Process.Events.Get_Time_Range (Start, Finish);
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_START_TIME, "Start", 10);
      Console.Print_Title (MAT.Consoles.F_END_TIME, "End time", 10);
      Console.Print_Title (MAT.Consoles.F_DURATION, "Duration", 10);
      Console.Print_Title (MAT.Consoles.F_EVENT_RANGE, "Event range", 20);
      Console.Print_Title (MAT.Consoles.F_MALLOC_COUNT, "# malloc", 10);
      Console.Print_Title (MAT.Consoles.F_REALLOC_COUNT, "# realloc", 10);
      Console.Print_Title (MAT.Consoles.F_FREE_COUNT, "# free", 10);
      Console.Print_Title (MAT.Consoles.F_TOTAL_SIZE, "Memory", 10);
      Console.End_Title;

      Iter := Groups.First;
      while MAT.Events.Timelines.Timeline_Info_Vectors.Has_Element (Iter) loop
         declare
            Info : constant MAT.Events.Timelines.Timeline_Info
              := MAT.Events.Timelines.Timeline_Info_Vectors.Element (Iter);
         begin
            Console.Start_Row;
            Console.Print_Duration (MAT.Consoles.F_START_TIME, Info.First_Event.Time - Start);
            Console.Print_Duration (MAT.Consoles.F_END_TIME, Info.Last_Event.Time - Start);
            Console.Print_Duration (MAT.Consoles.F_DURATION,
                                    Info.Last_Event.Time - Info.First_Event.Time);
            Console.Print_Field (MAT.Consoles.F_EVENT_RANGE,
                                 MAT.Formats.Event (Info.First_Event, Info.Last_Event));
            Console.Print_Field (MAT.Consoles.F_MALLOC_COUNT, Info.Malloc_Count);
            Console.Print_Field (MAT.Consoles.F_REALLOC_COUNT, Info.Realloc_Count);
            Console.Print_Field (MAT.Consoles.F_FREE_COUNT, Info.Free_Count);
            if Info.Alloc_Size = Info.Free_Size then
               Console.Print_Field (MAT.Consoles.F_TOTAL_SIZE, "");
            elsif Info.Alloc_Size > Info.Free_Size then
               Console.Print_Field (MAT.Consoles.F_TOTAL_SIZE,
                                    "+" & MAT.Formats.Size (Info.Alloc_Size - Info.Free_Size));
            else
               Console.Print_Field (MAT.Consoles.F_TOTAL_SIZE,
                                    "-" & MAT.Formats.Size (Info.Free_Size - Info.Alloc_Size));
            end if;
            Console.End_Row;
         end;
         MAT.Events.Timelines.Timeline_Info_Vectors.Next (Iter);
      end loop;
   end Timeline_Command;

   --  ------------------------------
   --  Addr command to print a description of an address.
   --  ------------------------------
   procedure Addr_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String) is
      use type ELF.Elf32_Word;

      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Addr    : MAT.Types.Target_Addr;
      Maps    : MAT.Memory.Region_Info_Map;
      Region  : MAT.Memory.Region_Info;
   begin
      if Args'Length >= 3 and then Args (Args'First .. Args'First + 1) = "0x" then
         Addr := MAT.Types.Hex_Value (Args (Args'First + 2 .. Args'Last));
      else
         Addr := MAT.Types.Hex_Value (Args);
      end if;

      --  Find out the memory regions for that address.
      MAT.Memory.Targets.Find (Memory => Process.Memory,
                               From   => Addr,
                               To     => Addr,
                               Into   => Maps);
      if Maps.Is_Empty then
         Console.Error ("Address '" & Args & "' is not in any memory region.");
         return;
      end if;

      --  Print the memory region.
      Print_Regions (Target.Console, Maps);
      Region := Maps.First_Element;

      --  If this is an executable region, find the symbol and print it.
      if (Region.Flags and ELF.PF_X) /= 0 then
         declare
            Symbol : MAT.Symbols.Targets.Symbol_Info;
         begin
            MAT.Symbols.Targets.Find_Nearest_Line (Symbols => Process.Symbols.Value.all,
                                                   Addr    => Addr,
                                                   Symbol  => Symbol);
            if Ada.Strings.Unbounded.Length (Symbol.File) /= 0 then
               Console.Notice (MAT.Consoles.N_INFO,
                               MAT.Formats.Location (Symbol.File, Symbol.Line, Symbol.Name));
            end if;
         end;
      else
         declare
            Filter  : MAT.Expressions.Expression_Type;
            Start, Finish : MAT.Types.Target_Tick_Ref;
            Events  : MAT.Events.Tools.Target_Event_Vector;
         begin
            Filter := MAT.Expressions.Create_Addr (Addr, Addr);
            Process.Events.Get_Time_Range (Start, Finish);
            MAT.Events.Timelines.Filter_Events (Process.Events.all, Filter, Events);
            Print_Events (Console, Events, Start);
         end;
      end if;

   exception
      when Constraint_Error =>
         Console.Error ("Invalid address '" & Args & "'");
   end Addr_Command;

   --  ------------------------------
   --  Maps command to dump the memory maps of the program.
   --  ------------------------------
   procedure Maps_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String) is
      pragma Unreferenced (Args);

      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Maps    : MAT.Memory.Region_Info_Map;
   begin
      MAT.Memory.Targets.Find (Memory => Process.Memory,
                               From   => MAT.Types.Target_Addr'First,
                               To     => MAT.Types.Target_Addr'Last,
                               Into   => Maps);
      Print_Regions (Target.Console, Maps);
   end Maps_Command;

   --  ------------------------------
   --  Info command to print symmary information about the program.
   --  ------------------------------
   procedure Info_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String) is
      pragma Unreferenced (Args);

      use type MAT.Targets.Target_Process_Type_Access;
      use type MAT.Types.Target_Tick_Ref;
      use Ada.Strings.Unbounded;

      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
      Start   : MAT.Events.Target_Event_Type;
      Finish  : MAT.Events.Target_Event_Type;
      Maps    : MAT.Memory.Region_Info_Map;
      Stats   : MAT.Memory.Targets.Memory_Stat;
   begin
      if Process = null then
         Console.Notice (Consoles.N_EVENT_ID, "There is no process");
         return;
      end if;
      Process.Memory.Stat_Information (Stats);
      Process.Events.Get_Limits (Start, Finish);

      --  Print number of memory regions.
      MAT.Memory.Targets.Find (Memory => Process.Memory,
                               From   => MAT.Types.Target_Addr'First,
                               To     => MAT.Types.Target_Addr'Last,
                               Into   => Maps);

      Console.Notice (Consoles.N_INFO, "Pid            : " & MAT.Formats.Pid (Process.Pid));
      Console.Notice (Consoles.N_INFO, "Path           : " & To_String (Process.Path));
      Console.Notice (Consoles.N_INFO, "Endianness     : "
                      & MAT.Readers.Endian_Type'Image (Process.Endian));
      Console.Notice (Consoles.N_INFO, "Memory regions : "
                      & Util.Strings.Image (Natural (Maps.Length)));
      Console.Notice (Consoles.N_INFO, "Events         : " & MAT.Formats.Event (Start, Finish));
      Console.Notice (Consoles.N_INFO, "Duration       : "
                      & MAT.Formats.Duration (Finish.Time - Start.Time));

      Console.Notice (Consoles.N_INFO, "Malloc count   : "
                      & Natural'Image (Stats.Malloc_Count));
      Console.Notice (Consoles.N_INFO, "Realloc count  : "
                      & Natural'Image (Stats.Realloc_Count));
      Console.Notice (Consoles.N_INFO, "Free count     : "
                      & Natural'Image (Stats.Free_Count));
      Console.Notice (Consoles.N_INFO, "Memory alloced : "
                      & MAT.Formats.Size (Stats.Total_Alloc));
      Console.Notice (Consoles.N_INFO, "Memory slots   : "
                      & Natural'Image (Stats.Used_Count));
   end Info_Command;

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
   begin
      MAT.Memory.Targets.Find (Memory => Process.Memory,
                               From   => MAT.Types.Target_Addr'First,
                               To     => MAT.Types.Target_Addr'Last,
                               Into   => Maps);
      MAT.Symbols.Targets.Load_Symbols (Process.Symbols.Value.all, Maps);
      MAT.Symbols.Targets.Open (Process.Symbols.Value.all, Args);

   exception
      when Bfd.OPEN_ERROR =>
         Console.Error ("Cannot open symbol file '" & Args & "'");
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
      Console.Notice (N_HELP, "exit                    Exit the tool");
      Console.Notice (N_HELP, "event id                Print the event ID");
      Console.Notice (N_HELP, "events <selection>      List the events filtered by the selection");
      Console.Notice (N_HELP, "threads                 List the threads");
      Console.Notice (N_HELP, "slots <selection>       List the memory slots"
                      & " filtered by the selection");
      Console.Notice (N_HELP, "sizes <selection>       Print a summary of sizes "
                      & "filtered by the selection");
      Console.Notice (N_HELP, "frames <level> [<filter>] Print the stack frames up"
                      & " to the given level");
      Console.Notice (N_HELP, "open <file>             Load the mat file to analyze");
      Console.Notice (N_HELP, "symbol <file>           Load the executable symbol file");
      Console.Notice (N_HELP, "info                    Print some information about the program");
      Console.Notice (N_HELP, "maps                    Print the program memory maps");
      Console.Notice (N_HELP, "Selection examples:");
      Console.Notice (N_HELP, "  size > 100 and size < 1000    Selection on the allocation size");
      Console.Notice (N_HELP, "  has 0x4a7281                  Allocations with the given address");
      Console.Notice (N_HELP, "  event >= 200 and event <= 300 Event range selection");
      Console.Notice (N_HELP, "  malloc or realloc             Malloc or realloc events");
      Console.Notice (N_HELP, "  by printf and not by foo      Event produced by a function");
      Console.Notice (N_HELP, "  in libz.so.1                  Event produced by a shared library");
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
      Index   : Natural := Util.Strings.Index (Line, ' ');
      Pos     : constant Command_Map.Cursor := Commands.Find (Command);
   begin
      if Command_Map.Has_Element (Pos) then
         if Index = 0 then
            Index := Line'Last + 1;
         end if;
         Command_Map.Element (Pos) (Target, Line (Index + 1 .. Line'Last));
      elsif Command'Length > 0 then
         Target.Console.Error ("Command '" & Command
                               & "' not found.  Type 'help' for available commands");
      end if;

   exception
      when Stop_Interp =>
         raise;

      when Usage_Error =>
         Target.Console.Error ("Invalid argument '" & Line (Index + 1 .. Line'Last)
                               & "' for command " & Command);

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
   Commands.Insert ("malloc-frames", Frames_Command'Access);
   Commands.Insert ("events", Events_Command'Access);
   Commands.Insert ("event", Event_Command'Access);
   Commands.Insert ("sizes", Event_Sizes_Command'Access);
   Commands.Insert ("frames", Event_Frames_Command'Access);
   Commands.Insert ("timeline", Timeline_Command'Access);
   Commands.Insert ("help", Help_Command'Access);
   Commands.Insert ("maps", Maps_Command'Access);
   Commands.Insert ("info", Info_Command'Access);
   Commands.Insert ("addr", Addr_Command'Access);
end MAT.Commands;
