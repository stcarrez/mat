-----------------------------------------------------------------------
--  mat-interp -- Command interpreter
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

with MAT.Types;
with MAT.Readers.Streams.Files;
with MAT.Memory.Tools;
with MAT.Memory.Targets;
with MAT.Symbols.Targets;
with MAT.Expressions;
with MAT.Frames;
with MAT.Consoles;
package body MAT.Commands is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Commands");

   function Get_Command (Line : in String) return String;
   procedure Slot_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);
   procedure Frames_Command (Target : in out MAT.Targets.Target_Type'Class;
                             Args   : in String);
   procedure Threads_Command (Target : in out MAT.Targets.Target_Type'Class;
                              Args   : in String);
   procedure Exit_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);
   procedure Open_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);

   package Command_Map is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Command_Handler,
                                                Equivalent_Keys => "=",
                                                Hash            => Ada.Strings.Hash);

   Commands : Command_Map.Map;

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

      procedure Print (Addr : in MAT.Types.Target_Addr;
                       Slot : in MAT.Memory.Allocation) is
         use type MAT.Frames.Frame_Type;
         Backtrace : constant MAT.Frames.Frame_Table := MAT.Frames.Backtrace (Slot.Frame);

         Name : Ada.Strings.Unbounded.Unbounded_String;
         Func : Ada.Strings.Unbounded.Unbounded_String;
         Line : Natural;
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
            Ada.Text_IO.Put ("   ");
            Ada.Text_IO.Put (Natural'Image (I));
            Ada.Text_IO.Put ("   ");
            Ada.Text_IO.Put (MAT.Types.Hex_Image (Backtrace (I)));
            MAT.Symbols.Targets.Find_Nearest_Line (Symbols => Symbols.Value.all,
                                                   Addr    => Backtrace (I),
                                                   Name    => Name,
                                                   Func    => Func,
                                                   Line    => Line);
            Ada.Text_IO.Put ("   ");
            Ada.Text_IO.Put (Ada.Strings.Unbounded.To_String (Func));
            Ada.Text_IO.Put (" ");
            Ada.Text_IO.Put (Ada.Strings.Unbounded.To_String (Name));
            if Line /= 0 then
               Ada.Text_IO.Put (":");
               Ada.Text_IO.Put (Util.Strings.Image (Line));
            end if;
            Ada.Text_IO.New_Line;
         end loop;
      end Print;

      Filter : MAT.Expressions.Expression_Type;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
   begin
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
      Iter    : MAT.Memory.Tools.Size_Info_Cursor;
      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
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
            Console.Print_Field (MAT.Consoles.F_TOTAL_SIZE, Total);
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
      Iter    : MAT.Memory.Memory_Info_Cursor;
      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
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
      Iter    : MAT.Memory.Frame_Info_Cursor;
      Level   : Positive := 3;
      Console : constant MAT.Consoles.Console_Access := Target.Console;
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
   begin
      if Args'Length > 0 then
         Level := Positive'Value (Args);
      end if;
      Console.Start_Title;
      Console.Print_Title (MAT.Consoles.F_FILE_NAME, "File", 20);
      Console.Print_Title (MAT.Consoles.F_LINE_NUMBER, "Line", 6);
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
            Name : Ada.Strings.Unbounded.Unbounded_String;
            File_Name : Ada.Strings.Unbounded.Unbounded_String;
            Line : Natural;
         begin
            MAT.Symbols.Targets.Find_Nearest_Line (Symbols => Process.Symbols.Value.all,
                                                   Addr    => Func,
                                                   Name    => File_Name,
                                                   Func    => Name,
                                                   Line    => Line);
            Console.Start_Row;
            if Ada.Strings.Unbounded.Length (Name) = 0 then
               Console.Print_Field (MAT.Consoles.F_FUNCTION_NAME, Func);
            else
               Console.Print_Field (MAT.Consoles.F_FILE_NAME, File_Name);
               Console.Print_Field (MAT.Consoles.F_LINE_NUMBER, Line);
               Console.Print_Field (MAT.Consoles.F_FUNCTION_NAME, Name);
            end if;
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
   --  Symbol command.
   --  Load the symbols from the binary file.
   --  ------------------------------
   procedure Symbol_Command (Target : in out MAT.Targets.Target_Type'Class;
                             Args   : in String) is
      Process : constant MAT.Targets.Target_Process_Type_Access := Target.Process;
   begin
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
      Reader : MAT.Readers.Streams.Files.File_Reader_Type;
   begin
      Target.Initialize (Reader);
      Reader.Open (Args);
      Reader.Read_All;

   exception
      when E : Ada.IO_Exceptions.Name_Error =>
         Log.Error ("Cannot open {0}: {1}", Args, Ada.Exceptions.Exception_Message (E));

      when E : others =>
         Log.Error ("Invalid format for " & Args, E, True);

   end Open_Command;

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
   Commands.Insert ("sizes", Sizes_Command'Access);
   Commands.Insert ("symbol", Symbol_Command'Access);
   Commands.Insert ("slots", Slot_Command'Access);
   Commands.Insert ("threads", Threads_Command'Access);
   Commands.Insert ("frames", Frames_Command'Access);
end MAT.Commands;
