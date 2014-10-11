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
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with MAT.Types;
with MAT.Readers.Files;
with MAT.Memory.Tools;
with MAT.Memory.Targets;
with MAT.Symbols.Targets;
with MAT.Frames;
with MAT.Frames.Print;
package body MAT.Commands is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Commands");

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
      Slots : MAT.Memory.Allocation_Map;
      Iter  : MAT.Memory.Allocation_Cursor;

      procedure Print (Addr : in MAT.Types.Target_Addr;
                       Slot : in MAT.Memory.Allocation) is
         use type MAT.Frames.Frame_Type;
         Backtrace : MAT.Frames.Frame_Table := MAT.Frames.Backtrace (Slot.Frame);

         Name : Ada.Strings.Unbounded.Unbounded_String;
         Func : Ada.Strings.Unbounded.Unbounded_String;
         Line : Natural;
      begin
         Ada.Text_IO.Put (MAT.Types.Hex_Image (Addr));
         Ada.Text_IO.Set_Col (14);
         Ada.Text_IO.Put (MAT.Types.Target_Size'Image (Slot.Size));
         Ada.Text_IO.New_Line;
         for I in Backtrace'Range loop
            Ada.Text_IO.Put ("   ");
            Ada.Text_IO.Put (Natural'Image (I));
            Ada.Text_IO.Put ("   ");
            Ada.Text_IO.Put (MAT.Types.Hex_Image (Backtrace (I)));
            MAT.Symbols.Targets.Find_Nearest_Line (Symbols => Target.Symbols,
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

   begin
      Target.Memory.Find (From => MAT.Types.Target_Addr'First,
                          To   => MAT.Types.Target_Addr'Last,
                          Into => Slots);
      Iter := Slots.First;
      while MAT.Memory.Allocation_Maps.Has_Element (Iter) loop
         MAT.Memory.Allocation_Maps.Query_Element (Iter, Print'Access);
         MAT.Memory.Allocation_Maps.Next (Iter);
      end loop;
   end Slot_Command;

   --  ------------------------------
   --  Sizes command.
   --  Collect statistics about the used memory slots and report the different slot
   --  sizes with count.
   --  ------------------------------
   procedure Sizes_Command (Target : in out MAT.Targets.Target_Type'Class;
                            Args   : in String) is
      Sizes : MAT.Memory.Tools.Size_Info_Map;
      Iter  : MAT.Memory.Tools.Size_Info_Cursor;
   begin
      MAT.Memory.Targets.Size_Information (Memory => Target.Memory,
                                           Sizes  => Sizes);
      Iter := Sizes.First;
      while MAT.Memory.Tools.Size_Info_Maps.Has_Element (Iter) loop
         declare
            use type MAT.Types.Target_Size;

            Size  : MAT.Types.Target_Size := MAT.Memory.Tools.Size_Info_Maps.Key (Iter);
            Info  : MAT.Memory.Tools.Size_Info_Type := MAT.Memory.Tools.Size_Info_Maps.Element (Iter);
            Total : MAT.Types.Target_Size := Size * MAT.Types.Target_Size (Info.Count);
         begin
            Ada.Text_IO.Put (MAT.Types.Target_Size'Image (Size));
            Ada.Text_IO.Set_Col (20);
            Ada.Text_IO.Put (Natural'Image (Info.Count));
            Ada.Text_IO.Set_Col (30);
            Ada.Text_IO.Put_Line (MAT.Types.Target_Size'Image (Total));
         end;
         MAT.Memory.Tools.Size_Info_Maps.Next (Iter);
      end loop;
   end Sizes_Command;

   --  ------------------------------
   --  Symbol command.
   --  Load the symbols from the binary file.
   --  ------------------------------
   procedure Symbol_Command (Target : in out MAT.Targets.Target_Type'Class;
                             Args   : in String) is
   begin
      MAT.Symbols.Targets.Open (Target.Symbols, Args);
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
      Reader : MAT.Readers.Files.File_Reader_Type;
   begin
      Target.Initialize (Reader);
      Reader.Open (Args);
      Reader.Read_All;

   exception
      when E : Ada.IO_Exceptions.Name_Error =>
         Log.Error ("Cannot open {0}", Args);
   end Open_Command;

   function Get_Command (Line : in String) return String is
      Pos : Natural := Util.Strings.Index (Line, ' ');
   begin
      if Pos <= 0 then
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
         Ada.Text_IO.Put_Line ("Command '" & Command & "' not found");
      end if;
   end Execute;

begin
   Commands.Insert ("exit", Exit_Command'Access);
   Commands.Insert ("quit", Exit_Command'Access);
   Commands.Insert ("open", Open_Command'Access);
   Commands.Insert ("sizes", Sizes_Command'Access);
   Commands.Insert ("symbol", Symbol_Command'Access);
   Commands.Insert ("slots", Slot_Command'Access);
end MAT.Commands;
