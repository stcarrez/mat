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

with MAT.Readers.Files;
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
      end if;
   end Execute;

begin
   Commands.Insert ("exit", Exit_Command'Access);
   Commands.Insert ("open", Open_Command'Access);
end MAT.Commands;
