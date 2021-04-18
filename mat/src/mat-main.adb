-----------------------------------------------------------------------
--  mat-main -- Main program
--  Copyright (C) 2014, 2015, 2021 Stephane Carrez
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
with Ada.IO_Exceptions;

with Ada.Command_Line;
with GNAT.Command_Line;
with MAT.Commands;
with MAT.Targets;
with MAT.Consoles.Text;
procedure Mat.Main is
   Target  : MAT.Targets.Target_Type;
   Console : aliased MAT.Consoles.Text.Console_Type;
begin
   MAT.Configure_Logs (Debug => False, Dump => False, Verbose => False);

   Target.Console (Console'Unchecked_Access);
   Target.Initialize_Options;
   MAT.Commands.Initialize_Files (Target);
   Target.Start;
   Target.Interactive;
   Target.Stop;

exception
   when GNAT.Command_Line.Exit_From_Command_Line | GNAT.Command_Line.Invalid_Switch =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when Ada.IO_Exceptions.End_Error | MAT.Targets.Usage_Error =>
      Target.Stop;
end Mat.Main;
