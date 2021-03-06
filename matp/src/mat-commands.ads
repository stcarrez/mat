-----------------------------------------------------------------------
--  mat-commands -- Command support and execution
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

with MAT.Targets;
package MAT.Commands is

   --  Exception raised to stop the program.
   Stop_Interp  : exception;

   --  Exception raised to stop the command (program continues).
   Stop_Command : exception;

   --  Exception raised by a command when a filter expression is invalid.
   Filter_Error : exception;

   --  Exception raised by a command when some command argument is invalid.
   Usage_Error : exception;

   --  Procedure that defines a command handler.
   type Command_Handler is access procedure (Target : in out MAT.Targets.Target_Type'Class;
                                             Args   : in String);

   --  Execute the command given in the line.
   procedure Execute (Target : in out MAT.Targets.Target_Type'Class;
                      Line   : in String);

   --  Initialize the process targets by loading the MAT files.
   procedure Initialize_Files (Target  : in out MAT.Targets.Target_Type'Class);

   --  Symbol command.
   --  Load the symbols from the binary file.
   procedure Symbol_Command (Target : in out MAT.Targets.Target_Type'Class;
                             Args   : in String);

   --  Sizes command.
   --  Collect statistics about the used memory slots and report the different slot
   --  sizes with count.
   procedure Sizes_Command (Target : in out MAT.Targets.Target_Type'Class;
                            Args   : in String);

   --  Threads command.
   --  Collect statistics about the threads and their allocation.
   procedure Threads_Command (Target : in out MAT.Targets.Target_Type'Class;
                              Args   : in String);

   --  Events command.
   --  Print the probe events.
   procedure Events_Command (Target : in out MAT.Targets.Target_Type'Class;
                             Args   : in String);

   --  Event command.
   --  Print the probe event with the stack frame.
   procedure Event_Command (Target : in out MAT.Targets.Target_Type'Class;
                            Args   : in String);

   --  Info command to print symmary information about the program.
   procedure Info_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);

   --  Timeline command.
   --  Identify the interesting timeline groups in the events and display them.
   procedure Timeline_Command (Target : in out MAT.Targets.Target_Type'Class;
                               Args   : in String);

   --  Addr command to print a description of an address.
   procedure Addr_Command (Target : in out MAT.Targets.Target_Type'Class;
                           Args   : in String);

end MAT.Commands;
