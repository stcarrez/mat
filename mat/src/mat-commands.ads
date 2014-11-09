-----------------------------------------------------------------------
--  mat-commands -- Command support and execution
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
with GNAT.Sockets;

with MAT.Targets;
package MAT.Commands is

   Stop_Interp : exception;

   --  Exception raised if some option is invalid.
   Usage_Error : exception;

   --  The options that can be configured through the command line.
   type Options_Type is record
      --  Enable and enter in the interactive TTY console mode.
      Interactive  : Boolean := True;

      --  Try to load the symbol file automatically when a new process is recieved.
      Load_Symbols : Boolean := True;

      --  Enable the graphical mode (when available).
      Graphical    : Boolean := False;

      --  Define the server listening address.
      Address      : GNAT.Sockets.Sock_Addr_Type := (Port => 4096, others => <>);
   end record;

   --  Procedure that defines a command handler.
   type Command_Handler is access procedure (Target : in out MAT.Targets.Target_Type'Class;
                                             Args   : in String);

   --  Execute the command given in the line.
   procedure Execute (Target : in out MAT.Targets.Target_Type'Class;
                      Line   : in String);

   --  Enter in the interactive loop reading the commands from the standard input
   --  and executing the commands.
   procedure Interactive (Target : in out MAT.Targets.Target_Type'Class);

   --  Parse the command line arguments and configure the target instance.
   procedure Initialize_Options (Target  : in out MAT.Targets.Target_Type'Class;
                                 Options : in out Options_Type);

   --  Initialize the process targets by loading the MAT files.
   procedure Initialize_Files (Target  : in out MAT.Targets.Target_Type'Class);

   --  Convert the string to a socket address.  The string can have two forms:
   --     port
   --     host:port
   function To_Sock_Addr_Type (Param : in String) return GNAT.Sockets.Sock_Addr_Type;

   --  Print the application usage.
   procedure Usage;

   --  Symbol command.
   --  Load the symbols from the binary file.
   procedure Symbol_Command (Target : in out MAT.Targets.Target_Type'Class;
                             Args   : in String);

end MAT.Commands;
