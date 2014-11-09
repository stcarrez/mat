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

   --  The options that can be configured through the command line.
   type Options_Type is record
      Interactive : Boolean := True;
      Graphical   : Boolean := False;
      Address     : GNAT.Sockets.Sock_Addr_Type := (Port => 4096, others => <>);
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

end MAT.Commands;
