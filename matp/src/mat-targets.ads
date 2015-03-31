-----------------------------------------------------------------------
--  mat-targets - Representation of target information
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
with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Finalization;

with GNAT.Sockets;

with MAT.Types;
with MAT.Memory.Targets;
with MAT.Symbols.Targets;
with MAT.Events.Targets;
with MAT.Events.Probes;
with MAT.Readers;
with MAT.Readers.Streams;
with MAT.Readers.Streams.Sockets;
with MAT.Consoles;
package MAT.Targets is

   --  Exception raised if some option is invalid.
   Usage_Error : exception;

   --  The options that can be configured through the command line.
   type Options_Type is record
      --  Enable and enter in the interactive TTY console mode.
      Interactive  : Boolean := True;

      --  Try to load the symbol file automatically when a new process is received.
      Load_Symbols : Boolean := True;

      --  Enable the graphical mode (when available).
      Graphical    : Boolean := False;

      --  Print the events as they are received.
      Print_Events : Boolean := False;

      --  Define the server listening address.
      Address      : GNAT.Sockets.Sock_Addr_Type := (Port => 4096, others => <>);
   end record;

   type Target_Process_Type is tagged limited record
      Pid     : MAT.Types.Target_Process_Ref;
      Path    : Ada.Strings.Unbounded.Unbounded_String;
      Memory  : MAT.Memory.Targets.Target_Memory;
      Symbols : MAT.Symbols.Targets.Target_Symbols_Ref;
      Events  : MAT.Events.Targets.Target_Events_Access;
      Console : MAT.Consoles.Console_Access;
   end record;
   type Target_Process_Type_Access is access all Target_Process_Type'Class;

   type Target_Type is new Ada.Finalization.Limited_Controlled
     and MAT.Events.Probes.Reader_List_Type with private;

   type Target_Type_Access is access all Target_Type'Class;

   --  Get the console instance.
   function Console (Target : in Target_Type) return MAT.Consoles.Console_Access;

   --  Set the console instance.
   procedure Console (Target  : in out Target_Type;
                      Console : in MAT.Consoles.Console_Access);

   --  Get the current process instance.
   function Process (Target : in Target_Type) return Target_Process_Type_Access;

   --  Initialize the target object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory and other events.
   overriding
   procedure Initialize (Target : in out Target_Type;
                         Reader : in out MAT.Events.Probes.Probe_Manager_Type'Class);

   --  Create a process instance to hold and keep track of memory and other information about
   --  the given process ID.
   procedure Create_Process (Target  : in out Target_Type;
                             Pid     : in MAT.Types.Target_Process_Ref;
                             Path    : in Ada.Strings.Unbounded.Unbounded_String;
                             Process : out Target_Process_Type_Access);

   --  Find the process instance from the process ID.
   function Find_Process (Target : in Target_Type;
                          Pid    : in MAT.Types.Target_Process_Ref)
                          return Target_Process_Type_Access;

   --  Iterate over the list of connected processes and execute the <tt>Process</tt> procedure.
   procedure Iterator (Target  : in Target_Type;
                       Process : access procedure (Proc : in Target_Process_Type'Class));

   --  Parse the command line arguments and configure the target instance.
   procedure Initialize_Options (Target  : in out Target_Type);

   --  Enter in the interactive loop reading the commands from the standard input
   --  and executing the commands.
   procedure Interactive (Target : in out MAT.Targets.Target_Type);

   --  Start the server to listen to MAT event socket streams.
   procedure Start (Target : in out Target_Type);

   --  Stop the server thread.
   procedure Stop (Target : in out Target_Type);

   --  Convert the string to a socket address.  The string can have two forms:
   --     port
   --     host:port
   function To_Sock_Addr_Type (Param : in String) return GNAT.Sockets.Sock_Addr_Type;

   --  Print the application usage.
   procedure Usage;

private

   --  Define a map of <tt>Target_Process_Type_Access</tt> keyed by the process Id.
   --  This map allows to retrieve the information about a process.
   use type MAT.Types.Target_Process_Ref;
   package Process_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Process_Ref,
                                      Element_Type => Target_Process_Type_Access);

   subtype Process_Map is Process_Maps.Map;
   subtype Process_Cursor is Process_Maps.Cursor;

   type Target_Type is new Ada.Finalization.Limited_Controlled
     and MAT.Events.Probes.Reader_List_Type with record
      Current   : Target_Process_Type_Access;
      Processes : Process_Map;
      Console   : MAT.Consoles.Console_Access;
      Options   : Options_Type;
      Server    : MAT.Readers.Streams.Sockets.Socket_Listener_Type;
   end record;

end MAT.Targets;
