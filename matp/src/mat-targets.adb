-----------------------------------------------------------------------
--  Clients - Abstract representation of client information
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
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Unchecked_Deallocation;

with GNAT.Command_Line;

with Readline;

with Util.Strings;
with Util.Log.Loggers;

with MAT.Commands;
with MAT.Targets.Probes;
package body MAT.Targets is

   procedure Free is
     new Ada.Unchecked_Deallocation (MAT.Events.Targets.Target_Events'Class,
                                     MAT.Events.Targets.Target_Events_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Target_Process_Type'Class,
                                     Target_Process_Type_Access);

   --  ------------------------------
   --  Release the target process instance.
   --  ------------------------------
   overriding
   procedure Finalize (Target : in out Target_Process_Type) is
   begin
      Free (Target.Events);
   end Finalize;

   --  ------------------------------
   --  Find the region that matches the given name.
   --  ------------------------------
   overriding
   function Find_Region (Resolver : in Target_Process_Type;
                         Name     : in String) return MAT.Memory.Region_Info is
   begin
      return Resolver.Memory.Find_Region (Name);
   end Find_Region;

   --  ------------------------------
   --  Find the symbol in the symbol table and return the start and end address.
   --  ------------------------------
   overriding
   function Find_Symbol (Resolver : in Target_Process_Type;
                         Name     : in String) return MAT.Memory.Region_Info is
      Region : MAT.Memory.Region_Info;
   begin
      if Resolver.Symbols.Is_Null then
         raise MAT.Memory.Targets.Not_Found;
      end if;
      Resolver.Symbols.Value.Find_Symbol_Range (Name, Region.Start_Addr, Region.End_Addr);
      return Region;
   end Find_Symbol;

   --  ------------------------------
   --  Get the console instance.
   --  ------------------------------
   function Console (Target : in Target_Type) return MAT.Consoles.Console_Access is
   begin
      return Target.Console;
   end Console;

   --  ------------------------------
   --  Set the console instance.
   --  ------------------------------
   procedure Console (Target  : in out Target_Type;
                      Console : in MAT.Consoles.Console_Access) is
   begin
      Target.Console := Console;
   end Console;

   --  ------------------------------
   --  Get the current process instance.
   --  ------------------------------
   function Process (Target : in Target_Type) return Target_Process_Type_Access is
   begin
      return Target.Current;
   end Process;

   --  ------------------------------
   --  Initialize the target object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory and other events.
   --  ------------------------------
   procedure Initialize (Target : in out Target_Type;
                         Reader : in out MAT.Events.Probes.Probe_Manager_Type'Class) is
   begin
      MAT.Targets.Probes.Initialize (Target  => Target,
                                     Manager => Reader);
   end Initialize;

   --  ------------------------------
   --  Create a process instance to hold and keep track of memory and other information about
   --  the given process ID.
   --  ------------------------------
   procedure Create_Process (Target  : in out Target_Type;
                             Pid     : in MAT.Types.Target_Process_Ref;
                             Path    : in Ada.Strings.Unbounded.Unbounded_String;
                             Process : out Target_Process_Type_Access) is
      Path_String : constant String := Ada.Strings.Unbounded.To_String (Path);
   begin
      Process := Target.Find_Process (Pid);
      if Process = null then
         Process := new Target_Process_Type;
         Process.Pid := Pid;
         Process.Path := Path;
         Process.Symbols := MAT.Symbols.Targets.Target_Symbols_Refs.Create;
         Process.Symbols.Value.Console := Target.Console;
         Target.Processes.Insert (Pid, Process);
         Target.Console.Notice (MAT.Consoles.N_PID_INFO,
                                "Process" & MAT.Types.Target_Process_Ref'Image (Pid) & " created");
         Target.Console.Notice (MAT.Consoles.N_PATH_INFO,
                                "Path " & Path_String);
      end if;
      if Target.Current = null then
         Target.Current := Process;
      end if;
   end Create_Process;

   --  ------------------------------
   --  Find the process instance from the process ID.
   --  ------------------------------
   function Find_Process (Target : in Target_Type;
                          Pid    : in MAT.Types.Target_Process_Ref)
                          return Target_Process_Type_Access is
      Pos : constant Process_Cursor := Target.Processes.Find (Pid);
   begin
      if Process_Maps.Has_Element (Pos) then
         return Process_Maps.Element (Pos);
      else
         return null;
      end if;
   end Find_Process;

   --  ------------------------------
   --  Iterate over the list of connected processes and execute the <tt>Process</tt> procedure.
   --  ------------------------------
   procedure Iterator (Target  : in Target_Type;
                       Process : access procedure (Proc : in Target_Process_Type'Class)) is
      Iter : Process_Cursor := Target.Processes.First;
   begin
      while Process_Maps.Has_Element (Iter) loop
         Process (Process_Maps.Element (Iter).all);
         Process_Maps.Next (Iter);
      end loop;
   end Iterator;

   --  ------------------------------
   --  Convert the string to a socket address.  The string can have two forms:
   --     port
   --     host:port
   --  ------------------------------
   function To_Sock_Addr_Type (Param : in String) return GNAT.Sockets.Sock_Addr_Type is
      Pos    : constant Natural := Util.Strings.Index (Param, ':');
      Result : GNAT.Sockets.Sock_Addr_Type;
   begin
      if Pos > 0 then
         Result.Port := GNAT.Sockets.Port_Type'Value (Param (Pos + 1 .. Param'Last));
         Result.Addr := GNAT.Sockets.Inet_Addr (Param (Param'First .. Pos - 1));
      else
         Result.Port := GNAT.Sockets.Port_Type'Value (Param);
         Result.Addr := GNAT.Sockets.Any_Inet_Addr;
      end if;
      return Result;
   end To_Sock_Addr_Type;

   --  ------------------------------
   --  Print the application usage.
   --  ------------------------------
   procedure Usage is
      use Ada.Text_IO;
   begin
      Put_Line ("Usage: mat [-i] [-e] [-nw] [-ns] [-b [ip:]port] [file.mat]");
      Put_Line ("-i            Enable the interactive mode");
      Put_Line ("-e            Print the probe events as they are received");
      Put_Line ("-nw           Disable the graphical mode");
      Put_Line ("-b [ip:]port  Define the port and local address to bind");
      Put_Line ("-ns           Disable the automatic symbols loading");
      Ada.Command_Line.Set_Exit_Status (2);
      raise Usage_Error;
   end Usage;

   --  ------------------------------
   --  Parse the command line arguments and configure the target instance.
   --  ------------------------------
   procedure Initialize_Options (Target  : in out MAT.Targets.Target_Type) is
   begin
      Util.Log.Loggers.Initialize ("matp.properties");
      GNAT.Command_Line.Initialize_Option_Scan (Stop_At_First_Non_Switch => True,
                                                Section_Delimiters       => "targs");
      loop
         case GNAT.Command_Line.Getopt ("i e nw ns b:") is
            when ASCII.NUL =>
               exit;

            when 'i' =>
               Target.Options.Interactive := True;

            when 'e' =>
               Target.Options.Print_Events := True;

            when 'b' =>
               Target.Options.Address := To_Sock_Addr_Type (GNAT.Command_Line.Parameter);

            when 'n' =>
               if GNAT.Command_Line.Full_Switch = "nw" then
                  Target.Options.Graphical := False;
               else
                  Target.Options.Load_Symbols := False;
               end if;

            when '*' =>
               exit;

            when others =>
               Usage;

         end case;
      end loop;

   exception
      when Usage_Error =>
         raise;

      when others =>
         Usage;

   end Initialize_Options;

   --  ------------------------------
   --  Enter in the interactive loop reading the commands from the standard input
   --  and executing the commands.
   --  ------------------------------
   procedure Interactive (Target : in out MAT.Targets.Target_Type) is
   begin
      loop
         declare
            Line : constant String := Readline.Get_Line ("matp>");
         begin
            MAT.Commands.Execute (Target, Line);

         exception
            when MAT.Commands.Stop_Interp =>
               exit;
         end;
      end loop;
   end Interactive;

   --  ------------------------------
   --  Start the server to listen to MAT event socket streams.
   --  ------------------------------
   procedure Start (Target : in out Target_Type) is
   begin
      Target.Server.Start (Target'Unchecked_Access, Target.Options.Address);
   end Start;

   --  ------------------------------
   --  Stop the server thread.
   --  ------------------------------
   procedure Stop (Target : in out Target_Type) is
   begin
      Target.Server.Stop;
   end Stop;

   --  ------------------------------
   --  Release the storage used by the target object.
   --  ------------------------------
   overriding
   procedure Finalize (Target : in out Target_Type) is
   begin
      while not Target.Processes.Is_Empty loop
         declare
            Process : Target_Process_Type_Access := Target.Processes.First_Element;
         begin
            Free (Process);
            Target.Processes.Delete_First;
         end;
      end loop;
   end Finalize;


end MAT.Targets;
