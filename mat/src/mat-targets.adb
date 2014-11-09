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

with GNAT.Command_Line;

with Util.Strings;
with Util.Log.Loggers;

with MAT.Commands;
with MAT.Targets.Readers;
package body MAT.Targets is

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
                         Reader : in out MAT.Readers.Manager_Base'Class) is
   begin
      MAT.Targets.Readers.Initialize (Target => Target,
                                      Reader => Reader);
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
         Target.Processes.Insert (Pid, Process);
         Target.Console.Notice (MAT.Consoles.N_PID_INFO,
                                "Process" & MAT.Types.Target_Process_Ref'Image (Pid) & " created");
         Target.Console.Notice (MAT.Consoles.N_PATH_INFO,
                                "Path " & Path_String);
         if Target.Options.Load_Symbols then
            MAT.Commands.Symbol_Command (Target, Path_String);
         end if;
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
      Put_Line ("Usage: mat [-i] [-nw] [-ns] [-b [ip:]port] [file.mat]");
      Put_Line ("-i            Enable the interactive mode");
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
         case GNAT.Command_Line.Getopt ("i nw ns b:") is
            when ASCII.NUL =>
               exit;

            when 'i' =>
               Target.Options.Interactive := True;

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
   --  Start the server to listen to MAT event socket streams.
   --  ------------------------------
   procedure Start (Target : in out Target_Type) is
   begin
      Target.Server.Start (Target.Options.Address);
   end Start;

end MAT.Targets;
