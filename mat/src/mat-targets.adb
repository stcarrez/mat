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
                             Process : out Target_Process_Type_Access) is
   begin
      Process := Target.Find_Process (Pid);
      if Process = null then
         Process := new Target_Process_Type;
         Process.Pid := Pid;
         Process.Symbols := MAT.Symbols.Targets.Target_Symbols_Refs.Create;
         Target.Processes.Insert (Pid, Process);
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

end MAT.Targets;
