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
package body MAT.Targets is

   --  ------------------------------
   --  Initialize the target object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory and other events.
   --  ------------------------------
   procedure Initialize (Target : in out Target_Type;
                         Reader : in out MAT.Readers.Manager_Base'Class) is
   begin
      MAT.Memory.Targets.Initialize (Memory => Target.Memory,
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
      Process := new Target_Process_Type;
      Process.Pid := Pid;
   end Create_Process;

end MAT.Targets;
