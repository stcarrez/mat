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

with MAT.Types;
with MAT.Memory.Targets;
with MAT.Symbols.Targets;
with MAT.Readers;
with MAT.Consoles;
package MAT.Targets is

   type Target_Process_Type is tagged limited record
      Pid     : MAT.Types.Target_Process_Ref;
      Memory  : MAT.Memory.Targets.Target_Memory;
      Console : MAT.Consoles.Console_Access;
   end record;
   type Target_Process_Type_Access is access all Target_Process_Type'Class;

   type Target_Type is tagged limited record
      Pid     : MAT.Types.Target_Process_Ref;
      Memory  : MAT.Memory.Targets.Target_Memory;
      Symbols : MAT.Symbols.Targets.Target_Symbols;
      Console : MAT.Consoles.Console_Access;
   end record;

   type Target_Type_Access is access all Target_Type'Class;

   --  Initialize the target object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory and other events.
   procedure Initialize (Target : in out Target_Type;
                         Reader : in out MAT.Readers.Manager_Base'Class);

   --  Create a process instance to hold and keep track of memory and other information about
   --  the given process ID.
   procedure Create_Process (Target  : in out Target_Type;
                             Pid     : in MAT.Types.Target_Process_Ref;
                             Process : out Target_Process_Type_Access);

   --  Find the process instance from the process ID.
   function Find_Process (Target : in Target_Type;
                          Pid    : in MAT.Types.Target_Process_Ref)
                          return Target_Process_Type_Access;

private

   --  Define a map of <tt>Target_Process_Type_Access</tt> keyed by the process Id.
   --  This map allows to retrieve the information about a process.
   use type MAT.Types.Target_Process_Ref;
   package Process_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Process_Ref,
                                      Element_Type => Target_Process_Type_Access);

   subtype Process_Map is Process_Maps.Map;
   subtype Process_Cursor is Process_Maps.Cursor;

end MAT.Targets;
