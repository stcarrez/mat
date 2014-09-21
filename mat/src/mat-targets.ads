-----------------------------------------------------------------------
--  Targets - Representation of target information
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
with MAT.Types;
with Ada.Strings.Unbounded;
with MAT.Memory.Targets;
with MAT.Readers;
package MAT.Targets is

   type Target_Type is tagged limited private;
   type Target_Type_Access is access all Target_Type'Class;

   --  Initialize the target object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory and other events.
   procedure Initialize (Target : in out Target_Type;
                         Reader : in out MAT.Readers.Manager_Base'Class);

private

   type Target_Type is tagged limited record
      Memory : MAT.Memory.Targets.Target_Memory;
   end record;

end MAT.Targets;
