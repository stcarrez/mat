-----------------------------------------------------------------------
--  Memory Events - Definition and Analysis of memory events
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
with MAT.Memory.Readers;
package body MAT.Memory.Targets is


   --  Initialize the target memory object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory events.
   procedure Initialize (Memory : in out Target_Memory;
                         Reader : in out MAT.Readers.Manager_Base'Class) is
      Memory_Reader : constant MAT.Memory.Readers.Memory_Reader_Access
        := new MAT.Memory.Readers.Memory_Servant;
   begin
      Memory.Reader := Memory_Reader.all'Access;
      MAT.Memory.Readers.Register (Reader, Memory_Reader);
   end Initialize;

end MAT.Memory.Targets;
