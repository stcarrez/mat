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
with MAT.Events;
with MAT.Readers;
with MAT.Memory.Targets;
package MAT.Memory.Readers is

   type Memory_Servant is new MAT.Readers.Reader_Base with record
      Data  : access MAT.Memory.Targets.Target_Memory;
   end record;
   type Memory_Reader_Access is access all Memory_Servant'Class;
   --  The memory servant is a proxy for the generic communication
   --  channel to process incomming events (such as memory allocations).

   overriding
   procedure Dispatch (For_Servant : in out Memory_Servant;
                       Id          : in MAT.Events.Internal_Reference;
                       Params      : in MAT.Events.Const_Attribute_Table_Access;
                       Frame       : in MAT.Events.Frame_Info;
                       Msg         : in out MAT.Readers.Message);

   --  Register the reader to extract and analyze memory events.
   procedure Register (Into   : in out MAT.Readers.Manager_Base'Class;
                       Reader : in Memory_Reader_Access);

end MAT.Memory.Readers;
