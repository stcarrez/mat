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
with MAT.Types;
with MAT.Events;
--  with MAT.Ipc;
with Util.Events;
with MAT.Readers;
with MAT.Memory.Targets;
--  with MAT.Memory.Clients;
--  with MAT.Ipc.Clients;
--  with MAT.Events; use MAT.Events;
package MAT.Memory.Readers is

   type Memory_Servant is new MAT.Readers.Reader_Base with record
      Data  : MAT.Memory.Targets.Client_Memory;
--        Slots : Client_Memory_Ref;
--        Impl        : Client_Memory_Ref;
   end record;
   --  The memory servant is a proxy for the generic communication
   --  channel to process incomming events (such as memory allocations).

   overriding
   procedure Dispatch (For_Servant : in out Memory_Servant;
                       Id          : in MAT.Events.Internal_Reference;
                       Params      : in MAT.Events.Const_Attribute_Table_Access;
                       Msg         : in out MAT.Readers.Message);

   procedure Bind (For_Servant : in out Memory_Servant);
   --  Bind the servant with the object adapter to register the
   --  events it recognizes.

end MAT.Memory.Readers;
