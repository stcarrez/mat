-----------------------------------------------------------------------
--  Memory clients - Client info related to its memory
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
with MAT.Frames;
with Util.Events;
with MAT.Memory.Events;
package MAT.Memory.Targets is

   type Client_Memory is record
      Memory_Slots  : Allocation_Map;
      Frames        : MAT.Frames.Frame;
--        Event_Channel : MAT.Memory.Events.Memory_Event_Channel.Channel;
   end record;
   type Client_Memory_Ref is access all Client_Memory;

--     procedure Create_Instance (Refs : in ClientInfo_Ref_Map);

   procedure Init;
end MAT.Memory.Targets;
