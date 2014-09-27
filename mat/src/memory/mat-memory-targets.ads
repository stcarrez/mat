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
with MAT.Readers;
with Ada.Containers.Ordered_Maps;
package MAT.Memory.Targets is

   type Target_Memory is record
      Reader        : MAT.Readers.Reader_Access;
      Memory_Slots  : Allocation_Map;
      Frames        : MAT.Frames.Frame_Ptr;
--        Event_Channel : MAT.Memory.Events.Memory_Event_Channel.Channel;
   end record;
   type Client_Memory_Ref is access all Target_Memory;

   --  Initialize the target memory object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory events.
   procedure Initialize (Memory : in out Target_Memory;
                         Reader : in out MAT.Readers.Manager_Base'Class);

   type Size_Info_Type is record
      Count : Natural;
   end record;

   use type MAT.Types.Target_Size;

   package Size_Info_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type => MAT.Types.Target_Size,
                                      Element_Type => Size_Info_Type);
   subtype Size_Info_Map is Size_Info_Maps.Map;
   subtype Size_Info_Cursor is Size_Info_Maps.Cursor;

   --  Collect the information about memory slot sizes allocated by the application.
   procedure Size_Information (Memory : in Target_Memory;
                               Sizes  : in out Size_Info_Map);

end MAT.Memory.Targets;
