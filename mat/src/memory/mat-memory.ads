-----------------------------------------------------------------------
--  Memory - Memory slot
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
with Ada.Containers.Ordered_Maps;

with MAT.Types;
with MAT.Frames;

with Interfaces;
package MAT.Memory is

   type Allocation is record
      Size   : MAT.Types.Target_Size;
      Frame  : Frames.Frame_Type;
      Time   : MAT.Types.Target_Tick_Ref;
      Thread : MAT.Types.Target_Thread_Ref;
   end record;

   use type MAT.Types.Target_Addr;
   package Allocation_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Addr,
                                      Element_Type => Allocation);

   subtype Allocation_Map is Allocation_Maps.Map;
   subtype Allocation_Cursor is Allocation_Maps.Cursor;

private

end MAT.Memory;
