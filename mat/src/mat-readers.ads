-----------------------------------------------------------------------
--  mat-readers -- Reader
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
with Ada.Containers;
with Ada.Finalization;

with System;

with Util.Streams.Buffered;

with MAT.Types;
with MAT.Events;
with MAT.Events.Targets;
package MAT.Readers is

   type Buffer_Type is private;
   type Buffer_Ptr is access all Buffer_Type;

   type Message_Type is record
      Kind   : MAT.Events.Event_Type;
      Size   : Natural;
      Buffer : Buffer_Ptr;
   end record;
   subtype Message is Message_Type;

   type Reader_List_Type is limited interface;
   type Reader_List_Type_Access is access all Reader_List_Type'Class;

   --  Initialize the target object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory and other events.
   procedure Initialize (List   : in out Reader_List_Type;
                         Reader : in out Manager_Base'Class) is abstract;

private

   type Endian_Type is (BIG_ENDIAN, LITTLE_ENDIAN);

   type Buffer_Type is record
      Current : System.Address;
      Start   : System.Address;
      Last    : System.Address;
      Buffer  : Util.Streams.Buffered.Buffer_Access;
      Size    : Natural;
      Total   : Natural;
      Endian  : Endian_Type := LITTLE_ENDIAN;
   end record;

end MAT.Readers;
