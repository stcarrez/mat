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
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with System;

with Util.Properties;
with Util.Streams.Buffered;

with MAT.Types;
with MAT.Events;
with MAT.Events.Targets;
package MAT.Readers is

   type Buffer_Type is private;
   type Buffer_Ptr is access all Buffer_Type;

   type Message is record
      Kind   : MAT.Events.Event_Type;
      Size   : Natural;
      Buffer : Buffer_Ptr;
   end record;

   -----------------
   --  Abstract servant definition
   -----------------
   --  The Servant is a small proxy that binds the specific message
   --  handlers to the client specific dispatcher.
   type Reader_Base is abstract tagged limited private;
   type Reader_Access is access all Reader_Base'Class;

   procedure Dispatch (For_Servant : in out Reader_Base;
                       Id          : in MAT.Events.Internal_Reference;
                       Params      : in MAT.Events.Const_Attribute_Table_Access;
                       Frame       : in MAT.Events.Frame_Info;
                       Msg         : in out Message) is abstract;

   -----------------
   --  Ipc Client Manager
   -----------------
   --  The Manager is a kind of object adapter. It registers a collection
   --  of servants and dispatches incomming messages to those servants.
   type Manager_Base is abstract tagged limited private;
   type Manager is access all Manager_Base'Class;

   --  Register the reader to handle the event identified by the given name.
   --  The event is mapped to the given id and the attributes table is used
   --  to setup the mapping from the data stream attributes.
   procedure Register_Reader (Into   : in out Manager_Base;
                              Reader : in Reader_Access;
                              Name   : in String;
                              Id     : in MAT.Events.Internal_Reference;
                              Model  : in MAT.Events.Const_Attribute_Table_Access);

   procedure Dispatch_Message (Client : in out Manager_Base;
                               Msg    : in out Message);

   --  Read a message from the stream.
   procedure Read_Message (Client : in out Manager_Base;
                           Msg    : in out Message) is abstract;

   --  Read a list of event definitions from the stream and configure the reader.
   procedure Read_Event_Definitions (Client : in out Manager_Base;
                                     Msg    : in out Message);

   --  Set the target events.
   procedure Set_Target_Events (Client : in out Manager_Base;
                                Events : out MAT.Events.Targets.Target_Events_Access);

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

   type Reader_Base is abstract tagged limited record
      Owner : Manager := null;
   end record;

   --  Record a servant
   type Message_Handler is record
      For_Servant : Reader_Access;
      Id          : MAT.Events.Internal_Reference;
      Attributes  : MAT.Events.Const_Attribute_Table_Access;
      Mapping     : MAT.Events.Attribute_Table_Ptr;
   end record;

   function Hash (Key : in MAT.Types.Uint16) return Ada.Containers.Hash_Type;

   use type MAT.Types.Uint16;

   package Reader_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Message_Handler,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=");

   --  Runtime handlers associated with the events.
   package Handler_Maps is
     new Ada.Containers.Hashed_Maps (Key_Type     => MAT.Types.Uint16,
                                     Element_Type => Message_Handler,
                                     Hash         => Hash,
                                     Equivalent_Keys => "=");

   type Manager_Base is abstract tagged limited record
      Readers     : Reader_Maps.Map;
      Handlers    : Handler_Maps.Map;
      Version     : MAT.Types.Uint16;
      Flags       : MAT.Types.Uint16;
      Probe       : MAT.Events.Attribute_Table_Ptr;
      Frame       : access MAT.Events.Frame_Info;
      Events      : aliased MAT.Events.Targets.Target_Events;
   end record;

   --  Read the event data stream headers with the event description.
   --  Configure the reader to analyze the data stream according to the event descriptions.
   procedure Read_Headers (Client : in out Manager_Base;
                           Msg    : in out Message);

   --  Read an event definition from the stream and configure the reader.
   procedure Read_Definition (Client : in out Manager_Base;
                              Msg    : in out Message);

end MAT.Readers;
