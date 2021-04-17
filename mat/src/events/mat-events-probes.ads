-----------------------------------------------------------------------
--  mat-events-probes -- Event probes
--  Copyright (C) 2014, 2015 Stephane Carrez
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
with Ada.Finalization;

with MAT.Types;
with MAT.Events;
with MAT.Events.Targets;
with MAT.Readers;
with MAT.Frames.Targets;
package MAT.Events.Probes is

   -----------------
   --  Abstract probe definition
   -----------------
   type Probe_Type is abstract tagged limited private;
   type Probe_Type_Access is access all Probe_Type'Class;

   --  Extract the probe information from the message.
   procedure Extract (Probe  : in Probe_Type;
                      Params : in MAT.Events.Const_Attribute_Table_Access;
                      Msg    : in out MAT.Readers.Message_Type;
                      Event  : in out Target_Event_Type) is abstract;

   procedure Execute (Probe : in Probe_Type;
                      Event : in out Target_Event_Type) is abstract;

   --  Update the Size and Prev_Id information in the event identified by <tt>Id</tt>.
   --  Update the event represented by <tt>Prev_Id</tt> so that its Next_Id refers
   --  to the <tt>Id</tt> event.
   procedure Update_Event (Probe   : in Probe_Type;
                           Id      : in MAT.Events.Event_Id_Type;
                           Size    : in MAT.Types.Target_Size;
                           Prev_Id : in MAT.Events.Event_Id_Type);

   -----------------
   --  Probe Manager
   -----------------
   type Probe_Manager_Type is abstract new Ada.Finalization.Limited_Controlled with private;
   type Probe_Manager_Type_Access is access all Probe_Manager_Type'Class;

   --  Initialize the probe manager instance.
   overriding
   procedure Initialize (Manager : in out Probe_Manager_Type);

   --  Register the probe to handle the event identified by the given name.
   --  The event is mapped to the given id and the attributes table is used
   --  to setup the mapping from the data stream attributes.
   procedure Register_Probe (Into   : in out Probe_Manager_Type;
                             Probe  : in Probe_Type_Access;
                             Name   : in String;
                             Id     : in MAT.Events.Probe_Index_Type;
                             Model  : in MAT.Events.Const_Attribute_Table_Access);

   procedure Dispatch_Message (Client : in out Probe_Manager_Type;
                               Msg    : in out MAT.Readers.Message_Type);

   --  Read a list of event definitions from the stream and configure the reader.
   procedure Read_Event_Definitions (Client : in out Probe_Manager_Type;
                                     Msg    : in out MAT.Readers.Message_Type);

   --  Get the target events.
   function Get_Target_Events (Client : in Probe_Manager_Type)
                               return MAT.Events.Targets.Target_Events_Access;

   --  Get the target frames.
   function Get_Target_Frames (Client : in Probe_Manager_Type)
                               return MAT.Frames.Targets.Target_Frames_Access;

   --  Read a message from the stream.
   procedure Read_Message (Reader : in out Probe_Manager_Type;
                           Msg    : in out MAT.Readers.Message_Type) is abstract;

   --  Get the size of a target address (4 or 8 bytes).
   function Get_Address_Size (Client : in Probe_Manager_Type) return MAT.Events.Attribute_Type;

   type Reader_List_Type is limited interface;
   type Reader_List_Type_Access is access all Reader_List_Type'Class;

   --  Initialize the target object to manage the memory slots, the stack frames
   --  and setup the reader to analyze the memory and other events.
   procedure Initialize (List    : in out Reader_List_Type;
                         Manager : in out Probe_Manager_Type'Class) is abstract;

private

   type Probe_Type is abstract tagged limited record
      Owner : Probe_Manager_Type_Access := null;
   end record;

   --  Record a probe
   type Probe_Handler is record
      Probe       : Probe_Type_Access;
      Id          : MAT.Events.Probe_Index_Type;
      Attributes  : MAT.Events.Const_Attribute_Table_Access;
      Mapping     : MAT.Events.Attribute_Table_Ptr;
   end record;

   function Hash (Key : in MAT.Types.Uint16) return Ada.Containers.Hash_Type;

   package Probe_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Probe_Handler,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=");

   --  Runtime handlers associated with the events.
   package Handler_Maps is
     new Ada.Containers.Hashed_Maps (Key_Type     => MAT.Types.Uint16,
                                     Element_Type => Probe_Handler,
                                     Hash         => Hash,
                                     Equivalent_Keys => "=");

   type Probe_Manager_Type is abstract new Ada.Finalization.Limited_Controlled with record
      Probes      : Probe_Maps.Map;
      Handlers    : Handler_Maps.Map;
      Version     : MAT.Types.Uint16;
      Flags       : MAT.Types.Uint16;
      Addr_Size   : MAT.Events.Attribute_Type;
      Probe       : MAT.Events.Attribute_Table_Ptr;
      Frame       : MAT.Events.Frame_Info_Access;
      Events      : MAT.Events.Targets.Target_Events_Access;
      Event       : Target_Event_Type;
      Frames      : MAT.Frames.Targets.Target_Frames_Access;
   end record;

   --  Read an event definition from the stream and configure the reader.
   procedure Read_Definition (Client : in out Probe_Manager_Type;
                              Msg    : in out MAT.Readers.Message);

end MAT.Events.Probes;
