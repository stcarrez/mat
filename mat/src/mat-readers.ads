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

with System;

with Util.Properties;

with MAT.Types;
with MAT.Events;
package MAT.Readers is

   type Buffer_Type is private;
   type Buffer_Ptr is access Buffer_Type;

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
                       Msg         : in out Message) is abstract;
   --  Dispatch the message

   procedure Bind (For_Servant : in out Reader_Base) is abstract;
   --  Bind the servant with the object adapter to register the
   --  events it recognizes.  This is called once we have all the
   --  information about the structures of events that we can
   --  receive.

   -----------------
   --  Ipc Client Manager
   -----------------
   --  The Manager is a kind of object adapter. It registers a collection
   --  of servants and dispatches incomming messages to those servants.
   type Manager_Base is tagged limited private;
   type Manager is access all Manager_Base'Class;

   procedure Register_Servant (Adapter : in Manager;
                               Proxy   : in Reader_Access);
   --  Register the proxy servant in the object adapter.

--     function Get_Manager (Refs : in ClientInfo_Ref_Map) return Manager;
   --  Return the object adapter manager which holds all the servant
   --  for the client.

   procedure Register_Message_Analyzer (Proxy  : in Reader_Access;
                                        Name   : in String;
                                        Id     : in MAT.Events.Internal_Reference;
                                        Model  : in MAT.Events.Attribute_Table;
                                        Table  : out MAT.Events.Attribute_Table_Ptr);
   --  Setup the servant to receive and process messages identified
   --  by Name.

   procedure Dispatch_Message (Client : in out Manager_Base;
                               Msg_Id : in MAT.Events.Internal_Reference;
                               Msg    : in out Message);

private

   type Buffer_Type is record
      Current : System.Address;
      Start   : System.Address;
      Last    : System.Address;
      Size    : Natural;
      Total   : Natural;
   end record;

   type Reader_Base is abstract tagged limited record
      Owner : Manager := null;
   end record;

   --  Record a servant
   type Message_Handler is record
      For_Servant : Reader_Access;
      Id          : MAT.Events.Internal_Reference;
   end record;

   function Hash (Key : in MAT.Events.Internal_Reference) return Ada.Containers.Hash_Type;

   use type MAT.Types.Uint32;

   --  Runtime handlers associated with the events.
   package Handler_Maps is
     new Ada.Containers.Hashed_Maps (Key_Type     => MAT.Events.Internal_Reference,
                                     Element_Type => Message_Handler,
                                     Hash         => Hash,
                                     Equivalent_Keys => "=");

   type Manager_Base is tagged limited record
      Handlers    : Handler_Maps.Map;
   end record;

end MAT.Readers;
