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


with MAT.Events;
with MAT.Types;
with MAT.Readers.Marshaller;
with Interfaces;
package body MAT.Readers is

   function Hash (Key : in MAT.Events.Internal_Reference) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Hash;

--
--     function "<" (Left, Right : String_Ptr) return Boolean;
--     package Event_Def_Containers is
--       new BC.Containers (Item => Event_Description_Ptr);
--
--     package Event_Def_Trees is new Event_Def_Containers.Trees;
--
--     package Event_Def_AVL is
--       new Event_Def_Trees.AVL (Key => String_Ptr,
--                                Storage => Global_Heap.Storage);
   --  Logical description of events received by the analysis tool.

   type IpcManager_Base is new Manager_Base with record
      Handlers    : Handler_Maps.Map;
--        Event_Types : Event_Def_AVL.AVL_Tree;
   end record;
   type IpcManager is access all IpcManager_Base'Class;

   type ClientInfo is new MAT.Clients.ClientInfo with record
      Ipc_Channel : Object_Ptr;
      Adapter     : aliased IpcManager_Base;
   end record;
   type ClientInfo_Ptr is access all ClientInfo;
--
--     function "<" (Left, Right : String_Ptr) return Boolean is
--     begin
--        return Left.all < Right.all;
--     end "<";

   --  Return the object adapter manager which holds all the servant
   --  for the client.
--
--     procedure Create_Instance (Refs : in ClientInfo_Ref_Map) is
--        Adapter : Manager       := Get_Manager (Refs);
--        --Client  : Client_Memory := new Client_Memory;
--     begin
--        -- Insert (Refs, "memory", Client);
--        -- Register_Servant (Adapter, Proxy);
--        null;
--     end Create_Instance;


--     function Get_Manager (Refs : in ClientInfo_Ref_Map) return Manager is
--        Info : MAT.Clients.ClientInfo_Ref := Find (Refs, "com-channel");
--     begin
--        if Info.all not in ClientInfo'Class then
--           raise PROGRAM_ERROR;
--        end if;
--        declare
--           Channel : ClientInfo_Ptr := ClientInfo (Info.all)'Access;
--        begin
--           return Channel.Adapter'Access;
--        end;
--     end Get_Manager;

   procedure Register_Servant (Adapter : in Manager;
                               Proxy : in Servant) is
   begin
      if Proxy.Owner /= null then
         raise PROGRAM_ERROR;
      end if;
      Proxy.Owner := Adapter;
   end Register_Servant;

   procedure Register_Message_Analyzer (Proxy  : in Reader_Access;
                                        Name   : in String;
                                        Id     : in MAT.Events.Internal_Reference;
                                        Model  : in MAT.Events.Attribute_Table;
                                        Table  : out MAT.Events.Attribute_Table_Ptr) is
      Handler : Message_Handler := (For_Servant => Proxy, Id => Id);
      Adapter : IpcManager := IpcManager_Base (Proxy.Owner.all)'Access;
      N : String_Ptr := new String'(Name);
      It      : Event_Def_AVL.Iterator   := Find (adapter.Event_Types, N);
   begin
      if Is_Done (It) then
         return;
      end if;

      declare
         Event_Def : Event_Description_Ptr := Current_Item (It);
         Inserted  : Boolean;
      begin
         Insert (T => Adapter.Handlers,
                 Element => Handler,
                 The_Key => Event_Def.Id,
                 Not_Found => Inserted);

         Table := new Attribute_Table (1 .. Event_Def.Nb_Attributes);
         Table (Table'Range) := Event_Def.Def (Table'Range);
         for I in Table'Range loop
            Table (I).Ref := 0;
            for J in Model'Range loop
               if Table (I).Name.all = Model (I).Name.all then
                  Table (I).Ref := Model (I).Ref;
                  exit;
               end if;
            end loop;
         end loop;
      end;
   end Register_Message_Analyzer;

   procedure Dispatch_Message (Client : in out Manager_Base;
                               Msg    : in out Message) is
      Event : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Pos   : constant Handler_Maps.Cursor := Client.Handlers.Find (Event);
   begin
      if Handler_Maps.Has_Element (Pos) then
         --  Message is not handled, skip it.
         null;
      else
         declare
            Handler : constant Message_Handler := Handler_Maps.Element (Pos);
         begin
            Dispatch (Handler.For_Servant.all, Handler.Id, Handler.Mapping, Msg);
         end;
      end if;
   end Dispatch_Message;

   procedure Read_Headers (Client : in out Manager_Base;
                           Msg    : in out Message) is
      Count : MAT.Types.Uint16;

   begin
      Client.Version := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Client.Flags   := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Count := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      for I in 1 .. Count loop
         declare
            Name  : constant String := MAT.Readers.Marshaller.Get_String (Msg.Buffer);
            Event : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
            Cnt   : constant MAT.Types.Uint8 := MAT.Readers.Marshaller.Get_Uint8 (Msg.Buffer);
            Pos   : constant Cursor := Client.Readers.Find (Name);

            procedure Read_Attribute_Definition (Element : in out Event_Description) is
               Name : constant String := MAT.Readers.Marshaller.Get_String (Msg.Buffer);
               Size : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
            begin
            end Read_Attribute_Definition;

         begin
            if Has_Element (Pos) then
               Client.Readers.Update_Element (Pos, Read_Attribute_Definition'Access);
            end if;
         end;
      end loop;
   end Read_Headers;

end MAT.Readers;
