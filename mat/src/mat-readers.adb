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
with Util.Log.Loggers;

with MAT.Events;
with MAT.Types;
with MAT.Readers.Marshaller;
with Interfaces;
package body MAT.Readers is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Readers");

   function Hash (Key : in MAT.Types.Uint16) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Hash;

   procedure Register_Servant (Adapter : in Manager;
                               Proxy : in Servant) is
   begin
      if Proxy.Owner /= null then
         raise PROGRAM_ERROR;
      end if;
      Proxy.Owner := Adapter;
   end Register_Servant;

   procedure Register_Reader (Into  : in out Manager_Base;
                              Name  : in String;
                              Id    : in MAT.Events.Internal_Reference;
                              Model : in MAT.Events.Attribute_Table_Ptr) is
      Handler : Message_Handler;
   begin
      Handler.Id := Id;
      Handler.Attributes  := Model;
      Handler.Mapping     := new MAT.Events.Attribute_Table (Model'Range);
      Handler.Mapping.all := Model.all;
      Into.Readers.Insert (Name, Handler);
   end Register_Reader;

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
            Dispatch (Handler.For_Servant.all, Handler.Id, Handler.Mapping.all'Access, Msg);
         end;
      end if;
   end Dispatch_Message;

   --  ------------------------------
   --  Read an event definition from the stream and configure the reader.
   --  ------------------------------
   procedure Read_Definition (Client : in out Manager_Base;
                              Msg    : in out Message) is
      Name  : constant String := MAT.Readers.Marshaller.Get_String (Msg.Buffer);
      Event : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Count : constant MAT.Types.Uint8 := MAT.Readers.Marshaller.Get_Uint8 (Msg.Buffer);
      Pos   : constant Reader_Maps.Cursor := Client.Readers.Find (Name);

      procedure Read_Attributes (Key     : in String;
                                 Element : in out Message_Handler) is
      begin
         Element.Mapping := new MAT.Events.Attribute_Table (1 .. Natural (Count));
         for I in 1 .. Natural (Count) loop
            declare
               Name : constant String := MAT.Readers.Marshaller.Get_String (Msg.Buffer);
               Size : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
            begin
               for J in Element.Attributes'Range loop
                  if Element.Attributes (J).Name.all = Name then
                     Element.Mapping (I) := Element.Attributes (J);
                  end if;
               end loop;
            end;
         end loop;
      end Read_Attributes;

   begin
      Log.Debug ("Read event definition {0}", Name);
      if Reader_Maps.Has_Element (Pos) then
         Client.Readers.Update_Element (Pos, Read_Attributes'Access);
      end if;
   end Read_Definition;

   procedure Read_Headers (Client : in out Manager_Base;
                           Msg    : in out Message) is
      Count : MAT.Types.Uint16;
   begin
      Client.Version := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Client.Flags   := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Count := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);

      Log.Info ("Read event stream version {0} with {1} definitions",
                MAT.Types.Uint16'Image (Client.Version),
                MAT.Types.Uint16'Image (Count));
      for I in 1 .. Count loop
         Read_Definition (Client, Msg);
      end loop;
   end Read_Headers;

end MAT.Readers;
