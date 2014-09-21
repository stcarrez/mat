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

   --  ------------------------------
   --  Register the reader to handle the event identified by the given name.
   --  The event is mapped to the given id and the attributes table is used
   --  to setup the mapping from the data stream attributes.
   --  ------------------------------
   procedure Register_Reader (Into   : in out Manager_Base;
                              Reader : in Reader_Access;
                              Name   : in String;
                              Id     : in MAT.Events.Internal_Reference;
                              Model  : in MAT.Events.Const_Attribute_Table_Access) is
      Handler : Message_Handler;
   begin
      Handler.For_Servant := Reader;
      Handler.Id := Id;
      Handler.Attributes  := Model;
      Handler.Mapping     := null;
      Into.Readers.Insert (Name, Handler);
   end Register_Reader;

   procedure Dispatch_Message (Client : in out Manager_Base;
                               Msg    : in out Message) is
      Event : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Pos   : constant Handler_Maps.Cursor := Client.Handlers.Find (Event);
   begin
      Log.Debug ("Dispatch message {0} - size {1}",
                 MAT.Types.Uint16'Image (Event),
                 Natural'Image (Msg.Size));

      if not Handler_Maps.Has_Element (Pos) then
         --  Message is not handled, skip it.
         null;
      else
         declare
            Handler : constant Message_Handler := Handler_Maps.Element (Pos);
         begin
            Dispatch (Handler.For_Servant.all, Handler.Id, Handler.Mapping.all'Access, Msg);
         end;
      end if;

   exception
      when E : others =>
         Log.Error ("Exception while processing event " & MAT.Types.Uint16'Image (Event), E);
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

      procedure Add_Handler (Key : in String;
                             Element : in out Message_Handler) is
      begin
         Client.Handlers.Insert (Event, Element);
      end Add_Handler;

   begin
      Log.Debug ("Read event definition {0}", Name);
      for I in 1 .. Natural (Count) loop
         declare
            Name : constant String := MAT.Readers.Marshaller.Get_String (Msg.Buffer);
            Size : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);

            procedure Read_Attribute (Key     : in String;
                                      Element : in out Message_Handler) is
               use type MAT.Events.Attribute_Table_Ptr;
            begin
               if Element.Mapping = null then
                  Element.Mapping := new MAT.Events.Attribute_Table (1 .. Natural (Count));
               end if;
               for J in Element.Attributes'Range loop
                  if Element.Attributes (J).Name.all = Name then
                     Element.Mapping (I) := Element.Attributes (J);
                     if Size = 1 then
                        Element.Mapping (I).Kind := MAT.Events.T_UINT8;
                     elsif Size = 2 then
                        Element.Mapping (I).Kind := MAT.Events.T_UINT16;
                     elsif Size = 4 then
                        Element.Mapping (I).Kind := MAT.Events.T_UINT32;
                     elsif Size = 8 then
                        Element.Mapping (I).Kind := MAT.Events.T_UINT64;
                     else
                        Element.Mapping (I).Kind := MAT.Events.T_UINT32;
                     end if;
                  end if;
               end loop;
            end Read_Attribute;

         begin
            if Reader_Maps.Has_Element (Pos) then
               Client.Readers.Update_Element (Pos, Read_Attribute'Access);
            end if;
         end;
      end loop;
      if Reader_Maps.Has_Element (Pos) then
         Client.Readers.Update_Element (Pos, Add_Handler'Access);
      end if;
   end Read_Definition;

   --  ------------------------------
   --  Read the event data stream headers with the event description.
   --  Configure the reader to analyze the data stream according to the event descriptions.
   --  ------------------------------
   procedure Read_Headers (Client : in out Manager_Base;
                           Msg    : in out Message) is
      Count : MAT.Types.Uint16;
   begin
      Client.Version := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Count := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);

      Log.Info ("Read event stream version {0} with {1} definitions",
                MAT.Types.Uint16'Image (Client.Version),
                MAT.Types.Uint16'Image (Count));
      for I in 1 .. Count loop
         Read_Definition (Client, Msg);
      end loop;
   end Read_Headers;

end MAT.Readers;
