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

   P_TIME_SEC         : constant MAT.Events.Internal_Reference := 0;
   P_TIME_USEC        : constant MAT.Events.Internal_Reference := 1;
   P_THREAD_ID        : constant MAT.Events.Internal_Reference := 2;
   P_THREAD_SP        : constant MAT.Events.Internal_Reference := 3;
   P_RUSAGE_MINFLT    : constant MAT.Events.Internal_Reference := 4;
   P_RUSAGE_MAJFLT    : constant MAT.Events.Internal_Reference := 5;
   P_RUSAGE_NVCSW     : constant MAT.Events.Internal_Reference := 6;
   P_RUSAGE_NIVCSW    : constant MAT.Events.Internal_Reference := 7;
   P_FRAME            : constant MAT.Events.Internal_Reference := 8;
   P_FRAME_PC         : constant MAT.Events.Internal_Reference := 9;

   TIME_SEC_NAME      : aliased constant String := "time-sec";
   TIME_USEC_NAME     : aliased constant String := "time-usec";
   THREAD_ID_NAME     : aliased constant String := "thread-id";
   THREAD_SP_NAME     : aliased constant String := "thread-sp";
   RUSAGE_MINFLT_NAME : aliased constant String := "ru-minflt";
   RUSAGE_MAJFLT_NAME : aliased constant String := "ru-majflt";
   RUSAGE_NVCSW_NAME  : aliased constant String := "ru-nvcsw";
   RUSAGE_NIVCSW_NAME : aliased constant String := "ru-nivcsw";
   FRAME_NAME         : aliased constant String := "frame";
   FRAME_PC_NAME      : aliased constant String := "frame-pc";

   Probe_Attributes : aliased constant MAT.Events.Attribute_Table :=
     (1 => (Name => TIME_SEC_NAME'Access,
            Size => 0,
            Kind => MAT.Events.T_SIZE_T,
            Ref  => P_TIME_SEC),
      2 => (Name => TIME_USEC_NAME'Access,
             Size => 0,
             Kind => MAT.Events.T_SIZE_T,
             Ref  => P_TIME_USEC),
      3 => (Name => THREAD_ID_NAME'Access,
            Size => 0,
            Kind => MAT.Events.T_SIZE_T,
            Ref  => P_THREAD_ID),
      4 => (Name => THREAD_SP_NAME'Access,
            Size => 0,
            Kind => MAT.Events.T_SIZE_T,
            Ref  => P_THREAD_SP),
      5 => (Name => FRAME_NAME'Access,
            Size => 0,
            Kind => MAT.Events.T_SIZE_T,
            Ref  => P_FRAME),
      6 => (Name => FRAME_PC_NAME'Access,
            Size => 0,
            Kind => MAT.Events.T_SIZE_T,
            Ref  => P_FRAME_PC),
      7 => (Name => RUSAGE_MINFLT_NAME'Access,
            Size => 0,
            Kind => MAT.Events.T_SIZE_T,
            Ref  => P_RUSAGE_MINFLT),
      8 => (Name => RUSAGE_MAJFLT_NAME'Access,
            Size => 0,
            Kind => MAT.Events.T_SIZE_T,
            Ref  => P_RUSAGE_MAJFLT),
      9 => (Name => RUSAGE_NVCSW_NAME'Access,
            Size => 0,
            Kind => MAT.Events.T_SIZE_T,
            Ref  => P_RUSAGE_NVCSW),
      10 => (Name => RUSAGE_NIVCSW_NAME'Access,
            Size => 0,
            Kind => MAT.Events.T_SIZE_T,
            Ref  => P_RUSAGE_NIVCSW)
     );

   function Hash (Key : in MAT.Types.Uint16) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Hash;

   --  ------------------------------
   --  Initialize the manager instance.
   --  ------------------------------
   overriding
   procedure Initialize (Manager : in out Manager_Base) is
   begin
      Manager.Events := new MAT.Events.Targets.Target_Events;
   end Initialize;

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

   --  ------------------------------
   --  Get the target events.
   --  ------------------------------
   function Get_Target_Events (Client : in Manager_Base)
                               return MAT.Events.Targets.Target_Events_Access is
   begin
      return Client.Events;
   end Get_Target_Events;

   procedure Read_Probe (Client : in out Manager_Base;
                         Msg    : in out Message) is
      use type Interfaces.Unsigned_64;

      Count     : Natural := 0;
      Time_Sec  : MAT.Types.Uint32  := 0;
      Time_Usec : MAT.Types.Uint32 := 0;
      Frame     : access MAT.Events.Frame_Info := Client.Frame;
   begin
      Frame.Thread := 0;
      Frame.Stack  := 0;
      Frame.Cur_Depth := 0;
      for I in Client.Probe'Range loop
         declare
            Def : MAT.Events.Attribute renames Client.Probe (I);
         begin
            case Def.Ref is
               when P_TIME_SEC =>
                  Time_Sec := MAT.Readers.Marshaller.Get_Target_Uint32 (Msg.Buffer, Def.Kind);

               when P_TIME_USEC =>
                  Time_Usec := MAT.Readers.Marshaller.Get_Target_Uint32 (Msg.Buffer, Def.Kind);

               when P_THREAD_ID =>
                  Frame.Thread := MAT.Readers.Marshaller.Get_Target_Uint32 (Msg.Buffer, Def.Kind);

               when P_THREAD_SP =>
                  Frame.Stack := MAT.Readers.Marshaller.Get_Target_Addr (Msg.Buffer, Def.Kind);

               when P_FRAME =>
                  Count := Natural (MAT.Readers.Marshaller.Get_Target_Uint32 (Msg.Buffer,
                                    Def.Kind));

               when P_FRAME_PC =>
                  for I in 1 .. Natural (Count) loop
                     if Count < Frame.Depth then
                        Frame.Frame (I) := MAT.Readers.Marshaller.Get_Target_Addr (Msg.Buffer,
                                                                                   Def.Kind);
                     end if;
                  end loop;

               when others =>
                  null;

            end case;
         end;
      end loop;
      Frame.Time := Interfaces.Shift_Left (Interfaces.Unsigned_64 (Time_Sec), 32);
      Frame.Time := Frame.Time or Interfaces.Unsigned_64 (Time_Usec);
      Frame.Cur_Depth := Count;
   end Read_Probe;

   procedure Dispatch_Message (Client : in out Manager_Base;
                               Msg    : in out Message) is
      use type MAT.Events.Attribute_Table_Ptr;
      use type MAT.Events.Targets.Target_Events_Access;

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
         if Client.Probe /= null then
            Read_Probe (Client, Msg);
         end if;
         declare
            Handler : constant Message_Handler := Handler_Maps.Element (Pos);
         begin
            Client.Events.Insert (Event, Client.Frame.all);
            Dispatch (Handler.For_Servant.all, Handler.Id, Handler.Mapping.all'Access,
                      Client.Frame.all, Msg);
         end;
      end if;

   exception
      when E : others =>
         Log.Error ("Exception while processing event " & MAT.Types.Uint16'Image (Event), E, True);
   end Dispatch_Message;

   --  ------------------------------
   --  Read an event definition from the stream and configure the reader.
   --  ------------------------------
   procedure Read_Definition (Client : in out Manager_Base;
                              Msg    : in out Message) is
      Name  : constant String := MAT.Readers.Marshaller.Get_String (Msg.Buffer);
      Event : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Count : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Pos   : constant Reader_Maps.Cursor := Client.Readers.Find (Name);
      Frame : Message_Handler;

      procedure Add_Handler (Key : in String;
                             Element : in out Message_Handler) is
      begin
         Client.Handlers.Insert (Event, Element);
      end Add_Handler;

   begin
      Log.Debug ("Read event definition {0} with {1} attributes",
                 Name, MAT.Types.Uint16'Image (Count));

      if Name = "start" then
         Frame.Mapping := new MAT.Events.Attribute_Table (1 .. Natural (Count));
         Frame.Attributes := Probe_Attributes'Access;
         Client.Probe := Frame.Mapping;
      else
         Frame.Mapping := null;
      end if;
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

            use type MAT.Events.Attribute_Table_Ptr;
         begin
            if Reader_Maps.Has_Element (Pos) then
               Client.Readers.Update_Element (Pos, Read_Attribute'Access);
            end if;
            if Frame.Mapping /= null then
               Read_Attribute ("start", Frame);
            end if;
         end;
      end loop;
      if Reader_Maps.Has_Element (Pos) then
         Client.Readers.Update_Element (Pos, Add_Handler'Access);
      end if;
   end Read_Definition;

   --  ------------------------------
   --  Read a list of event definitions from the stream and configure the reader.
   --  ------------------------------
   procedure Read_Event_Definitions (Client : in out Manager_Base;
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

   exception
      when E : MAT.Readers.Marshaller.Buffer_Underflow_Error =>
         Log.Error ("Not enough data in the message", E, True);

   end Read_Event_Definitions;

   --  ------------------------------
   --  Read the event data stream headers with the event description.
   --  Configure the reader to analyze the data stream according to the event descriptions.
   --  ------------------------------
   procedure Read_Headers (Client : in out Manager_Base;
                           Msg    : in out Message) is
   begin
      Client.Read_Event_Definitions (Msg);

      Client.Frame := new MAT.Events.Frame_Info (512);
   exception
         when E : others =>
         Log.Error ("Exception while reading headers ", E);
   end Read_Headers;

end MAT.Readers;
