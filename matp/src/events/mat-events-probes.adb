-----------------------------------------------------------------------
--  mat-readers -- Reader
--  Copyright (C) 2014, 2015, 2019 Stephane Carrez
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

with MAT.Readers.Marshaller;
package body MAT.Events.Probes is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Events.Probes");

   procedure Read_Probe (Client : in out Probe_Manager_Type;
                         Msg    : in out MAT.Readers.Message_Type);

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
   --  Update the Size and Prev_Id information in the event identified by <tt>Id</tt>.
   --  Update the event represented by <tt>Prev_Id</tt> so that its Next_Id refers
   --  to the <tt>Id</tt> event.
   --  ------------------------------
   procedure Update_Event (Probe   : in Probe_Type;
                           Id      : in MAT.Events.Event_Id_Type;
                           Size    : in MAT.Types.Target_Size;
                           Prev_Id : in MAT.Events.Event_Id_Type) is
   begin
      Probe.Owner.Get_Target_Events.Update_Event (Id, Size, Prev_Id);
   end Update_Event;

   --  ------------------------------
   --  Initialize the manager instance.
   --  ------------------------------
   overriding
   procedure Initialize (Manager : in out Probe_Manager_Type) is
   begin
      Manager.Events := new MAT.Events.Targets.Target_Events;
      Manager.Frames := new MAT.Frames.Targets.Target_Frames;
   end Initialize;

   --  ------------------------------
   --  Register the probe to handle the event identified by the given name.
   --  The event is mapped to the given id and the attributes table is used
   --  to setup the mapping from the data stream attributes.
   --  ------------------------------
   procedure Register_Probe (Into   : in out Probe_Manager_Type;
                             Probe  : in Probe_Type_Access;
                             Name   : in String;
                             Id     : in MAT.Events.Probe_Index_Type;
                             Model  : in MAT.Events.Const_Attribute_Table_Access) is
      Handler : Probe_Handler;
   begin
      Handler.Probe := Probe;
      Handler.Id    := Id;
      Handler.Attributes  := Model;
      Handler.Mapping     := null;
      Into.Probes.Insert (Name, Handler);
      Probe.Owner := Into'Unchecked_Access;
   end Register_Probe;

   --  ------------------------------
   --  Get the target events.
   --  ------------------------------
   function Get_Target_Events (Client : in Probe_Manager_Type)
                               return MAT.Events.Targets.Target_Events_Access is
   begin
      return Client.Events;
   end Get_Target_Events;

   --  ------------------------------
   --  Get the target frames.
   --  ------------------------------
   function Get_Target_Frames (Client : in Probe_Manager_Type)
                               return MAT.Frames.Targets.Target_Frames_Access is
   begin
      return Client.Frames;
   end Get_Target_Frames;

   procedure Read_Probe (Client : in out Probe_Manager_Type;
                         Msg    : in out MAT.Readers.Message) is
      use type MAT.Types.Target_Tick_Ref;

      Count     : Natural := 0;
      Time_Sec  : MAT.Types.Uint32  := 0;
      Time_Usec : MAT.Types.Uint32 := 0;
      Frame     : constant access MAT.Events.Frame_Info := Client.Frame;
   begin
      Client.Event.Thread := 0;
      Frame.Stack  := 0;
      Frame.Cur_Depth := 0;
      for I in Client.Probe'Range loop
         declare
            Def : MAT.Events.Attribute renames Client.Probe (I);
         begin
            case Def.Ref is
               when P_TIME_SEC =>
                  Time_Sec := MAT.Readers.Marshaller.Get_Target_Uint32 (Msg, Def.Kind);

               when P_TIME_USEC =>
                  Time_Usec := MAT.Readers.Marshaller.Get_Target_Uint32 (Msg, Def.Kind);

               when P_THREAD_ID =>
                  Client.Event.Thread := MAT.Types.Target_Thread_Ref
                    (MAT.Readers.Marshaller.Get_Target_Uint32 (Msg, Def.Kind));

               when P_THREAD_SP =>
                  Frame.Stack := MAT.Readers.Marshaller.Get_Target_Addr (Msg, Def.Kind);

               when P_FRAME =>
                  Count := Natural (MAT.Readers.Marshaller.Get_Target_Uint32 (Msg,
                                    Def.Kind));

               when P_FRAME_PC =>
                  for I in 1 .. Count loop --  reverse Count .. 1 loop
                     if Count < Frame.Depth then
                        Frame.Frame (Count - I + 1) := MAT.Readers.Marshaller.Get_Target_Addr (Msg,
                                                                                   Def.Kind);
                     end if;
                  end loop;

               when others =>
                  null;

            end case;
         end;
      end loop;
      --  Convert the time in usec to make computation easier.
      Client.Event.Time := MAT.Types.Target_Tick_Ref (Time_Sec) * 1_000_000;
      Client.Event.Time := Client.Event.Time + MAT.Types.Target_Tick_Ref (Time_Usec);
      Frame.Cur_Depth := Count;

   exception
      when others =>
         Log.Error ("Marshaling error, frame count {0}", Natural'Image (Count));
         raise;

   end Read_Probe;

   procedure Dispatch_Message (Client : in out Probe_Manager_Type;
                               Msg    : in out MAT.Readers.Message_Type) is
      use type MAT.Events.Targets.Target_Events_Access;

      Event : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Pos   : constant Handler_Maps.Cursor := Client.Handlers.Find (Event);
   begin
      if Log.Get_Level = Util.Log.DEBUG_LEVEL then
         Log.Debug ("Dispatch message {0} - size {1}",
                    MAT.Types.Uint16'Image (Event),
                    Natural'Image (Msg.Size));
      end if;

      if not Handler_Maps.Has_Element (Pos) then
         --  Message is not handled, skip it.
         null;
      else
         if Client.Probe /= null then
            Read_Probe (Client, Msg);
         end if;
         declare
            Handler : constant Probe_Handler := Handler_Maps.Element (Pos);
         begin
            Client.Event.Prev_Id := 0;
            Client.Event.Old_Size := 0;
            Client.Event.Event := Event;
            Client.Event.Index := Handler.Id;
            Handler.Probe.Extract (Handler.Mapping.all'Access, Msg, Client.Event);
            Client.Frames.Insert (Pc     => Client.Frame.Frame (1 .. Client.Frame.Cur_Depth),
                                  Result => Client.Event.Frame);
            Client.Events.Insert (Client.Event);
            Handler.Probe.Execute (Client.Event);
         end;
      end if;

   exception
      when E : others =>
         Log.Error ("Exception while processing event " & MAT.Types.Uint16'Image (Event), E, True);
   end Dispatch_Message;

   --  ------------------------------
   --  Read an event definition from the stream and configure the reader.
   --  ------------------------------
   procedure Read_Definition (Client : in out Probe_Manager_Type;
                              Msg    : in out MAT.Readers.Message_Type) is
      Name  : constant String := MAT.Readers.Marshaller.Get_String (Msg);
      Event : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Count : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Pos   : constant Probe_Maps.Cursor := Client.Probes.Find (Name);
      Frame : Probe_Handler;

      procedure Add_Handler (Key     : in String;
                             Element : in out Probe_Handler);

      procedure Add_Handler (Key     : in String;
                             Element : in out Probe_Handler) is
         pragma Unreferenced (Key);
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
            Name : constant String := MAT.Readers.Marshaller.Get_String (Msg);
            Size : constant MAT.Types.Uint16 := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);

            procedure Read_Attribute (Key     : in String;
                                      Element : in out Probe_Handler);

            procedure Read_Attribute (Key     : in String;
                                      Element : in out Probe_Handler) is
               pragma Unreferenced (Key);
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
            if Probe_Maps.Has_Element (Pos) then
               Client.Probes.Update_Element (Pos, Read_Attribute'Access);
            end if;
            if Frame.Mapping /= null then
               Read_Attribute ("start", Frame);
            end if;
         end;
      end loop;
      if Probe_Maps.Has_Element (Pos) then
         Client.Probes.Update_Element (Pos, Add_Handler'Access);
      end if;
   end Read_Definition;

   --  ------------------------------
   --  Read a list of event definitions from the stream and configure the reader.
   --  ------------------------------
   procedure Read_Event_Definitions (Client : in out Probe_Manager_Type;
                                     Msg    : in out MAT.Readers.Message) is
      Count : MAT.Types.Uint16;
   begin
      if Client.Frame = null then
         Client.Frame := new MAT.Events.Frame_Info (512);
      end if;
      Client.Version := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);
      Count := MAT.Readers.Marshaller.Get_Uint16 (Msg.Buffer);

      Log.Info ("Read event stream version {0} with {1} definitions",
                MAT.Types.Uint16'Image (Client.Version),
                MAT.Types.Uint16'Image (Count));
      for I in 1 .. Count loop
         Read_Definition (Client, Msg);
      end loop;

      --  Look at the probe definition to gather the target address size.
      Client.Addr_Size := MAT.Events.T_UINT32;
      for I in Client.Probe'Range loop
         declare
            Def : MAT.Events.Attribute renames Client.Probe (I);
         begin
            if Def.Ref = P_THREAD_SP or Def.Ref = P_FRAME_PC then
               Client.Addr_Size := Def.Kind;
               exit;
            end if;
         end;
      end loop;

   exception
      when E : MAT.Readers.Marshaller.Buffer_Underflow_Error =>
         Log.Error ("Not enough data in the message", E, True);

   end Read_Event_Definitions;

   --  ------------------------------
   --  Get the size of a target address (4 or 8 bytes).
   --  ------------------------------
   function Get_Address_Size (Client : in Probe_Manager_Type) return MAT.Events.Attribute_Type is
   begin
      return Client.Addr_Size;
   end Get_Address_Size;

end MAT.Events.Probes;
