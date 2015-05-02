-----------------------------------------------------------------------
--  mat-memory-probes - Definition and Analysis of memory events
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
with Util.Log.Loggers;

with MAT.Types;
with MAT.Readers.Marshaller;
with MAT.Memory;
package body MAT.Memory.Probes is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Memory.Probes");

   M_SIZE     : constant MAT.Events.Internal_Reference := 1;
   M_FRAME    : constant MAT.Events.Internal_Reference := 2;
   M_ADDR     : constant MAT.Events.Internal_Reference := 3;
   M_OLD_ADDR : constant MAT.Events.Internal_Reference := 4;
   M_THREAD   : constant MAT.Events.Internal_Reference := 5;
   M_UNKNOWN  : constant MAT.Events.Internal_Reference := 6;
   M_TIME     : constant MAT.Events.Internal_Reference := 7;
   --  Defines the possible data kinds which are recognized by
   --  the memory unmarshaller.  All others are ignored.

   SIZE_NAME  : aliased constant String := "size";
   FRAME_NAME : aliased constant String := "frame";
   ADDR_NAME  : aliased constant String := "pointer";
   OLD_NAME   : aliased constant String := "old-pointer";
   THREAD_NAME : aliased constant String := "thread";
   TIME_NAME   : aliased constant String := "time";

   Memory_Attributes : aliased constant MAT.Events.Attribute_Table :=
     (1 => (Name => SIZE_NAME'Access, Size => 0,
            Kind => MAT.Events.T_SIZE_T, Ref => M_SIZE),
      2 => (Name => FRAME_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_FRAME),
      3 => (Name => ADDR_NAME'Access, Size => 0,
            Kind => MAT.Events.T_POINTER, Ref => M_ADDR),
      4 => (Name => OLD_NAME'Access, Size => 0,
            Kind => MAT.Events.T_POINTER, Ref => M_OLD_ADDR),
      5 => (Name => THREAD_NAME'Access, Size => 0,
            Kind => MAT.Events.T_THREAD, Ref => M_THREAD),
      6 => (Name => TIME_NAME'Access, Size => 0,
            Kind => MAT.Events.T_TIME, Ref => M_TIME));

   procedure Unmarshall_Allocation (Msg      : in out MAT.Readers.Message;
                                    Size     : in out MAT.Types.Target_Size;
                                    Addr     : in out MAT.Types.Target_Addr;
                                    Old_Addr : in out MAT.Types.Target_Addr;
                                    Defs     : in MAT.Events.Attribute_Table);

   ----------------------
   --  Register the reader to extract and analyze memory events.
   ----------------------
   procedure Register (Into  : in out MAT.Events.Probes.Probe_Manager_Type'Class;
                       Probe : in Memory_Probe_Type_Access) is
   begin
      Into.Register_Probe (Probe.all'Access, "malloc", MAT.Events.Targets.MSG_MALLOC,
                           Memory_Attributes'Access);
      Into.Register_Probe (Probe.all'Access, "free", MAT.Events.Targets.MSG_FREE,
                           Memory_Attributes'Access);
      Into.Register_Probe (Probe.all'Access, "realloc", MAT.Events.Targets.MSG_REALLOC,
                           Memory_Attributes'Access);
   end Register;

   ----------------------
   --  Unmarshall from the message the memory slot information.
   --  The data is described by the Defs table.
   ----------------------
   procedure Unmarshall_Allocation (Msg      : in out MAT.Readers.Message_Type;
                                    Size     : in out MAT.Types.Target_Size;
                                    Addr     : in out MAT.Types.Target_Addr;
                                    Old_Addr : in out MAT.Types.Target_Addr;
                                    Defs     : in MAT.Events.Attribute_Table) is
   begin
      for I in Defs'Range loop
         declare
            Def : MAT.Events.Attribute renames Defs (I);
         begin
            case Def.Ref is
               when M_SIZE =>
                  Size := MAT.Readers.Marshaller.Get_Target_Size (Msg, Def.Kind);

               when M_ADDR =>
                  Addr := MAT.Readers.Marshaller.Get_Target_Addr (Msg, Def.Kind);

               when M_OLD_ADDR =>
                  Old_Addr := MAT.Readers.Marshaller.Get_Target_Addr (Msg, Def.Kind);

               when M_UNKNOWN =>
                  MAT.Readers.Marshaller.Skip (Msg, Def.Size);

               when others =>
                  MAT.Readers.Marshaller.Skip (Msg, Def.Size);

            end case;
         end;
      end loop;
   end Unmarshall_Allocation;

   ----------------------
   --  Extract the probe information from the message.
   ----------------------
   overriding
   procedure Extract (Probe   : in Memory_Probe_Type;
                      Params  : in MAT.Events.Const_Attribute_Table_Access;
                      Msg     : in out MAT.Readers.Message_Type;
                      Event   : in out MAT.Events.Targets.Probe_Event_Type) is
   begin
      case Event.Index is
         when MAT.Events.Targets.MSG_MALLOC =>
            Unmarshall_Allocation (Msg, Event.Size, Event.Addr, Event.Old_Addr, Params.all);

         when MAT.Events.Targets.MSG_FREE =>
            Unmarshall_Allocation (Msg, Event.Size, Event.Addr, Event.Old_Addr, Params.all);

         when MAT.Events.Targets.MSG_REALLOC =>
            Unmarshall_Allocation (Msg, Event.Size, Event.Addr, Event.Old_Addr, Params.all);

         when others =>
            Log.Error ("Invalid event {0} for memory extract probe",
                       MAT.Events.Targets.Probe_Index_Type'Image (Event.Index));
            raise Program_Error;

      end case;
   end Extract;

   procedure Execute (Probe : in Memory_Probe_Type;
                      Event : in out MAT.Events.Targets.Probe_Event_Type) is
      Slot     : Allocation;
   begin
      Slot.Size   := Event.Size;
      Slot.Thread := Event.Thread;
      Slot.Time   := Event.Time;
      Slot.Frame  := Event.Frame;
      case Event.Index is
         when MAT.Events.Targets.MSG_MALLOC =>
            Probe.Data.Probe_Malloc (Event.Addr, Slot);

         when MAT.Events.Targets.MSG_FREE =>
            Probe.Data.Probe_Free (Event.Addr, Slot);
            Event.Size := Slot.Size;

         when MAT.Events.Targets.MSG_REALLOC =>
            Probe.Data.Probe_Realloc (Event.Addr, Event.Old_Addr, Slot);

         when others =>
            Log.Error ("Invalid event {0} for memory execute probe",
                       MAT.Events.Targets.Probe_Index_Type'Image (Event.Index));
            raise Program_Error;

      end case;
   end Execute;

end MAT.Memory.Probes;
