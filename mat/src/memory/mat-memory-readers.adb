-----------------------------------------------------------------------
--  Memory Events - Definition and Analysis of memory events
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

with MAT.Types;
with MAT.Readers.Marshaller;
with MAT.Memory;
package body MAT.Memory.Readers is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Memory.Readers");

   MSG_MALLOC  : constant MAT.Events.Internal_Reference := 0;
   MSG_FREE    : constant MAT.Events.Internal_Reference := 1;
   MSG_REALLOC : constant MAT.Events.Internal_Reference := 2;

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
            Kind => MAT.Events.T_POINTER, Ref => M_ADDR),
      5 => (Name => THREAD_NAME'Access, Size => 0,
            Kind => MAT.Events.T_THREAD, Ref => M_THREAD),
      6 => (Name => TIME_NAME'Access, Size => 0,
            Kind => MAT.Events.T_TIME, Ref => M_TIME));

   procedure Unmarshall_Allocation (Msg      : in out MAT.Readers.Message;
                                    Slot     : in out Allocation;
                                    Addr     : in out MAT.Types.Target_Addr;
                                    Old_Addr : in out MAT.Types.Target_Addr;
                                    Defs     : in MAT.Events.Attribute_Table);

   ----------------------
   --  Register the reader to extract and analyze memory events.
   ----------------------
   procedure Register (Into   : in out MAT.Readers.Manager_Base'Class;
                       Reader : in Memory_Reader_Access) is
   begin
      Into.Register_Reader (Reader.all'Access, "malloc", MSG_MALLOC,
                            Memory_Attributes'Access);
      Into.Register_Reader (Reader.all'Access, "free", MSG_FREE,
                            Memory_Attributes'Access);
      Into.Register_Reader (Reader.all'Access, "realloc", MSG_REALLOC,
                            Memory_Attributes'Access);
   end Register;

   ----------------------
   --  Unmarshall from the message the memory slot information.
   --  The data is described by the Defs table.
   ----------------------
   procedure Unmarshall_Allocation (Msg      : in out MAT.Readers.Message;
                                    Slot     : in out Allocation;
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
                  Slot.Size := MAT.Readers.Marshaller.Get_Target_Size (Msg.Buffer, Def.Kind);

               when M_ADDR =>
                  Addr := MAT.Readers.Marshaller.Get_Target_Addr (Msg.Buffer, Def.Kind);

               when M_OLD_ADDR =>
                  Old_Addr := MAT.Readers.Marshaller.Get_Target_Addr (Msg.Buffer, Def.Kind);

               when M_UNKNOWN =>
                  MAT.Readers.Marshaller.Skip (Msg.Buffer, Def.Size);

               when others =>
                  MAT.Readers.Marshaller.Skip (Msg.Buffer, Def.Size);
            end case;
         end;
      end loop;
   end Unmarshall_Allocation;

   overriding
   procedure Dispatch (For_Servant : in out Memory_Servant;
                       Id          : in MAT.Events.Internal_Reference;
                       Params      : in MAT.Events.Const_Attribute_Table_Access;
                       Frame       : in MAT.Events.Frame_Info;
                       Msg         : in out MAT.Readers.Message) is
      Slot     : Allocation;
      Addr     : MAT.Types.Target_Addr := 0;
      Old_Addr : MAT.Types.Target_Addr := 0;
   begin
      case Id is
         when MSG_MALLOC =>
            Unmarshall_Allocation (Msg, Slot, Addr, Old_Addr, Params.all);
            Frames.Insert (Frame  => For_Servant.Data.Frames,
                           Pc     => Frame.Frame (1 .. Frame.Cur_Depth),
                           Result => Slot.Frame);
            For_Servant.Data.Probe_Malloc (Addr, Slot);

         when MSG_FREE =>
            Unmarshall_Allocation (Msg, Slot, Addr, Old_Addr, Params.all);
            For_Servant.Data.Probe_Free (Addr, Slot);

         when MSG_REALLOC =>
            Unmarshall_Allocation (Msg, Slot, Addr, Old_Addr, Params.all);
            Frames.Insert (Frame  => For_Servant.Data.Frames,
                           Pc     => Frame.Frame (1 .. Frame.Cur_Depth),
                           Result => Slot.Frame);
            For_Servant.Data.Probe_Realloc (Addr, Old_Addr, Slot);

         when others =>
            raise Program_Error;

      end case;
   end Dispatch;

end MAT.Memory.Readers;
