-----------------------------------------------------------------------
--  mat-targets-probes - Definition and Analysis of process start events
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

with MAT.Readers.Marshaller;
package body MAT.Targets.Probes is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Targets.Probes");

   MSG_BEGIN     : constant MAT.Events.Targets.Probe_Index_Type := 0;
   MSG_END       : constant MAT.Events.Targets.Probe_Index_Type := 1;

   M_PID         : constant MAT.Events.Internal_Reference := 1;
   M_EXE         : constant MAT.Events.Internal_Reference := 2;
   M_HEAP_START  : constant MAT.Events.Internal_Reference := 3;
   M_HEAP_END    : constant MAT.Events.Internal_Reference := 4;
   M_END         : constant MAT.Events.Internal_Reference := 5;

   PID_NAME      : aliased constant String := "pid";
   EXE_NAME      : aliased constant String := "exe";
   HP_START_NAME : aliased constant String := "hp_start";
   HP_END_NAME   : aliased constant String := "hp_end";
   END_NAME      : aliased constant String := "end";

   Process_Attributes : aliased constant MAT.Events.Attribute_Table :=
     (1 => (Name => PID_NAME'Access, Size => 0,
            Kind => MAT.Events.T_SIZE_T, Ref => M_PID),
      2 => (Name => EXE_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_EXE),
      3 => (Name => HP_START_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_HEAP_START),
      4 => (Name => HP_END_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_HEAP_END),
      5 => (Name => END_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_END));

   --  ------------------------------
   --  Create a new process after the begin event is received from the event stream.
   --  ------------------------------
   procedure Create_Process (Probe : in Process_Probe_Type;
                             Pid         : in MAT.Types.Target_Process_Ref;
                             Path        : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Probe.Target.Create_Process (Pid     => Pid,
                                   Path    => Path,
                                   Process => Probe.Target.Current);
      Probe.Target.Process.Events := Probe.Events;
      MAT.Memory.Targets.Initialize (Memory  => Probe.Target.Process.Memory,
                                     Manager => Probe.Manager.all);
   end Create_Process;

   procedure Probe_Begin (Probe : in Process_Probe_Type;
                          Id          : in MAT.Events.Targets.Probe_Index_Type;
                          Defs        : in MAT.Events.Attribute_Table;
                          Msg         : in out MAT.Readers.Message) is
      use type MAT.Types.Target_Addr;

      Pid  : MAT.Types.Target_Process_Ref := 0;
      Path : Ada.Strings.Unbounded.Unbounded_String;
      Heap : MAT.Memory.Region_Info;
   begin
      for I in Defs'Range loop
         declare
            Def : MAT.Events.Attribute renames Defs (I);
         begin
            case Def.Ref is
               when M_PID =>
                  Pid := MAT.Readers.Marshaller.Get_Target_Process_Ref (Msg, Def.Kind);

               when M_EXE =>
                  Path := MAT.Readers.Marshaller.Get_String (Msg);

               when M_HEAP_START =>
                  Heap.Start_Addr := MAT.Readers.Marshaller.Get_Target_Addr (Msg, Def.Kind);

               when M_HEAP_END =>
                  Heap.End_Addr := MAT.Readers.Marshaller.Get_Target_Addr (Msg, Def.Kind);

               when others =>
                  MAT.Readers.Marshaller.Skip (Msg, Def.Size);
            end case;
         end;
      end loop;
      Heap.Size       := Heap.End_Addr - Heap.Start_Addr;
      Heap.Path       := Ada.Strings.Unbounded.To_Unbounded_String ("[heap]");
      Probe.Create_Process (Pid, Path);
      Probe.Manager.Read_Message (Msg);
      Probe.Manager.Read_Event_Definitions (Msg);
      Probe.Target.Process.Memory.Add_Region (Heap);
   end Probe_Begin;

   overriding
   procedure Extract (Probe  : in Process_Probe_Type;
                      Params : in MAT.Events.Const_Attribute_Table_Access;
                      Msg    : in out MAT.Readers.Message_Type;
                      Event  : in out MAT.Events.Targets.Probe_Event_Type) is
      use type MAT.Events.Targets.Probe_Index_Type;
   begin
      if Event.Index = MSG_BEGIN then
         Probe.Probe_Begin (Event.Index, Params.all, Msg);
      end if;
   end Extract;

   procedure Execute (Probe : in Process_Probe_Type;
                      Event : in MAT.Events.Targets.Probe_Event_Type) is
   begin
      null;
   end Execute;

   --  ------------------------------
   --  Register the reader to extract and analyze process events.
   --  ------------------------------
   procedure Register (Into  : in out MAT.Events.Probes.Probe_Manager_Type'Class;
                       Probe : in Process_Probe_Type_Access) is
   begin
      Probe.Manager := Into'Unchecked_Access;
      Into.Register_Probe (Probe.all'Access, "begin", MSG_BEGIN,
                           Process_Attributes'Access);
      Into.Register_Probe (Probe.all'Access, "end", MSG_END,
                           Process_Attributes'Access);
   end Register;

   --  ------------------------------
   --  Initialize the target object to prepare for reading process events.
   --  ------------------------------
   procedure Initialize (Target  : in out Target_Type;
                         Manager : in out MAT.Events.Probes.Probe_Manager_Type'Class) is
      Process_Probe : constant Process_Probe_Type_Access
        := new Process_Probe_Type;
   begin
      Process_Probe.Target := Target'Unrestricted_Access;
      Process_Probe.Events := Manager.Get_Target_Events;
      Register (Manager, Process_Probe);
   end Initialize;

end MAT.Targets.Probes;
