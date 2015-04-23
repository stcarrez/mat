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

with ELF;
with MAT.Readers.Marshaller;
package body MAT.Targets.Probes is

   M_PID         : constant MAT.Events.Internal_Reference := 1;
   M_EXE         : constant MAT.Events.Internal_Reference := 2;
   M_HEAP_START  : constant MAT.Events.Internal_Reference := 3;
   M_HEAP_END    : constant MAT.Events.Internal_Reference := 4;
   M_END         : constant MAT.Events.Internal_Reference := 5;
   M_LIBNAME     : constant MAT.Events.Internal_Reference := 6;
   M_LADDR       : constant MAT.Events.Internal_Reference := 7;
   M_COUNT       : constant MAT.Events.Internal_Reference := 8;
   M_TYPE        : constant MAT.Events.Internal_Reference := 9;
   M_VADDR       : constant MAT.Events.Internal_Reference := 10;
   M_SIZE        : constant MAT.Events.Internal_Reference := 11;
   M_FLAGS       : constant MAT.Events.Internal_Reference := 12;

   PID_NAME      : aliased constant String := "pid";
   EXE_NAME      : aliased constant String := "exe";
   HP_START_NAME : aliased constant String := "hp_start";
   HP_END_NAME   : aliased constant String := "hp_end";
   END_NAME      : aliased constant String := "end";
   LIBNAME_NAME  : aliased constant String := "libname";
   LADDR_NAME    : aliased constant String := "laddr";
   COUNT_NAME    : aliased constant String := "count";
   TYPE_NAME     : aliased constant String := "type";
   VADDR_NAME    : aliased constant String := "vaddr";
   SIZE_NAME     : aliased constant String := "size";
   FLAGS_NAME    : aliased constant String := "flags";

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

   Library_Attributes : aliased constant MAT.Events.Attribute_Table :=
     (1 => (Name => LIBNAME_NAME'Access, Size => 0,
            Kind => MAT.Events.T_SIZE_T, Ref => M_LIBNAME),
      2 => (Name => LADDR_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_LADDR),
      3 => (Name => COUNT_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_COUNT),
      4 => (Name => TYPE_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_TYPE),
      5 => (Name => VADDR_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_VADDR),
      6 => (Name => SIZE_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_SIZE),
      7 => (Name => FLAGS_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_FLAGS));

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
                          Defs  : in MAT.Events.Attribute_Table;
                          Msg   : in out MAT.Readers.Message;
                          Event : in out MAT.Events.Targets.Probe_Event_Type) is
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
      Heap.Size  := Heap.End_Addr - Heap.Start_Addr;
      Heap.Path  := Ada.Strings.Unbounded.To_Unbounded_String ("[heap]");
      Event.Addr := Heap.Start_Addr;
      Event.Size := Heap.Size;
      Probe.Create_Process (Pid, Path);
      Probe.Manager.Read_Message (Msg);
      Probe.Manager.Read_Event_Definitions (Msg);
      Probe.Target.Process.Memory.Add_Region (Heap);
      Probe.Target.Process.Endian := MAT.Readers.Get_Endian (Msg);
   end Probe_Begin;

   --  ------------------------------
   --  Extract the information from the 'library' event.
   --  ------------------------------
   procedure Probe_Library (Probe : in Process_Probe_Type;
                            Defs  : in MAT.Events.Attribute_Table;
                            Msg   : in out MAT.Readers.Message;
                            Event : in out MAT.Events.Targets.Probe_Event_Type) is
      use type MAT.Types.Target_Addr;

      Count : MAT.Types.Target_Size := 0;
      Path  : Ada.Strings.Unbounded.Unbounded_String;
      Addr  : MAT.Types.Target_Addr;
      Pos   : Natural := Defs'Last + 1;
      Offset : MAT.Types.Target_Addr;
   begin
      for I in Defs'Range loop
         declare
            Def : MAT.Events.Attribute renames Defs (I);
         begin
            case Def.Ref is
               when M_COUNT =>
                  Count := MAT.Readers.Marshaller.Get_Target_Size (Msg, Def.Kind);
                  Pos := I + 1;
                  exit;

               when M_LIBNAME =>
                  Path := MAT.Readers.Marshaller.Get_String (Msg);

               when M_LADDR =>
                  Addr := MAT.Readers.Marshaller.Get_Target_Addr (Msg, Def.Kind);

               when others =>
                  MAT.Readers.Marshaller.Skip (Msg, Def.Size);
            end case;
         end;
      end loop;
      Event.Addr := Addr;
      Event.Size := 0;
      for Region in 1 .. Count loop
         declare
            Region : MAT.Memory.Region_Info;
            Kind   : ELF.Elf32_Word := 0;
         begin
            for I in Pos .. Defs'Last loop
               declare
                  Def : MAT.Events.Attribute renames Defs (I);
               begin
                  case Def.Ref is
                  when M_SIZE =>
                     Region.Size := MAT.Readers.Marshaller.Get_Target_Size (Msg, Def.Kind);

                  when M_VADDR =>
                     Region.Start_Addr := MAT.Readers.Marshaller.Get_Target_Addr (Msg, Def.Kind);

                  when M_TYPE =>
                     Kind := MAT.Readers.Marshaller.Get_Target_Uint32 (Msg, Def.Kind);

                  when M_FLAGS =>
                     Region.Flags := MAT.Readers.Marshaller.Get_Target_Uint32 (Msg, Def.Kind);

                  when others =>
                     MAT.Readers.Marshaller.Skip (Msg, Def.Size);
                  end case;
               end;
            end loop;
            if Kind = ELF.PT_LOAD then
               Region.Start_Addr := Addr + Region.Start_Addr;
               Region.End_Addr   := Region.Start_Addr + Region.Size;
               if Ada.Strings.Unbounded.Length (Path) = 0 then
                  Region.Path := Probe.Target.Process.Path;
                  Offset := 0;
               else
                  Region.Path := Path;
                  Offset := Region.Start_Addr;
               end if;
               Event.Size := Event.Size + Region.Size;
               Probe.Target.Process.Memory.Add_Region (Region);

               --  When auto-symbol loading is enabled, load the symbols associated with the
               --  shared libraries used by the program.
               if Probe.Target.Options.Load_Symbols then
                  begin
                     Probe.Target.Process.Symbols.Value.Load_Symbols (Region, Offset);
                  end;
               end if;
            end if;
         end;
      end loop;
   end Probe_Library;

   overriding
   procedure Extract (Probe  : in Process_Probe_Type;
                      Params : in MAT.Events.Const_Attribute_Table_Access;
                      Msg    : in out MAT.Readers.Message_Type;
                      Event  : in out MAT.Events.Targets.Probe_Event_Type) is
      use type MAT.Events.Targets.Probe_Index_Type;
   begin
      if Event.Index = MAT.Events.Targets.MSG_BEGIN then
         Probe.Probe_Begin (Params.all, Msg, Event);
      elsif Event.Index = MAT.Events.Targets.MSG_LIBRARY then
         Probe.Probe_Library (Params.all, Msg, Event);
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
      Into.Register_Probe (Probe.all'Access, "begin", MAT.Events.Targets.MSG_BEGIN,
                           Process_Attributes'Access);
      Into.Register_Probe (Probe.all'Access, "end", MAT.Events.Targets.MSG_END,
                           Process_Attributes'Access);
      Into.Register_Probe (Probe.all'Access, "shlib", MAT.Events.Targets.MSG_LIBRARY,
                           Library_Attributes'Access);
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
