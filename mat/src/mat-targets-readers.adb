-----------------------------------------------------------------------
--  mat-targets-readers - Definition and Analysis of process start events
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

with MAT.Readers.Marshaller;
package body MAT.Targets.Readers is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Targets.Readers");

   MSG_BEGIN  : constant MAT.Events.Internal_Reference := 0;
   MSG_END    : constant MAT.Events.Internal_Reference := 1;

   M_PID      : constant MAT.Events.Internal_Reference := 1;
   M_EXE      : constant MAT.Events.Internal_Reference := 2;

   PID_NAME   : aliased constant String := "pid";
   EXE_NAME   : aliased constant String := "exe";

   Process_Attributes : aliased constant MAT.Events.Attribute_Table :=
     (1 => (Name => PID_NAME'Access, Size => 0,
            Kind => MAT.Events.T_SIZE_T, Ref => M_PID),
      2 => (Name => EXE_NAME'Access, Size => 0,
            Kind => MAT.Events.T_FRAME, Ref => M_EXE));

   --  ------------------------------
   --  Create a new process after the begin event is received from the event stream.
   --  ------------------------------
   procedure Create_Process (For_Servant : in out Process_Servant;
                             Pid         : in MAT.Types.Target_Process_Ref) is
   begin
      For_Servant.Target.Create_Process (Pid     => Pid,
                                         Process => For_Servant.Process);
      MAT.Memory.Targets.Initialize (Memory => For_Servant.Process.Memory,
                                     Reader => For_Servant.Reader.all);
   end Create_Process;

   procedure Probe_Begin (For_Servant : in out Process_Servant;
                          Id          : in MAT.Events.Internal_Reference;
                          Defs        : in MAT.Events.Attribute_Table;
                          Frame       : in MAT.Events.Frame_Info;
                          Msg         : in out MAT.Readers.Message) is
      Pid  : MAT.Types.Target_Process_Ref := 0;
      Path : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I in Defs'Range loop
         declare
            Def : MAT.Events.Attribute renames Defs (I);
         begin
            case Def.Ref is
               when M_PID =>
                  Pid := MAT.Readers.Marshaller.Get_Target_Size (Msg.Buffer, Def.Kind);

               when M_EXE =>
                  Path := MAT.Readers.Marshaller.Get_String (Msg.Buffer);

               when others =>
                  MAT.Readers.Marshaller.Skip (Msg.Buffer, Def.Size);
            end case;
         end;
      end loop;
      For_Servant.Create_Process (Pid);
   end Probe_Begin;

   overriding
   procedure Dispatch (For_Servant : in out Process_Servant;
                       Id          : in MAT.Events.Internal_Reference;
                       Params      : in MAT.Events.Const_Attribute_Table_Access;
                       Frame       : in MAT.Events.Frame_Info;
                       Msg         : in out MAT.Readers.Message) is
   begin
      case Id is
         when MSG_BEGIN =>
            For_Servant.Probe_Begin (Id, Params.all, Frame, Msg);

         when MSG_END =>
            null;

         when others =>
            null;

      end case;
   end Dispatch;

   --  ------------------------------
   --  Register the reader to extract and analyze process events.
   --  ------------------------------
   procedure Register (Into   : in out MAT.Readers.Manager_Base'Class;
                       Reader : in Process_Reader_Access) is
   begin
      Reader.Reader := Into'Unchecked_Access;
      Into.Register_Reader (Reader.all'Access, "begin", MSG_BEGIN,
                            Process_Attributes'Access);
      Into.Register_Reader (Reader.all'Access, "end", MSG_END,
                            Process_Attributes'Access);
   end Register;

   --  ------------------------------
   --  Initialize the target object to prepare for reading process events.
   --  ------------------------------
   procedure Initialize (Target : in out Target_Type;
                         Reader : in out MAT.Readers.Manager_Base'Class) is
      Process_Reader : constant Process_Reader_Access
        := new Process_Servant;
   begin
      Process_Reader.Target := Target'Unrestricted_Access;
      Register (Reader, Process_Reader);
   end Initialize;

end MAT.Targets.Readers;