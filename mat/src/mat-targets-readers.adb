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

   procedure Create_Process (For_Servant : in out Process_Servant;
                             Pid         : in MAT.Types.Target_Process_Ref) is
   begin
      MAT.Memory.Targets.Initialize (Memory => For_Servant.Process.Memory,
                                     Reader => For_Servant.Reader.all);
   end Create_Process;

   overriding
   procedure Dispatch (For_Servant : in out Process_Servant;
                       Id          : in MAT.Events.Internal_Reference;
                       Params      : in MAT.Events.Const_Attribute_Table_Access;
                       Frame       : in MAT.Events.Frame_Info;
                       Msg         : in out MAT.Readers.Message) is
   begin
      case Id is
         when MSG_BEGIN =>
            null;

         when MSG_END =>
            null;

         when others =>
            null;

      end case;
   end Dispatch;

   --  Register the reader to extract and analyze process events.
   procedure Register (Into   : in out MAT.Readers.Manager_Base'Class;
                       Reader : in Process_Reader_Access) is
   begin
      Into.Register_Reader (Reader.all'Access, "begin", MSG_BEGIN,
                            Process_Attributes'Access);
      Into.Register_Reader (Reader.all'Access, "end", MSG_END,
                            Process_Attributes'Access);
   end Register;

end MAT.Targets.Readers;
