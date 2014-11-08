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
with MAT.Events;
with MAT.Readers;
package MAT.Targets.Readers is

   type Process_Servant is new MAT.Readers.Reader_Base with record
      Target  : Target_Type_Access;
      Reader  : MAT.Readers.Manager;
      Process : Target_Process_Type_Access;
   end record;
   type Process_Reader_Access is access all Process_Servant'Class;

   --  Create a new process after the begin event is received from the event stream.
   procedure Create_Process (For_Servant : in out Process_Servant;
                             Pid         : in MAT.Types.Target_Process_Ref);

   overriding
   procedure Dispatch (For_Servant : in out Process_Servant;
                       Id          : in MAT.Events.Internal_Reference;
                       Params      : in MAT.Events.Const_Attribute_Table_Access;
                       Frame       : in MAT.Events.Frame_Info;
                       Msg         : in out MAT.Readers.Message);

   --  Register the reader to extract and analyze process events.
   procedure Register (Into   : in out MAT.Readers.Manager_Base'Class;
                       Reader : in Process_Reader_Access);

   --  Initialize the target object to prepare for reading process events.
   procedure Initialize (Target : in out Target_Type;
                         Reader : in out MAT.Readers.Manager_Base'Class);

private

   procedure Probe_Begin (For_Servant : in out Process_Servant;
                          Id          : in MAT.Events.Internal_Reference;
                          Defs        : in MAT.Events.Attribute_Table;
                          Frame       : in MAT.Events.Frame_Info;
                          Msg         : in out MAT.Readers.Message);

end MAT.Targets.Readers;