-----------------------------------------------------------------------
--  mat-targets-probes - Definition and Analysis of process start events
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
with MAT.Events.Targets;
with MAT.Events.Probes;
package MAT.Targets.Probes is

   type Process_Probe_Type is new MAT.Events.Probes.Probe_Type with record
      Target  : Target_Type_Access;
      Manager : MAT.Events.Probes.Probe_Manager_Type_Access;
      Process : Target_Process_Type_Access;
      Events  : MAT.Events.Targets.Target_Events_Access;
   end record;
   type Process_Probe_Type_Access is access all Process_Probe_Type'Class;

   --  Create a new process after the begin event is received from the event stream.
   procedure Create_Process (Probe : in out Process_Probe_Type;
                             Pid   : in MAT.Types.Target_Process_Ref;
                             Path  : in Ada.Strings.Unbounded.Unbounded_String);

   --  Extract the probe information from the message.
   overriding
   procedure Extract (Probe  : in Process_Probe_Type;
                      Params : in MAT.Events.Const_Attribute_Table_Access;
                      Msg    : in out MAT.Readers.Message_Type;
                      Event  : in out MAT.Events.Targets.Probe_Event_Type);

   procedure Execute (Probe : in Process_Probe_Type;
                      Event : in MAT.Events.Targets.Probe_Event_Type);

   --  Register the reader to extract and analyze process events.
   procedure Register (Into  : in out MAT.Events.Probes.Probe_Manager_Type'Class;
                       Probe : in Process_Probe_Type_Access);

   --  Initialize the target object to prepare for reading process events.
   procedure Initialize (Target  : in out Target_Type;
                         Manager : in out MAT.Events.Probes.Probe_Manager_Type'Class);

private

   procedure Probe_Begin (Probe : in out Process_Probe_Type;
                          Id          : in MAT.Events.Internal_Reference;
                          Defs        : in MAT.Events.Attribute_Table;
                          Frame       : in MAT.Events.Frame_Info;
                          Msg         : in out MAT.Readers.Message);

end MAT.Targets.Probes;
