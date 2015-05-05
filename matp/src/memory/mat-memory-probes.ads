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
with MAT.Events;
with MAT.Readers;
with MAT.Events.Targets;
with MAT.Events.Probes;
with MAT.Memory.Targets;
package MAT.Memory.Probes is

   type Memory_Probe_Type is new MAT.Events.Probes.Probe_Type with record
      Data  : access MAT.Memory.Targets.Target_Memory;
   end record;
   type Memory_Probe_Type_Access is access all Memory_Probe_Type'Class;

   --  Extract the probe information from the message.
   overriding
   procedure Extract (Probe   : in Memory_Probe_Type;
                      Params  : in MAT.Events.Const_Attribute_Table_Access;
                      Msg     : in out MAT.Readers.Message_Type;
                      Event   : in out MAT.Events.Target_Event_Type);

   procedure Execute (Probe : in Memory_Probe_Type;
                      Event : in out MAT.Events.Target_Event_Type);

   --  Register the reader to extract and analyze memory events.
   procedure Register (Into   : in out MAT.Events.Probes.Probe_Manager_Type'Class;
                       Probe : in Memory_Probe_Type_Access);

end MAT.Memory.Probes;
