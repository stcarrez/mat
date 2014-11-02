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
      Reader  : MAT.Readers.Manager;
      Process : Target_Process_Type_Access;
   end record;
   type Process_Reader_Access is access all Process_Servant'Class;

   overriding
   procedure Dispatch (For_Servant : in out Process_Servant;
                       Id          : in MAT.Events.Internal_Reference;
                       Params      : in MAT.Events.Const_Attribute_Table_Access;
                       Frame       : in MAT.Events.Frame_Info;
                       Msg         : in out MAT.Readers.Message);

   --  Register the reader to extract and analyze process events.
   procedure Register (Into   : in out MAT.Readers.Manager_Base'Class;
                       Reader : in Process_Reader_Access);

end MAT.Targets.Readers;
