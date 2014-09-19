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
with MAT.Types; use MAT.Types;
package body MAT.Memory.Targets is

   procedure Create_Instance (Refs : in ClientInfo_Ref_Map) is
      --  Adapter : Manager       := Get_Manager (Refs);
      --  Client  : Client_Memory := new Client_Memory;
   begin
      --  Register_Client (Refs, "memory", Client.all'Access);
      --  Register_Servant (Adapter, Proxy);
      null;
   end Create_Instance;

end MAT.Memory.Targets;
