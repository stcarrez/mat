-----------------------------------------------------------------------
--  Clients - Abstract representation of client information
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
package body MAT.Targets is

   function Find_Sibling (Client : ClientInfo_Ref;
                          Kind : in String) return ClientInfo_Ref is
   begin
      return null;
   end Find_Sibling;

   function Find (Container : ClientInfo_Ref_Map;
                  Kind : in String) return ClientInfo_Ref is
--        It : Iterator := Find (Container.Map, +Kind);
   begin
      return null;
--        if Is_Done (It) then
--  	 return null;
--        else
--  	 return Current_Item (It);
--        end if;
   end Find;

   procedure Register_Client (Refs   : in out ClientInfo_Ref_Map;
                              Name   : in String;
                              Client : in ClientInfo_Ref) is
      Inserted : Boolean;
   begin
      --        Insert (Refs.Map, Client, +Name, Inserted);
      null;
   end Register_Client;

end MAT.Targets;
