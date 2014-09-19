-----------------------------------------------------------------------
--  Targets - Abstract representation of target information
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
with MAT.Types;
with Ada.Strings.Unbounded;
package MAT.Targets is

   use Ada.Strings.Unbounded;

   subtype ClientInfoType is Ada.Strings.Unbounded.Unbounded_String;

   function "+" (S : String) return ClientInfoType renames To_Unbounded_String;

   type ClientInfo is tagged limited private;

   type ClientInfo_Ref is access all ClientInfo'Class;

   type ClientInfo_Ref_Map is limited private;

   function Find_Sibling (Client : ClientInfo_Ref;
                          Kind : in String) return ClientInfo_Ref;

   function Find (Container : ClientInfo_Ref_Map;
                  Kind : in String) return ClientInfo_Ref;

   procedure Register_Client (Refs : in out ClientInfo_Ref_Map;
                              Name : in String;
                              Client : in ClientInfo_Ref);

private
   type ClientInfo is tagged limited null record;
--
--     package Clients_Containers is new BC.Containers (Item => ClientInfo_Ref);
--     package Clients_Trees is new Clients_Containers.Trees;
--     package Clients_AVL is
--       new Clients_Trees.AVL (Key     => ClientInfoType,
--                              Storage => Global_Heap.Storage);

   type ClientInfo_Ref_Map is limited record
      Map : Natural; --  Clients_AVL.AVL_Tree;
   end record;

end MAT.Targets;
