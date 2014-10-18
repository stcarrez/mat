-----------------------------------------------------------------------
--  mat-expressions -- Expressions for memory slot selection
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
with MAT.Memory;
package MAT.Expressions is

   type Context_Type is record
      Addr       : MAT.Types.Target_Addr;
      Allocation : MAT.Memory.Allocation;
   end record;

   type Kind_Type is (N_NOT, N_OR, N_AND, N_TRUE, N_FALSE,
                      N_IN_FILE, N_IN_FILE_DIRECT,
                      N_IN_FUNC, N_IN_FUNC_DIRECT,
                      N_RANGE_SIZE, N_RANGE_ADDR,
                      N_CONDITION, N_THREAD);

   type Expression_Type is tagged private;

   --  Create a new expression node.
   function Create (Kind : in Kind_Type;
                    Expr : in Expression_Type) return Expression_Type;

   --  Evaluate the expression to check if the memory slot described by the
   --  context is selected.  Returns True if the memory slot is selected.
   function Is_Selected (Node    : in Expression_Type;
                         Context : in Context_Type) return Boolean;

private

   type Node_Type (Kind : Kind_Type) is tagged limited record
      N : Natural;
   end record;
   type Node_Type_Access is access all Node_Type'Class;

   function Is_Selected (Node    : in Node_Type;
                         Context : in Context_Type) return Boolean;

   type Expression_Type is tagged record
      Node : Node_Type_Access;
   end record;

end MAT.Expressions;
