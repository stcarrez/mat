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
package body MAT.Expressions is

   --  ------------------------------
   --  Create a NOT expression node.
   --  ------------------------------
   function Create_Not (Expr : in Expression_Type) return Expression_Type is
      Result : Expression_Type;
   begin
      Result.Node := new Node_Type'(Util.Refs.Ref_Entity with
                                      Kind => N_NOT, Expr => Expr.Node);
      return Result;
   end Create_Not;

end MAT.Expressions;
