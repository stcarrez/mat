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
      Result.Node := new Node_Type'(Ref_Counter => Util.Concurrent.Counters.ONE,
                                    Kind        => N_NOT,
                                    Expr        => Expr.Node);
      Util.Concurrent.Counters.Increment (Expr.Node.Ref_Counter);
      return Result;
   end Create_Not;

   --  ------------------------------
   --  Create a AND expression node.
   --  ------------------------------
   function Create_And (Left  : in Expression_Type;
                        Right : in Expression_Type) return Expression_Type is
      Result : Expression_Type;
   begin
      Result.Node := new Node_Type'(Ref_Counter => Util.Concurrent.Counters.ONE,
                                    Kind        => N_AND,
                                    Left        => Left.Node,
                                    Right       => Right.Node);
      Util.Concurrent.Counters.Increment (Left.Node.Ref_Counter);
      Util.Concurrent.Counters.Increment (Right.Node.Ref_Counter);
      return Result;
   end Create_And;

   --  ------------------------------
   --  Create a OR expression node.
   --  ------------------------------
   function Create_Or (Left  : in Expression_Type;
                       Right : in Expression_Type) return Expression_Type is
      Result : Expression_Type;
   begin
      Result.Node := new Node_Type'(Ref_Counter => Util.Concurrent.Counters.ONE,
                                    Kind        => N_OR,
                                    Left        => Left.Node,
                                    Right       => Right.Node);
      Util.Concurrent.Counters.Increment (Left.Node.Ref_Counter);
      Util.Concurrent.Counters.Increment (Right.Node.Ref_Counter);
      return Result;
   end Create_Or;

   --  ------------------------------
   --  Create an INSIDE expression node.
   --  ------------------------------
   function Create_Inside (Name : in String;
                           Kind : in Inside_Type) return Expression_Type is
      Result : Expression_Type;
   begin
      Result.Node := new Node_Type'(Ref_Counter => Util.Concurrent.Counters.ONE,
                                    Kind        => N_INSIDE,
                                    Name        => Ada.Strings.Unbounded.To_Unbounded_String (Name),
                                    Inside      => Kind);
      return Result;
   end Create_Inside;

   --  ------------------------------
   --  Create an size range expression node.
   --  ------------------------------
   function Create_Size (Min : in MAT.Types.Target_Size;
                         Max : in MAT.Types.Target_Size) return Expression_Type is
      Result : Expression_Type;
   begin
      Result.Node := new Node_Type'(Ref_Counter => Util.Concurrent.Counters.ONE,
                                    Kind        => N_RANGE_SIZE,
                                    Min_Size    => Min,
                                    Max_Size    => Max);
      return Result;
   end Create_Size;

   --  ------------------------------
   --  Create an addr range expression node.
   --  ------------------------------
   function Create_Addr (Min : in MAT.Types.Target_Addr;
                         Max : in MAT.Types.Target_Addr) return Expression_Type is
      Result : Expression_Type;
   begin
      Result.Node := new Node_Type'(Ref_Counter => Util.Concurrent.Counters.ONE,
                                    Kind        => N_RANGE_ADDR,
                                    Min_Addr    => Min,
                                    Max_Addr    => Max);
      return Result;
   end Create_Addr;

end MAT.Expressions;
