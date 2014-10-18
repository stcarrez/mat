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

   --  ------------------------------
   --  Create an time range expression node.
   --  ------------------------------
   function Create_Time (Min : in MAT.Types.Target_Tick_Ref;
                         Max : in MAT.Types.Target_Tick_Ref) return Expression_Type is
      Result : Expression_Type;
   begin
      Result.Node := new Node_Type'(Ref_Counter => Util.Concurrent.Counters.ONE,
                                    Kind        => N_RANGE_TIME,
                                    Min_Time    => Min,
                                    Max_Time    => Max);
      return Result;
   end Create_Time;

   --  ------------------------------
   --  Evaluate the expression to check if the memory slot described by the
   --  context is selected.  Returns True if the memory slot is selected.
   --  ------------------------------
   function Is_Selected (Node    : in Expression_Type;
                         Context : in Context_Type) return Boolean is
   begin
      if Node.Node = null then
         return False;
      else
         return Is_Selected (Node.Node.all, Context);
      end if;
   end Is_Selected;

   --  ------------------------------
   --  Evaluate the node against the context.  Returns True if the node expression
   --  selects the memory slot defined by the context.
   --  ------------------------------
   function Is_Selected (Node    : in Node_Type;
                         Context : in Context_Type) return Boolean is
      use type MAT.Types.Target_Size;
      use type MAT.Types.Target_Tick_Ref;
   begin
      case Node.Kind is
         when N_NOT =>
            return not Is_Selected (Node.Expr.all, Context);

         when N_AND =>
            return Is_Selected (Node.Left.all, Context)
              and then Is_Selected (Node.Right.all, Context);

         when N_OR =>
            return Is_Selected (Node.Left.all, Context)
              or else Is_Selected (Node.Right.all, Context);

         when N_RANGE_SIZE =>
            return Context.Allocation.Size >= Node.Min_Size
              and Context.Allocation.Size <= Node.Max_Size;

         when N_RANGE_ADDR =>
            return Context.Addr >= Node.Min_Addr
              and Context.Addr <= Node.Max_Addr;

         when N_RANGE_TIME =>
            return Context.Allocation.Time >= Node.Min_Time
              and Context.Allocation.Time <= Node.Max_Time;

         when others =>
            return False;

      end case;
   end Is_Selected;

end MAT.Expressions;
