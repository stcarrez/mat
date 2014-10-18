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
with Ada.Strings.Unbounded;

private with Util.Concurrent.Counters;

with MAT.Types;
with MAT.Memory;
package MAT.Expressions is

   type Context_Type is record
      Addr       : MAT.Types.Target_Addr;
      Allocation : MAT.Memory.Allocation;
   end record;

   type Inside_Type is (INSIDE_FILE, INSIDE_FUNCTION);

   type Expression_Type is tagged private;

   --  Create a NOT expression node.
   function Create_Not (Expr : in Expression_Type) return Expression_Type;

   --  Create a AND expression node.
   function Create_And (Left  : in Expression_Type;
                        Right : in Expression_Type) return Expression_Type;

   --  Create a OR expression node.
   function Create_Or (Left  : in Expression_Type;
                       Right : in Expression_Type) return Expression_Type;

   --  Create an INSIDE expression node.
   function Create_Inside (Name : in String;
                           Kind : in Inside_Type) return Expression_Type;

   --  Create an size range expression node.
   function Create_Size (Min : in MAT.Types.Target_Size;
                         Max : in MAT.Types.Target_Size) return Expression_Type;

   --  Create an addr range expression node.
   function Create_Addr (Min : in MAT.Types.Target_Addr;
                         Max : in MAT.Types.Target_Addr) return Expression_Type;

   --  Create an time range expression node.
   function Create_Time (Min : in MAT.Types.Target_Tick_Ref;
                         Max : in MAT.Types.Target_Tick_Ref) return Expression_Type;

   --  Evaluate the expression to check if the memory slot described by the
   --  context is selected.  Returns True if the memory slot is selected.
   function Is_Selected (Node    : in Expression_Type;
                         Context : in Context_Type) return Boolean;

private

   type Kind_Type is (N_NOT, N_OR, N_AND, N_TRUE, N_FALSE,
                      N_IN_FILE, N_IN_FILE_DIRECT, N_INSIDE,
                      N_CALL_ADDR, N_CALL_ADDR_DIRECT,
                      N_IN_FUNC, N_IN_FUNC_DIRECT,
                      N_RANGE_SIZE, N_RANGE_ADDR,
                      N_RANGE_TIME,
                      N_CONDITION, N_THREAD);

   type Node_Type;
   type Node_Type_Access is access all Node_Type;

   type Node_Type (Kind : Kind_Type) is record
      Ref_Counter : Util.Concurrent.Counters.Counter;
      case Kind is
         when N_NOT =>
            Expr : Node_Type_Access;

         when N_OR | N_AND =>
            Left, Right : Node_Type_Access;

         when N_INSIDE | N_IN_FILE | N_IN_FILE_DIRECT | N_IN_FUNC | N_IN_FUNC_DIRECT =>
            Name   : Ada.Strings.Unbounded.Unbounded_String;
            Inside : Inside_Type;

         when N_RANGE_SIZE =>
            Min_Size : MAT.Types.Target_Size;
            Max_Size : MAT.Types.Target_Size;

         when N_RANGE_ADDR | N_CALL_ADDR | N_CALL_ADDR_DIRECT =>
            Min_Addr : MAT.Types.Target_Addr;
            Max_Addr : MAT.Types.Target_Addr;

         when N_RANGE_TIME =>
            Min_Time : MAT.Types.Target_Tick_Ref;
            Max_Time : MAT.Types.Target_Tick_Ref;

         when N_THREAD =>
            Thread : MAT.Types.Target_Thread_Ref;

         when others =>
            null;
      end case;
   end record;

   --  Evaluate the node against the context.  Returns True if the node expression
   --  selects the memory slot defined by the context.
   function Is_Selected (Node    : in Node_Type;
                         Context : in Context_Type) return Boolean;

   type Expression_Type is tagged record
      Node : Node_Type_Access;
   end record;

end MAT.Expressions;
