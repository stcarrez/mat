-----------------------------------------------------------------------
--  mat-expressions -- Expressions for event and memory slot selection
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
with Ada.Strings.Unbounded;
with Ada.Finalization;

private with Util.Concurrent.Counters;

with MAT.Types;
with MAT.Memory;
with MAT.Events.Targets;
package MAT.Expressions is

   type Resolver_Type is limited interface;
   type Resolver_Type_Access is access all Resolver_Type'Class;

   --  Find the region that matches the given name.
   function Find_Region (Resolver : in Resolver_Type;
                         Name     : in String) return MAT.Memory.Region_Info is abstract;

   --  Find the symbol in the symbol table and return the start and end address.
   function Find_Symbol (Resolver : in Resolver_Type;
                         Name     : in String) return MAT.Memory.Region_Info is abstract;

   type Context_Type is record
      Addr       : MAT.Types.Target_Addr;
      Allocation : MAT.Memory.Allocation;
   end record;

   type Inside_Type is (INSIDE_REGION,
                        INSIDE_DIRECT_REGION,
                        INSIDE_FUNCTION,
                        INSIDE_DIRECT_FUNCTION);

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
   function Create_Inside (Name : in Ada.Strings.Unbounded.Unbounded_String;
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

   --  Create an event ID range expression node.
   function Create_Event (Min : in MAT.Events.Targets.Event_Id_Type;
                          Max : in MAT.Events.Targets.Event_Id_Type) return Expression_Type;

   --  Create a thread ID range expression node.
   function Create_Thread (Min : in MAT.Types.Target_Thread_Ref;
                           Max : in MAT.Types.Target_Thread_Ref) return Expression_Type;

   --  Create a event type expression check.
   function Create_Event_Type (Event_Kind : in MAT.Events.Targets.Probe_Index_Type)
                               return Expression_Type;

   --  Create an expression node to keep allocation events which don't have any associated free.
   function Create_No_Free return Expression_Type;

   --  Evaluate the expression to check if the memory slot described by the
   --  context is selected.  Returns True if the memory slot is selected.
   function Is_Selected (Node       : in Expression_Type;
                         Addr       : in MAT.Types.Target_Addr;
                         Allocation : in MAT.Memory.Allocation) return Boolean;

   --  Evaluate the expression to check if the event described by the
   --  context is selected.  Returns True if the event is selected.
   function Is_Selected (Node       : in Expression_Type;
                         Event      : in MAT.Events.Targets.Probe_Event_Type) return Boolean;

   --  Parse the string and return the expression tree.
   function Parse (Expr     : in String;
                   Resolver : in Resolver_Type_Access) return Expression_Type;

   --  Empty expression.
   EMPTY : constant Expression_Type;

   type yystype is record
      low   : MAT.Types.Uint64 := 0;
      high  : MAT.Types.Uint64 := 0;
      bval  : Boolean := False;
      name  : Ada.Strings.Unbounded.Unbounded_String;
      expr  : Expression_Type;
   end record;

private

   type Kind_Type is (N_NOT, N_OR, N_AND, N_TRUE, N_FALSE,
                      N_IN_FILE, N_IN_FILE_DIRECT, N_INSIDE,
                      N_CALL_ADDR, N_CALL_ADDR_DIRECT,
                      N_IN_FUNC, N_IN_FUNC_DIRECT,
                      N_RANGE_SIZE, N_RANGE_ADDR,
                      N_RANGE_TIME, N_EVENT, N_HAS_ADDR,
                      N_CONDITION, N_THREAD, N_TYPE, N_NO_FREE);


   type Node_Type;
   type Node_Type_Access is access all Node_Type;

   type Node_Type (Kind : Kind_Type) is record
      Ref_Counter : Util.Concurrent.Counters.Counter;
      case Kind is
         when N_NOT =>
            Expr : Node_Type_Access;

         when N_OR | N_AND =>
            Left, Right : Node_Type_Access;

         when N_INSIDE | N_IN_FILE | N_IN_FILE_DIRECT =>
            Name   : Ada.Strings.Unbounded.Unbounded_String;
            Inside : Inside_Type;

         when N_RANGE_SIZE =>
            Min_Size : MAT.Types.Target_Size;
            Max_Size : MAT.Types.Target_Size;

         when N_RANGE_ADDR | N_CALL_ADDR | N_CALL_ADDR_DIRECT | N_HAS_ADDR
            | N_IN_FUNC | N_IN_FUNC_DIRECT =>
            Min_Addr : MAT.Types.Target_Addr;
            Max_Addr : MAT.Types.Target_Addr;

         when N_RANGE_TIME =>
            Min_Time : MAT.Types.Target_Tick_Ref;
            Max_Time : MAT.Types.Target_Tick_Ref;

         when N_THREAD =>
            Min_Thread : MAT.Types.Target_Thread_Ref;
            Max_Thread : MAT.Types.Target_Thread_Ref;

         when N_EVENT =>
            Min_Event : MAT.Events.Targets.Event_Id_Type;
            Max_Event : MAT.Events.Targets.Event_Id_Type;

         when N_TYPE =>
            Event_Kind : MAT.Events.Targets.Probe_Index_Type;

         when others =>
            null;
      end case;
   end record;

   --  Evaluate the node against the context.  Returns True if the node expression
   --  selects the memory slot defined by the context.
   function Is_Selected (Node       : in Node_Type;
                         Addr       : in MAT.Types.Target_Addr;
                         Allocation : in MAT.Memory.Allocation) return Boolean;

   --  Evaluate the expression to check if the event described by the
   --  context is selected.  Returns True if the event is selected.
   function Is_Selected (Node       : in Node_Type;
                         Event      : in MAT.Events.Targets.Probe_Event_Type) return Boolean;

   type Expression_Type is new Ada.Finalization.Controlled with record
      Node : Node_Type_Access;
   end record;

   --  Release the reference and destroy the expression tree if it was the last reference.
   overriding
   procedure Finalize (Obj : in out Expression_Type);

   --  Update the reference after an assignment.
   overriding
   procedure Adjust (Obj : in out Expression_Type);

   --  Empty expression.
   EMPTY : constant Expression_Type := Expression_Type'(Ada.Finalization.Controlled with
                                                        Node => null);

end MAT.Expressions;
