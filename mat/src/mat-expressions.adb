-----------------------------------------------------------------------
--  mat-expressions -- Expressions for memory slot selection
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
with Ada.Unchecked_Deallocation;

with MAT.Types;
with MAT.Memory;
with MAT.Expressions.Parser;
package body MAT.Expressions is

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Node_Type,
                                     Name   => Node_Type_Access);

   --  Destroy recursively the node, releasing the storage.
   procedure Destroy (Node : in out Node_Type_Access);

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
   function Create_Inside (Name : in Ada.Strings.Unbounded.Unbounded_String;
                           Kind : in Inside_Type) return Expression_Type is
      Result : Expression_Type;
   begin
      Result.Node := new Node_Type'(Ref_Counter => Util.Concurrent.Counters.ONE,
                                    Kind        => N_INSIDE,
                                    Name        => Name,
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
   --  Create an event ID range expression node.
   --  ------------------------------
   function Create_Event (Min : in MAT.Events.Targets.Event_Id_Type;
                          Max : in MAT.Events.Targets.Event_Id_Type) return Expression_Type is
      Result : Expression_Type;
   begin
      Result.Node := new Node_Type'(Ref_Counter => Util.Concurrent.Counters.ONE,
                                    Kind        => N_EVENT,
                                    Min_Event   => Min,
                                    Max_Event   => Max);
      return Result;
   end Create_Event;

   --  ------------------------------
   --  Evaluate the expression to check if the memory slot described by the
   --  context is selected.  Returns True if the memory slot is selected.
   --  ------------------------------
   function Is_Selected (Node       : in Expression_Type;
                         Addr       : in MAT.Types.Target_Addr;
                         Allocation : in MAT.Memory.Allocation) return Boolean is
   begin
      if Node.Node = null then
         return True;
      else
         return Is_Selected (Node.Node.all, Addr, Allocation);
      end if;
   end Is_Selected;

   --  ------------------------------
   --  Evaluate the expression to check if the event described by the
   --  context is selected.  Returns True if the event is selected.
   --  ------------------------------
   function Is_Selected (Node       : in Expression_Type;
                         Event      : in MAT.Events.Targets.Probe_Event_Type) return Boolean is
   begin
      return Is_Selected (Node.Node.all, Event);
   end Is_Selected;

   --  ------------------------------
   --  Evaluate the node against the context.  Returns True if the node expression
   --  selects the memory slot defined by the context.
   --  ------------------------------
   function Is_Selected (Node       : in Node_Type;
                         Addr       : in MAT.Types.Target_Addr;
                         Allocation : in MAT.Memory.Allocation) return Boolean is
      use type MAT.Types.Target_Size;
      use type MAT.Types.Target_Tick_Ref;
   begin
      case Node.Kind is
         when N_NOT =>
            return not Is_Selected (Node.Expr.all, Addr, Allocation);

         when N_AND =>
            return Is_Selected (Node.Left.all, Addr, Allocation)
              and then Is_Selected (Node.Right.all, Addr, Allocation);

         when N_OR =>
            return Is_Selected (Node.Left.all, Addr, Allocation)
              or else Is_Selected (Node.Right.all, Addr, Allocation);

         when N_RANGE_SIZE =>
            return Allocation.Size >= Node.Min_Size
              and Allocation.Size <= Node.Max_Size;

         when N_RANGE_ADDR =>
            return Addr >= Node.Min_Addr
              and Addr <= Node.Max_Addr;

         when N_RANGE_TIME =>
            return Allocation.Time >= Node.Min_Time
              and Allocation.Time <= Node.Max_Time;

         when others =>
            return False;

      end case;
   end Is_Selected;

   --  ------------------------------
   --  Evaluate the expression to check if the event described by the
   --  context is selected.  Returns True if the event is selected.
   --  ------------------------------
   function Is_Selected (Node       : in Node_Type;
                         Event      : in MAT.Events.Targets.Probe_Event_Type) return Boolean is
      use type MAT.Types.Target_Size;
      use type MAT.Types.Target_Tick_Ref;
      use type MAT.Events.Targets.Event_Id_Type;
   begin
      case Node.Kind is
         when N_NOT =>
            return not Is_Selected (Node.Expr.all, Event);

         when N_AND =>
            return Is_Selected (Node.Left.all, Event)
              and then Is_Selected (Node.Right.all, Event);

         when N_OR =>
            return Is_Selected (Node.Left.all, Event)
              or else Is_Selected (Node.Right.all, Event);

         when N_RANGE_SIZE =>
            return Event.Size >= Node.Min_Size
              and Event.Size <= Node.Max_Size;

         when N_RANGE_ADDR =>
            return Event.Addr >= Node.Min_Addr
              and Event.Addr <= Node.Max_Addr;

         when N_RANGE_TIME =>
            return Event.Time >= Node.Min_Time
              and Event.Time <= Node.Max_Time;

         when N_EVENT =>
            return Event.Id >= Node.Min_Event
              and Event.Id <= Node.Max_Event;

         when others =>
            return False;

      end case;
   end Is_Selected;

   --  ------------------------------
   --  Parse the string and return the expression tree.
   --  ------------------------------
   function Parse (Expr : in String) return Expression_Type is
   begin
      return MAT.Expressions.Parser.Parse (Expr);
   end Parse;

   --  ------------------------------
   --  Destroy recursively the node, releasing the storage.
   --  ------------------------------
   procedure Destroy (Node : in out Node_Type_Access) is
      Release : Boolean;
   begin
      if Node /= null then
         Util.Concurrent.Counters.Decrement (Node.Ref_Counter, Release);
         if Release then
            case Node.Kind is
            when N_NOT =>
               Destroy (Node.Expr);

            when N_AND | N_OR =>
               Destroy (Node.Left);
               Destroy (Node.Right);

            when others =>
               null;
            end case;
            Free (Node);
         else
            Node := null;
         end if;
      end if;
   end Destroy;

   --  ------------------------------
   --  Release the reference and destroy the expression tree if it was the last reference.
   --  ------------------------------
   overriding
   procedure Finalize (Obj : in out Expression_Type) is
   begin
      if Obj.Node /= null then
         Destroy (Obj.Node);
      end if;
   end Finalize;

   --  ------------------------------
   --  Update the reference after an assignment.
   --  ------------------------------
   overriding
   procedure Adjust (Obj : in out Expression_Type) is
   begin
      if Obj.Node /= null then
         Util.Concurrent.Counters.Increment (Obj.Node.Ref_Counter);
      end if;
   end Adjust;

end MAT.Expressions;
