-----------------------------------------------------------------------
--  Frames - Representation of stack frames
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
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with MAT.Types; use MAT.Types;
package body MAT.Frames is

   procedure Free is
      new Ada.Unchecked_Deallocation (Frame, Frame_Ptr);

   --  Return the parent frame.
   function Parent (F : in Frame_Ptr) return Frame_Ptr is
   begin
      return F.Parent;
   end Parent;

   --  Returns the backtrace of the current frame (up to the root).
   function Backtrace (F :  in Frame_Ptr) return PC_Table is
      Pc      : Pc_Table (1 .. F.Depth);
      Current : Frame_Ptr := F;
      Pos     : Natural   := Current.Depth;
      New_Pos : Natural;
   begin
      while Pos /= 0 loop
         New_Pos := Pos - Current.Local_Depth + 1;
         Pc (New_Pos .. Pos) := Current.Calls (1 .. Current.Local_Depth);
         Pos     := New_Pos - 1;
         Current := Current.Parent;
      end loop;
      return Pc;
   end Backtrace;

   --  Returns the number of children in the frame.
   --  When recursive is true, compute in the sub-tree.
   function Count_Children (F : in Frame_Ptr;
                            Recursive : in Boolean := False) return Natural is
      Count : Natural   := 0;
      N : Natural;
      Child : Frame_Ptr := F.Children;
   begin
      while Child /= null loop
         Count := Count + 1;
         if Recursive then
            declare
               N : Natural := Count_Children (Child, True);
            begin
               if N > 0 then
                  N := N - 1;
               end if;
               Count := Count + N;
            end;
         end if;
         Child := Child.Next;
      end loop;
      return Count;
   end Count_Children;

   --  Returns all the direct calls made by the current frame.
   function Calls (F : in Frame_Ptr) return PC_Table is
      Nb_Calls : Natural   := Count_Children (F);
      Pc       : Pc_Table (1 .. Nb_Calls);
      Child    : Frame_Ptr := F.Children;
      Pos      : Natural   := 1;
   begin
      while Child /= null loop
         Pc (Pos) := Child.Calls (1);
         Pos   := Pos + 1;
         Child := Child.Next;
      end loop;
      return Pc;
   end Calls;

   --  Returns the current stack depth (# of calls from the root
   --  to reach the frame).
   function Current_Depth (F : in Frame_Ptr) return Natural is
   begin
      return F.Depth;
   end Current_Depth;

   --  Create a root for stack frame representation.
   function Create_Root return Frame_Ptr is
   begin
      return new Frame;
   end Create_Root;

   --  Destroy the frame tree recursively.
   procedure Destroy (Tree : in out Frame_Ptr) is
      F : Frame_Ptr;
   begin
      --  Destroy its children recursively.
      while Tree.Children /= null loop
         F := Tree.Children;
         Destroy (F);
      end loop;

      --  Unlink from parent list.
      if Tree.Parent /= null then
         F := Tree.Parent.Children;
         if F = Tree then
            Tree.Parent.Children := Tree.Next;
         else
            while F /= null and F.Next /= Tree loop
               F := F.Next;
            end loop;
            if F = null then
               raise Program_Error;
            end if;
            F.Next := Tree.Next;
         end if;
      end if;
      Free (Tree);
   end Destroy;

   --  Release the frame when its reference is no longer necessary.
   procedure Release (F : in Frame_Ptr) is
      Current : Frame_Ptr := F;
   begin
      --  Scan the fram until the root is reached
      --  and decrement the used counter.  Free the frames
      --  when the used counter reaches 0.
      while Current /= null loop
         if Current.Used <= 1 then
            declare
               Tree : Frame_Ptr := Current;
            begin
               Current := Current.Parent;
               Destroy (Tree);
            end;
         else
            Current.Used := Current.Used - 1;
            Current := Current.Parent;
         end if;
      end loop;
   end Release;

   --  Split the node pointed to by `F' at the position `Pos'
   --  in the caller chain.  A new parent is created for the node
   --  and the brothers of the node become the brothers of the
   --  new parent.
   --
   --  Returns in `F' the new parent node.
   procedure Split (F : in out Frame_Ptr; Pos : in Positive) is

      --  Before:                       After:
      --
      --    +-------+             +-------+
      --  /-|  P    |           /-|  P    |
      --  | +-------+           | +-------+
      --  |       ^             |       ^
      --  |   +-------+         |    +-------+
      --  ...>| node  |...      ....>|  new  |...        (0..N brothers)
      --      +-------+              +-------+
      --       |     ^              |      ^
      --       | +-------+          | +-------+
      --       ->|  c    |          ->| node  |-->0      (0 brother)
      --         +-------+            +-------+
      --                                    |
      --                                +-------+
      --                                |   c   |
      --                                +-------+
      --
      New_Parent : Frame_Ptr := new Frame '(Parent      => F.Parent,
                                            Next        => F.Next,
                                            Children    => F,
                                            Used        => F.Used,
                                            Depth       => F.Depth,
                                            Local_Depth => Pos,
                                            Calls       => (others => 0));
      Child : Frame_Ptr := F.Parent.Children;
   begin
      --  Move the PC values in the new parent.
      New_Parent.Calls (1 .. Pos) := F.Calls (1 .. Pos);
      F.Calls (1 .. F.Local_Depth - Pos) := F.Calls (Pos + 1 .. F.Local_Depth);
      F.Parent         := New_Parent;
      F.Next           := null;
      New_Parent.Depth := F.Depth - F.Local_Depth + Pos;
      F.Local_Depth    := F.Local_Depth - Pos;

      --  Remove F from its parent children list and replace if with New_Parent.
      if Child = F then
         New_Parent.Parent.Children := New_Parent;
      else
         while Child.Next /= F loop
            Child := Child.Next;
         end loop;
         Child.Next := New_Parent;
      end if;
      F := New_Parent;
   end Split;

   procedure Add_Frame (F  : in Frame_Ptr;
                        Pc : in Pc_Table;
                        Result : out Frame_Ptr) is
      Child         : Frame_Ptr := F;
      Pos           : Positive  := Pc'First;
      Current_Depth : Natural   := F.Depth;
      Cnt           : Local_Depth_Type;
   begin
      while Pos <= Pc'Last loop
         Cnt := Frame_Group_Size;
         if Pos + Cnt > Pc'Last then
            Cnt := Pc'Last - Pos + 1;
         end if;
         Current_Depth := Current_Depth + Cnt;
         Child := new Frame '(Parent      => Child,
                              Next        => Child.Children,
                              Children    => null,
                              Used        => 1,
                              Depth       => Current_Depth,
                              Local_Depth => Cnt,
                              Calls       => (others => 0));
         Child.Calls (1 .. Cnt) := Pc (Pos .. Pos + Cnt - 1);
         Pos := Pos + Cnt;
         Child.Parent.Children := Child;
      end loop;
      Result := Child;
   end Add_Frame;

   procedure Insert (F : in Frame_Ptr;
                     Pc : in Pc_Table;
                     Result : out Frame_Ptr) is
      Current : Frame_Ptr := F;
      Child   : Frame_Ptr;
      Pos     : Positive  := Pc'First;
      Lpos    : Positive  := 1;
      Addr    : Target_Addr;
   begin
      while Pos <= Pc'Last loop
         Addr := Pc (Pos);
         if Lpos <= Current.Local_Depth then
            if Addr = Current.Calls (Lpos) then
               Lpos := Lpos + 1;
               Pos  := Pos + 1;

               --  Split this node
            else
               if Lpos > 1 then
                  Split (Current, Lpos - 1);
               end if;
               Add_Frame (Current, Pc (Pos .. Pc'Last), Result);
               return;
            end if;
         else
            --  Find the first child which has the address.
            Child := Current.Children;
            while Child /= null loop
               exit when Child.Calls (1) = Addr;
               Child := Child.Next;
            end loop;
            if Child = null then
               Add_Frame (Current, Pc (Pos .. Pc'Last), Result);
               return;
            end if;
            Current := Child;
            Lpos    := 2;
            Pos     := Pos + 1;
            Current.Used := Current.Used + 1;
         end if;
      end loop;
      if Lpos <= Current.Local_Depth then
         Split (Current, Lpos - 1);
      end if;
      Result := Current;
   end Insert;

   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
   function Find (F : in Frame_Ptr; Pc : in Target_Addr) return Frame_Ptr is
      Child : Frame_Ptr := F.Children;
   begin
      while Child /= null loop
         if Child.Local_Depth >= 1 and then Child.Calls (1) = Pc then
            return Child;
         end if;
         Child := Child.Next;
      end loop;
      raise Not_Found;
   end Find;

   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
   function Find (F : in Frame_Ptr; Pc : in PC_Table) return Frame_Ptr is
      Child : Frame_Ptr := F;
      Pos   : Positive  := Pc'First;
      Lpos  : Positive;
   begin
      while Pos <= Pc'Last loop
         Child := Find (Child, Pc (Pos));
         Pos  := Pos + 1;
         Lpos := 2;
         --  All the PC of the child frame must match.
         while Pos <= Pc'Last and Lpos <= Child.Local_Depth loop
            if Child.Calls (Lpos) /= Pc (Pos) then
               raise Not_Found;
            end if;
            Lpos := Lpos + 1;
            Pos  := Pos + 1;
         end loop;
      end loop;
      return Child;
   end Find;

   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
   procedure Find (F       : in Frame_Ptr;
                   Pc      : in PC_Table;
                   Result  : out Frame_Ptr;
                   Last_Pc : out Natural) is
      Current : Frame_Ptr := F;
      Pos     : Positive  := Pc'First;
      Lpos    : Positive;
   begin
      Main_Search:
      while Pos <= Pc'Last loop
         declare
            Addr  : Target_Addr := Pc (Pos);
            Child : Frame_Ptr   := Current.Children;
         begin
            --  Find the child which has the corresponding PC.
            loop
               exit Main_Search when Child = null;
               exit when Child.Local_Depth >= 1 and Child.Calls (1) = Addr;
               Child := Child.Next;
            end loop;

            Current := Child;
            Pos  := Pos + 1;
            Lpos := 2;
            --  All the PC of the child frame must match.
            while Pos <= Pc'Last and Lpos <= Current.Local_Depth loop
               exit Main_Search when Current.Calls (Lpos) /= Pc (Pos);
               Lpos := Lpos + 1;
               Pos  := Pos + 1;
            end loop;
         end;
      end loop Main_Search;
      Result := Current;
      if Pos > Pc'Last then
         Last_Pc := 0;
      else
         Last_Pc := Pos;
      end if;
   end Find;

end MAT.Frames;
