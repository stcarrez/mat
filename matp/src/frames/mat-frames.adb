-----------------------------------------------------------------------
--  mat-frames - Representation of stack frames
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

with Util.Log.Loggers;
package body MAT.Frames is

   use type MAT.Types.Target_Addr;

   procedure Free is
     new Ada.Unchecked_Deallocation (Frame, Frame_Type);

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Frames.Targets");

   procedure Split (F     : in out Frame_Type;
                    Pos   : in Positive);

   procedure Add_Frame (F      : in Frame_Type;
                        Pc     : in Frame_Table;
                        Result : out Frame_Type);

   --  Get the root frame object.
   function Get_Root (Frame : in Frame_Type) return Frame_Type;
   function Check (Frame : in Frame_Type) return Boolean;

   function Check (Frame : in Frame_Type) return Boolean is
      Child  : Frame_Type;
      Result : Boolean := True;
   begin
      if Frame = null then
         return False;
      end if;
      Child := Frame.Children;
      while Child /= null loop
         if Child.Parent /= Frame then
            Log.Error ("Invalid parent link");
            Result := False;
         end if;
         if Child.Children /= null and then not Check (Child) then
            Log.Error ("A child is now invalid");
            Result := False;
         end if;
         Child := Child.Next;
      end loop;
      return Result;
   end Check;

   --  ------------------------------
   --  Return the parent frame.
   --  ------------------------------
   function Parent (Frame : in Frame_Type) return Frame_Type is
   begin
      if Frame = null then
         return null;
      else
         return Frame.Parent;
      end if;
   end Parent;

   --  ------------------------------
   --  Get the root frame object.
   --  ------------------------------
   function Get_Root (Frame : in Frame_Type) return Frame_Type is
      Parent : Frame_Type := Frame;
   begin
      loop
         if Parent.Parent = null then
            return Parent;
         end if;
         Parent := Parent.Parent;
      end loop;
   end Get_Root;

   --  ------------------------------
   --  Returns the backtrace of the current frame (up to the root).
   --  When <tt>Max_Level</tt> is positive, limit the number of PC frames to the value.
   --  ------------------------------
   function Backtrace (Frame     : in Frame_Type;
                       Max_Level : in Natural := 0) return Frame_Table is
      Length  : Natural;
   begin
      if Max_Level > 0 and Max_Level < Frame.Depth then
         Length := Max_Level;
      else
         Length := Frame.Depth;
      end if;
      declare
         Current : Frame_Type := Frame;
         Pos     : Natural    := Length;
         New_Pos : Natural;
         Pc      : Frame_Table (1 .. Length);
      begin
         while Current /= null and Pos /= 0 loop
            if Pos >= Current.Local_Depth then
               New_Pos := Pos - Current.Local_Depth + 1;
               Pc (New_Pos .. Pos) := Current.Calls (1 .. Current.Local_Depth);
               Pos     := New_Pos - 1;
            else
               Pc (1 .. Pos) := Current.Calls (Current.Local_Depth - Pos + 1 .. Current.Local_Depth);
               Pos := 0;
            end if;
            Current := Current.Parent;
         end loop;
         return Pc;
      end;
   end Backtrace;

   --  ------------------------------
   --  Returns the number of children in the frame.
   --  When recursive is true, compute in the sub-tree.
   --  ------------------------------
   function Count_Children (Frame     : in Frame_Type;
                            Recursive : in Boolean := False) return Natural is
      Count : Natural := 0;
      Child : Frame_Type;
   begin
      if Frame /= null then
         Child := Frame.Children;
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
      end if;
      return Count;
   end Count_Children;

   --  ------------------------------
   --  Returns all the direct calls made by the current frame.
   --  ------------------------------
   function Calls (Frame : in Frame_Type) return Frame_Table is
      Nb_Calls : constant Natural   := Count_Children (Frame);
      Pc       : Frame_Table (1 .. Nb_Calls);
   begin
      if Frame /= null then
         declare
            Child : Frame_Type := Frame.Children;
            Pos   : Natural   := 1;
         begin
            while Child /= null loop
               Pc (Pos) := Child.Calls (1);
               Pos   := Pos + 1;
               Child := Child.Next;
            end loop;
         end;
      end if;
      return Pc;
   end Calls;

   --  ------------------------------
   --  Returns the current stack depth (# of calls from the root
   --  to reach the frame).
   --  ------------------------------
   function Current_Depth (Frame : in Frame_Type) return Natural is
   begin
      if Frame = null then
         return 0;
      else
         return Frame.Depth;
      end if;
   end Current_Depth;

   --  ------------------------------
   --  Create a root for stack frame representation.
   --  ------------------------------
   function Create_Root return Frame_Type is
   begin
      return new Frame;
   end Create_Root;

   --  ------------------------------
   --  Destroy the frame tree recursively.
   --  ------------------------------
   procedure Destroy (Frame : in out Frame_Type) is
      F : Frame_Type;
   begin
      if Frame = null then
         return;
      end if;
      --  Destroy its children recursively.
      while Frame.Children /= null loop
         F := Frame.Children;
         Destroy (F);
      end loop;

      --  Unlink from parent list.
      if Frame.Parent /= null then
         F := Frame.Parent.Children;
         if F = Frame then
            Frame.Parent.Children := Frame.Next;
         else
            while F /= null and then F.Next /= Frame loop
               F := F.Next;
            end loop;
            if F = null then
--                 Log.Error ("Frame is not linked to the correct parent");
--                 if not Check (Get_Root (Frame)) then
--                    Log.Error ("Check failed");
--                 else
--                    Log.Error ("Check is ok");
--                 end if;
--                 MAT.Frames.Print (Ada.Text_IO.Standard_Output, Get_Root (Frame));
               return;
            else
               F.Next := Frame.Next;
            end if;
         end if;
      end if;
--        Free (Frame);
   end Destroy;

   --  ------------------------------
   --  Release the frame when its reference is no longer necessary.
   --  ------------------------------
   procedure Release (Frame : in Frame_Type) is
      Current : Frame_Type := Frame;
   begin
      --  Scan the frame until the root is reached
      --  and decrement the used counter.  Free the frames
      --  when the used counter reaches 0.
      while Current /= null loop
         if Current.Used <= 1 then
            declare
               Tree : Frame_Type := Current;
            begin
               Current := Current.Parent;
               Destroy (Tree);
            end;
         else
            Current.Used := Current.Used - 1;
            Current := Current.Parent;
         end if;
      end loop;
--        if not Check (Get_Root (Frame)) then
--           Log.Error ("Frame is invalid");
--        end if;
   end Release;

   --  ------------------------------
   --  Split the node pointed to by `F' at the position `Pos'
   --  in the caller chain.  A new parent is created for the node
   --  and the brothers of the node become the brothers of the
   --  new parent.
   --
   --  Returns in `F' the new parent node.
   --  ------------------------------
   procedure Split (F     : in out Frame_Type;
                    Pos   : in Positive) is

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
      New_Parent : constant Frame_Type := new Frame '(Parent      => F.Parent,
                                                      Next        => F.Next,
                                                      Children    => F,
                                                      Used        => F.Used - 1,
                                                      Depth       => F.Depth,
                                                      Local_Depth => Pos,
                                                      Calls       => (others => 0));
      Child : Frame_Type := F.Parent.Children;
   begin
      Log.Debug ("Split frame");

      --  Move the PC values in the new parent.
      New_Parent.Calls (1 .. Pos) := F.Calls (1 .. Pos);
      F.Calls (1 .. F.Local_Depth - Pos) := F.Calls (Pos + 1 .. F.Local_Depth);
      F.Parent         := New_Parent;
      F.Next           := null;
      F.Used           := 1;
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
--        if not Check (Get_Root (F)) then
--           Log.Error ("Error when splitting frame");
--        end if;
   end Split;

   procedure Add_Frame (F      : in Frame_Type;
                        Pc     : in Frame_Table;
                        Result : out Frame_Type) is
      Child         : Frame_Type := F;
      Pos           : Positive   := Pc'First;
      Current_Depth : Natural    := F.Depth;
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
--           if not Check (Get_Root (Child)) then
--              Log.Error ("Error when adding frame");
--           end if;
      end loop;
      Result := Child;
   end Add_Frame;

   --  ------------------------------
   --  Insert in the frame tree the new stack frame represented by <tt>Pc</tt>.
   --  If the frame is already known, the frame reference counter is incremented.
   --  The frame represented by <tt>Pc</tt> is returned in <tt>Result</tt>.
   --  ------------------------------
   procedure Insert (Frame  : in Frame_Type;
                     Pc     : in Frame_Table;
                     Result : out Frame_Type) is
      Current : Frame_Type := Frame;
      Child   : Frame_Type;
      Pos     : Positive  := Pc'First;
      Lpos    : Positive  := 1;
      Addr    : MAT.Types.Target_Addr;
   begin
      while Pos <= Pc'Last loop
         Addr := Pc (Pos);
         if Lpos <= Current.Local_Depth then
            if Addr = Current.Calls (Lpos) then
               Lpos := Lpos + 1;
               Pos  := Pos + 1;

               --  Split this node
            else
               Current.Used := Current.Used + 1;
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
               Current.Used := Current.Used + 1;
               Add_Frame (Current, Pc (Pos .. Pc'Last), Result);
               return;
            end if;
            Current.Used := Current.Used + 1;
            Current := Child;
            Lpos    := 2;
            Pos     := Pos + 1;
         end if;
      end loop;
      Current.Used := Current.Used + 1;
      if Lpos <= Current.Local_Depth then
         Split (Current, Lpos - 1);
      end if;
      Result := Current;
   end Insert;

   --  ------------------------------
   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
   --  ------------------------------
   function Find (Frame : in Frame_Type;
                  Pc    : in MAT.Types.Target_Addr) return Frame_Type is
      Child : Frame_Type := Frame.Children;
   begin
      while Child /= null loop
         if Child.Local_Depth >= 1 and then Child.Calls (1) = Pc then
            return Child;
         end if;
         Child := Child.Next;
      end loop;
      raise Not_Found;
   end Find;

   --  ------------------------------
   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
   --  ------------------------------
   function Find (Frame : in Frame_Type;
                  Pc    : in Frame_Table) return Frame_Type is
      Child : Frame_Type := Frame;
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

   --  ------------------------------
   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
   --  ------------------------------
   procedure Find (Frame   : in Frame_Type;
                   Pc      : in Frame_Table;
                   Result  : out Frame_Type;
                   Last_Pc : out Natural) is
      Current : Frame_Type := Frame;
      Pos     : Positive  := Pc'First;
      Lpos    : Positive;
   begin
      Main_Search :
      while Pos <= Pc'Last loop
         declare
            Addr  : constant MAT.Types.Target_Addr := Pc (Pos);
            Child : Frame_Type  := Current.Children;
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

   --  ------------------------------
   --  Check whether the frame contains a call to the function described by the address range.
   --  ------------------------------
   function In_Function (Frame : in Frame_Type;
                         From  : in MAT.Types.Target_Addr;
                         To    : in MAT.Types.Target_Addr) return Boolean is
      Current : Frame_Type := Frame;
   begin
      while Current /= null loop
         for I in 1 .. Current.Local_Depth loop
            if Current.Calls (I) >= From and Current.Calls (I) <= To then
               return True;
            end if;
         end loop;
         Current := Current.Parent;
      end loop;
      return False;
   end In_Function;

   --  ------------------------------
   --  Check whether the inner most frame contains a call to the function described by
   --  the address range.  This function looks only at the inner most frame and not the
   --  whole stack frame.
   --  ------------------------------
   function By_Function (Frame : in Frame_Type;
                         From  : in MAT.Types.Target_Addr;
                         To    : in MAT.Types.Target_Addr) return Boolean is
      Pc : MAT.Types.Target_Addr;
   begin
      if Frame /= null then
         Pc := Frame.Calls (Frame.Local_Depth);
         if Pc >= From and Pc <= To then
            return True;
         end if;
      end if;
      return False;
   end By_Function;


end MAT.Frames;
