-----------------------------------------------------------------------
--  mat-frames - Representation of stack frames
--  Copyright (C) 2014, 2015, 2021 Stephane Carrez
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

package body MAT.Frames is

   use type MAT.Types.Target_Addr;

   procedure Free is
     new Ada.Unchecked_Deallocation (Frame, Frame_Type);

   procedure Add_Frame (F      : in Frame_Type;
                        Pc     : in Frame_Table;
                        Result : out Frame_Type);

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
         Pc      : Frame_Table (1 .. Length);
      begin
         while Current /= null and Pos /= 0 loop
            Pc (Pos) := Current.Pc;
            Pos := Pos - 1;
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
               Pc (Pos) := Child.Pc;
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
      return new Frame '(Parent => null, Depth => 0, Pc => 0, others => <>);
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
         Frame.Children := F.Next;
         Destroy (F);
      end loop;
      Free (Frame);
   end Destroy;

   procedure Add_Frame (F      : in Frame_Type;
                        Pc     : in Frame_Table;
                        Result : out Frame_Type) is
      Child         : Frame_Type := F.Children;
      Parent        : Frame_Type := F;
      Pos           : Positive   := Pc'First;
      Current_Depth : Natural    := F.Depth;
   begin
      while Pos <= Pc'Last loop
         Current_Depth := Current_Depth + 1;
         Child := new Frame '(Parent      => Parent,
                              Next        => Child,
                              Children    => null,
                              Used        => 1,
                              Depth       => Current_Depth,
                              Pc          => Pc (Pos));
         Pos := Pos + 1;
         Parent.Children := Child;
         Parent := Child;
         Child := null;
      end loop;
      Result := Parent;
   end Add_Frame;

   --  ------------------------------
   --  Insert in the frame tree the new stack frame represented by <tt>Pc</tt>.
   --  If the frame is already known, the frame reference counter is incremented.
   --  The frame represented by <tt>Pc</tt> is returned in <tt>Result</tt>.
   --  ------------------------------
   procedure Insert (Frame  : in Frame_Type;
                     Pc     : in Frame_Table;
                     Result : out Frame_Type) is
      Parent  : Frame_Type := Frame;
      Current : Frame_Type := Frame.Children;
      Pos     : Positive  := Pc'First;
      Addr    : MAT.Types.Target_Addr;
   begin
      if Pc'Length = 0 then
         Result := Frame;
         Frame.Used := Frame.Used + 1;
         return;
      end if;
      if Current = null then
         Frame.Used := Frame.Used + 1;
         Add_Frame (Frame, Pc, Result);
         return;
      end if;
      Addr := Pc (Pos);
      loop
         if Addr = Current.Pc then
            Parent.Used := Parent.Used + 1;
            Pos  := Pos + 1;
            if Pos > Pc'Last then
               Current.Used := Current.Used + 1;
               Result := Current;
               return;
            end if;
            if Current.Children = null then
               Current.Used := Current.Used + 1;
               Add_Frame (Current, Pc (Pos .. Pc'Last), Result);
               return;
            end if;
            Parent := Current;
            Current := Current.Children;
            Addr := Pc (Pos);

         elsif Current.Next = null then
            Parent.Used := Parent.Used + 1;
            Add_Frame (Parent, Pc (Pos .. Pc'Last), Result);
            return;
         else
            Current := Current.Next;
         end if;
      end loop;
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
         if Child.Pc = Pc then
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
--     function Find (Frame : in Frame_Type;
--                    Pc    : in Frame_Table) return Frame_Type is
--        Child : Frame_Type := Frame;
--        Pos   : Positive  := Pc'First;
--     begin
--        while Pos <= Pc'Last loop
--           Child := Find (Child, Pc (Pos));
--           Pos  := Pos + 1;
--           --  All the PC of the child frame must match.
--           while Pos <= Pc'Last and Lpos <= Child.Local_Depth loop
--              if Child.Calls (Lpos) /= Pc (Pos) then
--                 raise Not_Found;
--              end if;
--              Lpos := Lpos + 1;
--              Pos  := Pos + 1;
--           end loop;
--        end loop;
--        return Child;
--     end Find;

   --  ------------------------------
   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
--     --  ------------------------------
--     procedure Find (Frame   : in Frame_Type;
--                     Pc      : in Frame_Table;
--                     Result  : out Frame_Type;
--                     Last_Pc : out Natural) is
--        Current : Frame_Type := Frame;
--        Pos     : Positive  := Pc'First;
--        Lpos    : Positive;
--     begin
--        Main_Search :
--        while Pos <= Pc'Last loop
--           declare
--              Addr  : constant MAT.Types.Target_Addr := Pc (Pos);
--              Child : Frame_Type  := Current.Children;
--           begin
--              --  Find the child which has the corresponding PC.
--              loop
--                 exit Main_Search when Child = null;
--                 exit when Child.Pc = Addr;
--                 Child := Child.Next;
--              end loop;
--
--              Current := Child;
--              Pos  := Pos + 1;
--              Lpos := 2;
--              --  All the PC of the child frame must match.
--              while Pos <= Pc'Last and Lpos <= Current.Local_Depth loop
--                 exit Main_Search when Current.Calls (Lpos) /= Pc (Pos);
--                 Lpos := Lpos + 1;
--                 Pos  := Pos + 1;
--              end loop;
--           end;
--        end loop Main_Search;
--        Result := Current;
--        if Pos > Pc'Last then
--           Last_Pc := 0;
--        else
--           Last_Pc := Pos;
--        end if;
--     end Find;

   --  ------------------------------
   --  Check whether the frame contains a call to the function described by the address range.
   --  ------------------------------
   function In_Function (Frame : in Frame_Type;
                         From  : in MAT.Types.Target_Addr;
                         To    : in MAT.Types.Target_Addr) return Boolean is
      Current : Frame_Type := Frame;
   begin
      while Current /= null loop
         if Current.Pc >= From and Current.Pc <= To then
            return True;
         end if;
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
   begin
      return Frame /= null and then Frame.Pc >= From and then Frame.Pc <= To;
   end By_Function;

end MAT.Frames;
