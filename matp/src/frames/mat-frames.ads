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
with MAT.Types;
package MAT.Frames is

   Not_Found : exception;

   type Frame_Type is private;
   type Frame_Table is array (Natural range <>) of MAT.Types.Target_Addr;

   --  Return the parent frame.
   function Parent (Frame : in Frame_Type) return Frame_Type;

   --  Returns the backtrace of the current frame (up to the root).
   --  When <tt>Max_Level</tt> is positive, limit the number of PC frames to the value.
   function Backtrace (Frame     : in Frame_Type;
                       Max_Level : in Natural := 0) return Frame_Table;

   --  Returns all the direct calls made by the current frame.
   function Calls (Frame : in Frame_Type) return Frame_Table;

   --  Returns the number of children in the frame.
   --  When recursive is true, compute in the sub-tree.
   function Count_Children (Frame     : in Frame_Type;
                            Recursive : in Boolean := False) return Natural;

   --  Returns the current stack depth (# of calls from the root
   --  to reach the frame).
   function Current_Depth (Frame : in Frame_Type) return Natural;

   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
   function Find (Frame : in Frame_Type;
                  Pc    : in MAT.Types.Target_Addr) return Frame_Type;

   --  Check whether the frame contains a call to the function described by the address range.
   function In_Function (Frame : in Frame_Type;
                         From  : in MAT.Types.Target_Addr;
                         To    : in MAT.Types.Target_Addr) return Boolean;

   --  Check whether the inner most frame contains a call to the function described by
   --  the address range.  This function looks only at the inner most frame and not the
   --  whole stack frame.
   function By_Function (Frame : in Frame_Type;
                         From  : in MAT.Types.Target_Addr;
                         To    : in MAT.Types.Target_Addr) return Boolean;
   procedure Verify_Frames;

private

   type Frame;
   type Frame_Type is access all Frame;

   --  The frame information is readonly and we can safely use the By_Function, In_Function
   --  and Backtrace without protection.  Insertion and creation of stack frame must be
   --  protected through a protected type managed by Target_Frames.  All the frame instances
   --  are released when the Target_Frames protected type is released.
   type Frame (Parent : Frame_Type;
               Depth  : Natural;
               Pc     : MAT.Types.Target_Addr) is limited record
      Next     : Frame_Type := null;
      Children : Frame_Type := null;
      Used     : Natural   := 0;
   end record;

   --  Create a root for stack frame representation.
   function Create_Root return Frame_Type;

   --  Insert in the frame tree the new stack frame represented by <tt>Pc</tt>.
   --  If the frame is already known, the frame reference counter is incremented.
   --  The frame represented by <tt>Pc</tt> is returned in <tt>Result</tt>.
   procedure Insert (Frame  : in Frame_Type;
                     Pc     : in Frame_Table;
                     Result : out Frame_Type);

   --  Destroy the frame tree recursively.
   procedure Destroy (Frame : in out Frame_Type);

end MAT.Frames;
