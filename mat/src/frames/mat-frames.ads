-----------------------------------------------------------------------
--  mat-frames - Representation of stack frames
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
with MAT.Events;
package MAT.Frames is

   Not_Found : exception;

   type Frame_Type is private;

   subtype Frame_Table is MAT.Events.Frame_Table;

   --  Return the parent frame.
   function Parent (Frame : in Frame_Type) return Frame_Type;

   --  Returns the backtrace of the current frame (up to the root).
   function Backtrace (Frame : in Frame_Type) return Frame_Table;

   --  Returns all the direct calls made by the current frame.
   function Calls (Frame : in Frame_Type) return Frame_Table;

   --  Returns the number of children in the frame.
   --  When recursive is true, compute in the sub-tree.
   function Count_Children (Frame     : in Frame_Type;
                            Recursive : in Boolean := False) return Natural;

   --  Returns the current stack depth (# of calls from the root
   --  to reach the frame).
   function Current_Depth (Frame : in Frame_Type) return Natural;

   --  Create a root for stack frame representation.
   function Create_Root return Frame_Type;

   --  Destroy the frame tree recursively.
   procedure Destroy (Frame : in out Frame_Type);

   --  Release the frame when its reference is no longer necessary.
   procedure Release (Frame : in Frame_Type);

   --  Insert in the frame tree the new stack frame represented by <tt>Pc</tt>.
   --  If the frame is already known, the frame reference counter is incremented.
   --  The frame represented by <tt>Pc</tt> is returned in <tt>Result</tt>.
   procedure Insert (Frame  : in Frame_Type;
                     Pc     : in Frame_Table;
                     Result : out Frame_Type);

   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
   function Find (Frame : in Frame_Type;
                  Pc    : in MAT.Types.Target_Addr) return Frame_Type;

   function Find (Frame : in Frame_Type;
                  Pc    : in Frame_Table) return Frame_Type;

   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
   procedure Find (Frame   : in Frame_Type;
                   Pc      : in Frame_Table;
                   Result  : out Frame_Type;
                   Last_Pc : out Natural);
private

   Frame_Group_Size : constant Natural := 4;

   subtype Local_Depth_Type is Natural range 0 .. Frame_Group_Size;

   subtype Mini_Frame_Table is Frame_Table (1 .. Local_Depth_Type'Last);

   type Frame;
   type Frame_Type is access all Frame;

   type Frame is record
      Parent   : Frame_Type := null;
      Next     : Frame_Type := null;
      Children : Frame_Type := null;
      Used     : Natural   := 0;
      Depth    : Natural   := 0;
      Calls    : Mini_Frame_Table;
      Local_Depth : Local_Depth_Type := 0;
   end record;

end MAT.Frames;
