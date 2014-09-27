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
with MAT.Types; use MAT.Types;
with MAT.Events;
package MAT.Frames is

   Not_Found : exception;

   type Frame is limited private;
   type Frame_Ptr is access all Frame;

   subtype PC_Table is MAT.Events.Frame_Table;

   function Parent (F : in Frame_Ptr) return Frame_Ptr;
   --  Return the parent frame.

   function Backtrace (F : in Frame_Ptr) return PC_Table;
   --  Returns the backtrace of the current frame (up to the root).

   function Calls (F : in Frame_Ptr) return PC_Table;
   --  Returns all the direct calls made by the current frame.

   function Count_Children (F : in Frame_Ptr;
                            Recursive : in Boolean := False) return Natural;
   --  Returns the number of children in the frame.
   --  When recursive is true, compute in the sub-tree.

   function Current_Depth (F : in Frame_Ptr) return Natural;
   --  Returns the current stack depth (# of calls from the root
   --  to reach the frame).

   function Create_Root return Frame_Ptr;
   --  Create a root for stack frame representation.

   procedure Destroy (Tree : in out Frame_Ptr);
   --  Destroy the frame tree recursively.

   procedure Release (F : in Frame_Ptr);
   --  Release the frame when its reference is no longer necessary.

   procedure Insert (F : in Frame_Ptr;
                     Pc : in Pc_Table;
                     Result : out Frame_Ptr);

   function Find (F : in Frame_Ptr;
                  Pc : in Target_Addr) return Frame_Ptr;
   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.

   function Find (F : in Frame_Ptr;
                  Pc : in Pc_Table) return Frame_Ptr;
   --

   --  Find the child frame which has the given PC address.
   --  Returns that frame pointer or raises the Not_Found exception.
   procedure Find (F       : in Frame_Ptr;
                   Pc      : in PC_Table;
                   Result  : out Frame_Ptr;
                   Last_Pc : out Natural);
private

   Frame_Group_Size : constant Natural := 4;

   subtype Local_Depth_Type is Natural range 0 .. Frame_Group_Size;

   subtype Frame_Table is Pc_Table (1 .. Local_Depth_Type'Last);

   type Frame is record
      Parent   : Frame_Ptr := null;
      Next     : Frame_Ptr := null;
      Children : Frame_Ptr := null;
      Used     : Natural   := 0;
      Depth    : Natural   := 0;
      Calls    : Frame_Table;
      Local_Depth : Local_Depth_Type := 0;
   end record;

end MAT.Frames;
