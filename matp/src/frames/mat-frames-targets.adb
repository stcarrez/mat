-----------------------------------------------------------------------
--  mat-frames-targets - Representation of stack frames
--  Copyright (C) 2015 Stephane Carrez
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

package body MAT.Frames.Targets is

   --  ------------------------------
   --  Insert in the frame tree the new stack frame represented by <tt>Pc</tt>.
   --  If the frame is already known, the frame reference counter is incremented.
   --  The frame represented by <tt>Pc</tt> is returned in <tt>Result</tt>.
   --  ------------------------------
   procedure Insert (Frame  : in out Target_Frames;
                     Pc     : in Frame_Table;
                     Result : out Frame_Type) is
   begin
      Frame.Frames.Insert (Pc, Result);
   end Insert;

   --  ------------------------------
   --  Get the number of different stack frames which have been registered.
   --  ------------------------------
   function Get_Frame_Count (Frame : in Target_Frames) return Natural is
   begin
      return Frame.Frames.Get_Frame_Count;
   end Get_Frame_Count;

   protected body Frames_Type is

      --  ------------------------------
      --  Insert in the frame tree the new stack frame represented by <tt>Pc</tt>.
      --  If the frame is already known, the frame reference counter is incremented.
      --  The frame represented by <tt>Pc</tt> is returned in <tt>Result</tt>.
      --  ------------------------------
      procedure Insert (Pc     : in Frame_Table;
                        Result : out Frame_Type) is
      begin
         MAT.Frames.Insert (Root, Pc, Result);
      end Insert;

      --  ------------------------------
      --  Clear and destroy all the frame instances.
      --  ------------------------------
      procedure Clear is
      begin
         Destroy (Root);
      end Clear;

   end Frames_Type;

   --  ------------------------------
   --  Release all the stack frames.
   --  ------------------------------
   overriding
   procedure Finalize (Frames : in out Target_Frames) is
   begin
      Frames.Frames.Clear;
   end Finalize;

end MAT.Frames.Targets;
