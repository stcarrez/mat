-----------------------------------------------------------------------
--  mat-readers-tests -- Unit tests for MAT readers
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

with Ada.Text_IO;
with Ada.Directories;
with Util.Test_Caller;

with MAT.Frames.Print;
with MAT.Readers.Files;
package body MAT.Frames.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Files");

   --  Builtin and well known definition of test frames.
   Frame_1_0 : constant Frame_Table (1 .. 10) :=
     (1_0, 1_2, 1_3, 1_4, 1_5, 1_6, 1_7, 1_8, 1_9, 1_10);

   Frame_1_1 : constant Frame_Table (1 .. 15) :=
     (1_0, 1_2, 1_3, 1_4, 1_5, 1_6, 1_7, 1_8, 1_9, 1_10,
      1_11, 1_12, 1_13, 1_14, 1_15);

   Frame_1_2 : constant Frame_Table (1 .. 16) :=
     (1_0, 1_2, 1_3, 1_4, 1_5, 1_6, 1_7, 1_8, 1_9, 1_10,
      1_11, 1_12, 1_13, 1_14, 1_20, 1_21);

   Frame_1_3 : constant Frame_Table (1 .. 15) :=
     (1_0, 1_2, 1_3, 1_4, 1_5, 1_6, 1_7, 1_8, 1_9, 1_10,
      1_11, 1_12, 1_13, 1_14, 1_30);

   Frame_2_0 : constant Frame_Table (1 .. 10) :=
     (2_0, 2_2, 2_3, 2_4, 2_5, 2_6, 2_7, 2_8, 2_9, 2_10);

   Frame_2_1 : constant Frame_Table (1 .. 10) :=
     (2_0, 2_1, 2_1, 2_1, 2_1, 2_1, 2_1, 2_1, 2_1, 2_10);

   Frame_2_2 : constant Frame_Table (1 .. 10) :=
     (2_0, 2_1, 2_2, 2_1, 2_1, 2_1, 2_1, 2_1, 2_1, 2_1);

   Frame_2_3 : constant Frame_Table (1 .. 10) :=
     (2_0, 2_1, 2_2, 2_1, 2_1, 2_3, 2_1, 2_1, 2_1, 2_1);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test MAT.Frames.Insert",
                       Test_Simple_Frames'Access);
      Caller.Add_Test (Suite, "Test MAT.Frames.Find",
                       Test_Find_Frames'Access);
      Caller.Add_Test (Suite, "Test MAT.Frames.Backtrace",
                       Test_Complex_Frames'Access);
      Caller.Add_Test (Suite, "Test MAT.Frames.Release",
                       Test_Release_Frames'Access);
   end Add_Tests;

   --  ------------------------------
   --  Create a tree with the well known test frames.
   --  ------------------------------
   function Create_Test_Frames return Frame_Type is
      Root : Frame_Type := Create_Root;
      F    : Frame_Type;
   begin
      Insert (Root, Frame_1_0, F);
      Insert (Root, Frame_1_1, F);
      Insert (Root, Frame_1_2, F);
      Insert (Root, Frame_1_3, F);
      Insert (Root, Frame_2_0, F);
      Insert (Root, Frame_2_1, F);
      Insert (Root, Frame_2_2, F);
      Insert (Root, Frame_2_3, F);
      return Root;
   end Create_Test_Frames;

   --  ------------------------------
   --  Basic consistency checks when creating the test tree
   --  ------------------------------
   procedure Test_Simple_Frames (T : in out Test) is
      Root : Frame_Type := Create_Root;
      F    : Frame_Type;
   begin
      --  Consistency check on empty tree.
      Util.Tests.Assert_Equals (T, 0, Count_Children (Root),
                                "Empty frame: Count_Children must return 0");
      Util.Tests.Assert_Equals (T, 0, Current_Depth (Root),
                                "Empty frame: Current_Depth must return 0");

      --  Insert first frame and verify consistency.
      Insert (Root, Frame_1_0, F);
      Util.Tests.Assert_Equals (T, 1, Count_Children (Root),
                                "Simple frame: Count_Children must return 1");
      Util.Tests.Assert_Equals (T, 10, Current_Depth (F),
                                "Simple frame: Current_Depth must return 10");
--        Expect (Msg    => "Frames.Count_Children",
--                Val    => 1,
--                Result => Count_Children (Root));
--        Expect (Msg    => "Frames.Count_Children(recursive)",
--                Val    => 1,
--                Result => Count_Children (Root, True));
--        Expect (Msg    => "Frames.Current_Depth",
--                Val    => 10,
--                Result => Current_Depth (F));

      Insert (Root, Frame_1_1, F);
      Insert (Root, Frame_1_2, F);
      Insert (Root, Frame_1_3, F);
      if Verbose then
         MAT.Frames.Print (Ada.Text_IO.Standard_Output, Root);
      end if;
      Util.Tests.Assert_Equals (T, 1, Count_Children (Root),
                                "Simple frame: Count_Children must return 1");
      Util.Tests.Assert_Equals (T, 3, Count_Children (Root, True),
                                "Simple frame: Count_Children (recursive) must return 3");
      Util.Tests.Assert_Equals (T, 15, Current_Depth (F),
                                "Simple frame: Current_Depth must return 15");

      Insert (Root, Frame_2_0, F);
      Insert (Root, Frame_2_1, F);
      Insert (Root, Frame_2_2, F);
      Insert (Root, Frame_2_3, F);
      if Verbose then
         MAT.Frames.Print (Ada.Text_IO.Standard_Output, Root);
      end if;
      Util.Tests.Assert_Equals (T, 2, Count_Children (Root),
                                "Simple frame: Count_Children must return 2");
      Util.Tests.Assert_Equals (T, 7, Count_Children (Root, True),
                                "Simple frame: Count_Children (recursive) must return 7");
      Util.Tests.Assert_Equals (T, 10, Current_Depth (F),
                                "Simple frame: Current_Depth must return 10");

      Destroy (Root);
   end Test_Simple_Frames;

   --  ------------------------------
   --  Test searching in the frame.
   --  ------------------------------
   procedure Test_Find_Frames (T : in out Test) is
      Root    : Frame_Type := Create_Test_Frames;
      Result  : Frame_Type;
      Last_Pc : Natural;
   begin
      --  Find exact frame.
      Find (Root, Frame_2_3, Result, Last_Pc);
      T.Assert (Result /= Root, "Frames.Find must return a valid frame");
      T.Assert (Last_Pc = 0, "Frames.Find must return a 0 Last_Pc");

      declare
         Pc : Frame_Table (1 .. 8) := Frame_2_3 (1 .. 8);
      begin
         Find (Root, Pc, Result, Last_Pc);

         T.Assert (Result /= Root, "Frames.Find must return a valid frame");
--           Expect (Msg    => "Frames.Find (Last_Pc param)",
--                   Val    => 0,
--                   Result => Last_Pc);
      end;

      Destroy (Root);
   end Test_Find_Frames;

   --  ------------------------------
   --  Create a complex frame tree and run tests on it.
   --  ------------------------------
   procedure Test_Complex_Frames (T : in out Test) is

      Pc   : Frame_Table (1 .. 8);
      Root : Frame_Type := Create_Root;

      procedure Create_Frame (Depth : in Natural);

      procedure Create_Frame (Depth : in Natural) is
         use type MAT.Types.Target_Addr;
         use type MAT.Events.Frame_Table;

         F : Frame_Type;
      begin
         Pc (Depth) := MAT.Types.Target_Addr (Depth);
         if Depth < Pc'Last then
            Create_Frame (Depth + 1);
         end if;

         Insert (Root, Pc (1 .. Depth), F);
         Pc (Depth) := MAT.Types.Target_Addr (Depth) + 1000;
         Insert (Root, Pc (1 .. Depth), F);
         if Depth < Pc'Last then
            Create_Frame (Depth + 1);
         end if;

         declare
            Read_Pc : Frame_Table := Backtrace (F);
         begin
            T.Assert (Read_Pc = Pc (1 .. Depth), "Frames.backtrace (same as inserted)");
            if Verbose then
               for I in Read_Pc'Range loop
                  Ada.Text_IO.Put (" " & MAT.Types.Target_Addr'Image (Read_Pc (I)));
               end loop;
               Ada.Text_IO.New_Line;
            end if;
         end;
      end Create_Frame;

   begin
      Create_Frame (1);
      if Verbose then
         MAT.Frames.Print (Ada.Text_IO.Standard_Output, Root);
      end if;
      Destroy (Root);
   end Test_Complex_Frames;

   --  ------------------------------
   --  Test allocating and releasing frames.
   --  ------------------------------
   procedure Test_Release_Frames (T : in out Test) is
      Root : Frame_Type := Create_Root;
      F1   : Frame_Type;
      F2   : Frame_Type;
      F3   : Frame_Type;
   begin
      Insert (Root, Frame_1_1, F1);
      T.Assert (Root.Used > 0, "Insert must increment the root used count");
      MAT.Frames.Print (Ada.Text_IO.Standard_Output, Root);
      Insert (Root, Frame_1_2, F2);
      MAT.Frames.Print (Ada.Text_IO.Standard_Output, Root);
      Insert (Root, Frame_1_3, F3);
      MAT.Frames.Print (Ada.Text_IO.Standard_Output, Root);
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output, "Release frame F1");
      Release (F1);
      MAT.Frames.Print (Ada.Text_IO.Standard_Output, Root);
      T.Assert (F2.Used > 0, "Release must not change other frames");
      T.Assert (Root.Used > 0, "Release must not change root frame");
      Destroy (Root);
   end Test_Release_Frames;

end MAT.Frames.Tests;
