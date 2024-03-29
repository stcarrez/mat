-----------------------------------------------------------------------
--  mat-memory-tests -- Unit tests for MAT memory
--  Copyright (C) 2014, 2015, 2019, 2021 Stephane Carrez
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

with Util.Test_Caller;

with MAT.Expressions;
with MAT.Frames.Targets;
with MAT.Memory.Targets;
package body MAT.Memory.Tests is

   package Caller is new Util.Test_Caller (Test, "Memory");

   --  Builtin and well known definition of test frames.
   Frame_1_0 : constant MAT.Frames.Frame_Table (1 .. 10) :=
     (1_0, 1_2, 1_3, 1_4, 1_5, 1_6, 1_7, 1_8, 1_9, 1_10);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test MAT.Memory.Probe_Malloc",
                       Test_Probe_Malloc'Access);
      Caller.Add_Test (Suite, "Test MAT.Memory.Probe_Free",
                       Test_Probe_Free'Access);
   end Add_Tests;

   --  ------------------------------
   --  Basic consistency checks when creating the test tree
   --  ------------------------------
   procedure Test_Probe_Malloc (T : in out Test) is
      M    : MAT.Memory.Targets.Target_Memory;
      S    : Allocation;
      R    : Allocation_Map;
      F    : MAT.Frames.Targets.Target_Frames;
   begin
      S.Size :=  4;
      F.Insert (Frame_1_0, S.Frame);

      --  Create memory slots:
      --   [10 .. 14] [20 .. 24] [30 ..34] .. [100 .. 104]
      for I in 1 .. 10 loop
         M.Probe_Malloc (MAT.Types.Target_Addr (10 * I), S);
      end loop;

      --  Search for a memory region that does not overlap a memory slot.
      M.Find (15, 19, MAT.Expressions.EMPTY, R);
      Util.Tests.Assert_Equals (T, 0, Integer (R.Length),
                                "Find must return 0 slots in range [15 .. 19]");
      M.Find (1, 9, MAT.Expressions.EMPTY, R);
      Util.Tests.Assert_Equals (T, 0, Integer (R.Length),
                                "Find must return 0 slots in range [1 .. 9]");
      M.Find (105, 1000, MAT.Expressions.EMPTY, R);
      Util.Tests.Assert_Equals (T, 0, Integer (R.Length),
                                "Find must return 0 slots in range [105 .. 1000]");

      --  Search with an overlap.
      M.Find (1, 1000, MAT.Expressions.EMPTY, R);
      Util.Tests.Assert_Equals (T, 10, Integer (R.Length),
                                "Find must return 10 slots in range [1 .. 1000]");

      R.Clear;
      M.Find (1, 19, MAT.Expressions.EMPTY, R);
      Util.Tests.Assert_Equals (T, 1, Integer (R.Length),
                                "Find must return 1 slot in range [1 .. 19]");

      R.Clear;
      M.Find (13, 19, MAT.Expressions.EMPTY, R);
      Util.Tests.Assert_Equals (T, 1, Integer (R.Length),
                                "Find must return 1 slot in range [13 .. 19]");

      R.Clear;
      M.Find (100, 1000, MAT.Expressions.EMPTY, R);
      Util.Tests.Assert_Equals (T, 1, Integer (R.Length),
                                "Find must return 1 slot in range [100 .. 1000]");

      R.Clear;
      M.Find (101, 1000, MAT.Expressions.EMPTY, R);
      Util.Tests.Assert_Equals (T, 1, Integer (R.Length),
                                "Find must return 1 slot in range [101 .. 1000]");

   end Test_Probe_Malloc;

   --  ------------------------------
   --  Test Probe_Free with update of memory slots.
   --  ------------------------------
   procedure Test_Probe_Free (T : in out Test) is
      M    : MAT.Memory.Targets.Target_Memory;
      S    : Allocation;
      R    : Allocation_Map;
      Size : MAT.Types.Target_Size;
      Id   : MAT.Events.Event_Id_Type with Unreferenced;
      F    : MAT.Frames.Targets.Target_Frames;
   begin
      S.Size :=  4;
      S.Event := 1;
      F.Insert (Frame_1_0, S.Frame);

      --  Malloc followed by a free.
      M.Probe_Malloc (10, S);
      Id := 12;
      M.Probe_Free (10, S, Size, Id);

      M.Find (1, 1000, MAT.Expressions.EMPTY, R);
      Util.Tests.Assert_Equals (T, 0, Integer (R.Length),
                                "Find must return 0 slot after a free");

      --  Free the same slot a second time (free error).
      M.Probe_Free (10, S, Size, Id);

      --  Malloc followed by a free.
      M.Probe_Malloc (10, S);
      M.Probe_Malloc (20, S);
      M.Probe_Malloc (30, S);
      M.Probe_Free (20, S, Size, Id);

      M.Find (1, 1000, MAT.Expressions.EMPTY, R);
      Util.Tests.Assert_Equals (T, 2, Integer (R.Length),
                                "Find must return 2 slots after a malloc/free sequence");
   end Test_Probe_Free;

end MAT.Memory.Tests;
