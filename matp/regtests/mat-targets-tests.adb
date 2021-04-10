-----------------------------------------------------------------------
--  mat-readers-tests -- Unit tests for MAT readers
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

with MAT.Readers.Streams.Files;
package body MAT.Targets.Tests is

   package Caller is new Util.Test_Caller (Test, "Files");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test MAT.Targets.Read_File",
                       Test_Read_File'Access);
      Caller.Add_Test (Suite, "Test MAT.Types.Tick_Value",
                       Test_Conversions'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test reading a file into a string
   --  Reads this ada source file and checks we have read it correctly
   --  ------------------------------
   procedure Test_Read_File (T : in out Test) is
      pragma Unreferenced (T);

      Path   : constant String := Util.Tests.Get_Path ("regtests/files/file-v1.dat");
      Target : MAT.Targets.Target_Type;
      Reader : MAT.Readers.Streams.Files.File_Reader_Type;
   begin
      Target.Initialize (Reader);
      Reader.Open (Path);
      Reader.Read_All;
   end Test_Read_File;

   --  ------------------------------
   --  Test various type conversions.
   --  ------------------------------
   procedure Test_Conversions (T : in out Test) is
      use MAT.Types;
      Time : MAT.Types.Target_Tick_Ref;
   begin
      Time := MAT.Types.Tick_Value ("1.1");
      Util.Tests.Assert_Equals (T, 1, Natural (Time / 1_000000), "Invalid Tick_Value conversion");
      Util.Tests.Assert_Equals (T, 100_000, Natural (Time mod 1_000000),
                                "Invalid Tick_Value conversion");

      Time := MAT.Types.Tick_Value ("12.001234");
      Util.Tests.Assert_Equals (T, 12, Natural (Time / 1_000000), "Invalid Tick_Value conversion");
      Util.Tests.Assert_Equals (T, 1_234, Natural (Time mod 1_000000),
                                "Invalid Tick_Value conversion");
   end Test_Conversions;

end MAT.Targets.Tests;
