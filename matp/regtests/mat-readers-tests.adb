-----------------------------------------------------------------------
--  mat-readers-tests -- Unit tests for MAT readers
--  Copyright (C) 2014, 2019 Stephane Carrez
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
package body MAT.Readers.Tests is

   package Caller is new Util.Test_Caller (Test, "Files");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test MAT.Readers.Read_File",
                       Test_Read_File'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test reading a file into a string
   --  Reads this ada source file and checks we have read it correctly
   --  ------------------------------
   procedure Test_Read_File (T : in out Test) is
      pragma Unreferenced (T);

      Path   : constant String := Util.Tests.Get_Test_Path ("regtests/files/file-v1.dat");
      Reader : MAT.Readers.Streams.Files.File_Reader_Type;
   begin
      Reader.Open (Path);
      Reader.Read_All;
   end Test_Read_File;

end MAT.Readers.Tests;
