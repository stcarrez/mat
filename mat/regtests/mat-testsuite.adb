-----------------------------------------------------------------------
--  mat-testsuite - MAT Testsuite
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
with MAT.Readers.Tests;
with MAT.Targets.Tests;
with MAT.Frames.Tests;
with MAT.Memory.Tests;
package body MAT.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Result : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      MAT.Frames.Tests.Add_Tests (Result);
      MAT.Memory.Tests.Add_Tests (Result);
      MAT.Readers.Tests.Add_Tests (Result);
      MAT.Targets.Tests.Add_Tests (Result);
      return Result;
   end Suite;

end MAT.Testsuite;
