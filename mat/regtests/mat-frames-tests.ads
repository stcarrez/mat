-----------------------------------------------------------------------
--  mat-frames-tests -- Unit tests for MAT frames
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

with Util.Tests;

package MAT.Frames.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Basic consistency checks when creating the test tree
   procedure Test_Simple_Frames (T : in out Test);

   --  Test searching in the frame.
   procedure Test_Find_Frames (T : in out Test);

   --  Create a complex frame tree and run tests on it.
   procedure Test_Complex_Frames (T : in out Test);

   --  Test allocating and releasing frames.
   procedure Test_Release_Frames (T : in out Test);

end MAT.Frames.Tests;
