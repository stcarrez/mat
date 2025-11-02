-----------------------------------------------------------------------
--  mat-expressions-tests -- Unit tests for MAT expressions
--  Copyright (C) 2014, 2015, 2019 Stephane Carrez
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
with Util.Assertions;

package body MAT.Expressions.Tests is

   package Caller is new Util.Test_Caller (Test, "Expressions");

   procedure Assert_Equals_Kind is
     new Util.Assertions.Assert_Equals_T (Value_Type => Kind_Type);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test MAT.Expressions.Parse",
                       Test_Parse_Expression'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test parsing simple expressions
   --  ------------------------------
   procedure Test_Parse_Expression (T : in out Test) is
      Result : MAT.Expressions.Expression_Type;
   begin
      Result := MAT.Expressions.Parse ("by foo", null);
      T.Assert (Result.Node /= null, "Parse 'by foo' must return a expression");
      Assert_Equals_Kind (T, N_IN_FUNC, Result.Node.Kind, "Invalid node kind");

      Result := MAT.Expressions.Parse ("by direct foo", null);
      T.Assert (Result.Node /= null, "Parse 'by direct foo' must return a expression");
      Assert_Equals_Kind (T, N_IN_FUNC_DIRECT, Result.Node.Kind, "Invalid node kind");

      Result := MAT.Expressions.Parse ("after 10.2", null);
      T.Assert (Result.Node /= null, "Parse 'after 10.2' must return a expression");
      Assert_Equals_Kind (T, N_RANGE_TIME, Result.Node.Kind, "Invalid node kind");

      Result := MAT.Expressions.Parse ("before 3.5", null);
      T.Assert (Result.Node /= null, "Parse 'before 3.5' must return a expression");
      Assert_Equals_Kind (T, N_RANGE_TIME, Result.Node.Kind, "Invalid node kind");

      Result := MAT.Expressions.Parse ("from 2.2 to 3.3", null);
      T.Assert (Result.Node /= null, "Parse 'from 2.2 to 3.3' must return a expression");
      Assert_Equals_Kind (T, N_RANGE_TIME, Result.Node.Kind, "Invalid node kind");

      Result := MAT.Expressions.Parse ("size = 10", null);
      T.Assert (Result.Node /= null, "Parse 'size = 10' must return a expression");
      Assert_Equals_Kind (T, N_RANGE_SIZE, Result.Node.Kind, "Invalid node kind");

      Result := MAT.Expressions.Parse ("size > 10", null);
      T.Assert (Result.Node /= null, "Parse 'size > 10' must return a expression");
      Assert_Equals_Kind (T, N_RANGE_SIZE, Result.Node.Kind, "Invalid node kind");

      Result := MAT.Expressions.Parse ("size < 10", null);
      T.Assert (Result.Node /= null, "Parse 'size < 10' must return a expression");
      Assert_Equals_Kind (T, N_RANGE_SIZE, Result.Node.Kind, "Invalid node kind");

      Result := MAT.Expressions.Parse ("event = 23", null);
      T.Assert (Result.Node /= null, "Parse 'event = 23' must return a expression");
      Assert_Equals_Kind (T, N_EVENT, Result.Node.Kind, "Invalid node kind");

      Result := MAT.Expressions.Parse ("event = 1..100", null);
      T.Assert (Result.Node /= null, "Parse 'event = 1..100' must return a expression");
      Assert_Equals_Kind (T, N_EVENT, Result.Node.Kind, "Invalid node kind");

   end Test_Parse_Expression;

end MAT.Expressions.Tests;
