-----------------------------------------------------------------------
--  mat-types -- Global types
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
with Ada.IO_Exceptions;
with Readline;
with MAT.Commands;
with MAT.Targets;
procedure Matp is

   procedure Interactive_Loop is
      Target : MAT.Targets.Target_Type;
   begin
      loop
         declare
            Line : constant String := Readline.Get_Line ("matp>");
         begin
            MAT.Commands.Execute (Target, Line);

         exception
            when MAT.Commands.Stop_Interp =>
               return;
         end;
      end loop;

   exception

      when Ada.IO_Exceptions.End_Error =>
         return;

   end Interactive_Loop;

begin
   Interactive_Loop;
end Matp;
