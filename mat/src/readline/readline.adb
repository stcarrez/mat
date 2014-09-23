-----------------------------------------------------------------------
--  readline -- A simple readline binding
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
with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.IO_Exceptions;
package body Readline is

   pragma Linker_Options ("-lreadline");

   function Readline (Prompt : in Interfaces.C.Strings.chars_ptr)
                      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Readline, "readline");

   procedure Add_History (Line : in Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Add_History, "add_history");

   --  ------------------------------
   --  Print the prompt and a read a line from the terminal.
   --  Raise the Ada.IO_Exceptions.End_Error when the EOF is reached.
   --  ------------------------------
   function Get_Line (Prompt : in String) return String is
      use type Interfaces.C.Strings.chars_ptr;

      P : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Prompt);
      R : Interfaces.C.Strings.chars_ptr;
   begin
      R := Readline (P);
      Interfaces.C.Strings.Free (P);
      if R = Interfaces.C.Strings.null_ptr then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      declare
         Result : constant String := Interfaces.C.Strings.Value (R);
      begin
         if Result'Length > 0 then
            Add_History (R);
         end if;
         Interfaces.C.Strings.Free (R);
         return Result;
      end;
   end Get_Line;

end Readline;
