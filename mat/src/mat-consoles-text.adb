-----------------------------------------------------------------------
--  mat-consoles-text - Text console interface
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

package body MAT.Consoles.Text is

   --  ------------------------------
   --  Print the field value for the given field.
   --  ------------------------------
   overriding
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in String) is
   begin
      Ada.Text_IO.Put (Value);
   end Print_Field;

   --  ------------------------------
   --  Print the title for the given field.
   --  ------------------------------
   overriding
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String) is
   begin
      Ada.Text_IO.Put (Title);
   end Print_Title;

   --  ------------------------------
   --  Start a new title in a report.
   --  ------------------------------
   overriding
   procedure Start_Title (Console : in out Console_Type) is
   begin
      null;
   end Start_Title;

   --  ------------------------------
   --  Start a new row in a report.
   --  ------------------------------
   overriding
   procedure Start_Row (Console : in out Console_Type) is
   begin
      null;
   end Start_Row;

   --  ------------------------------
   --  Finish a new row in a report.
   --  ------------------------------
   overriding
   procedure End_Row (Console : in out Console_Type) is
   begin
      Ada.Text_IO.New_Line;
   end End_Row;

end MAT.Consoles.Text;
