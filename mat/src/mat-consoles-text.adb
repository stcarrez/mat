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
   --  Report an error message.
   --  ------------------------------
   overriding
   procedure Error (Console : in out Console_Type;
                    Message : in String) is
      pragma Unreferenced (Console);
   begin
      Ada.Text_IO.Put_Line (Message);
   end Error;

   --  ------------------------------
   --  Report a notice message.
   --  ------------------------------
   overriding
   procedure Notice (Console : in out Console_Type;
                     Kind    : in Notice_Type;
                     Message : in String) is
      pragma Unreferenced (Console, Kind);
   begin
      Ada.Text_IO.Put_Line (Message);
   end Notice;

   --  ------------------------------
   --  Print the field value for the given field.
   --  ------------------------------
   overriding
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in String) is
      use type Ada.Text_IO.Count;

      Pos : Ada.Text_IO.Count := Ada.Text_IO.Count (Console.Cols (Field));
   begin
      if Pos > 1 then
         Pos := Pos + Ada.Text_IO.Count (Console.Sizes (Field));
         if Pos > Value'Length then
            Ada.Text_IO.Set_Col (Pos - Value'Length);
         else
            Ada.Text_IO.Set_Col (Ada.Text_IO.Count (Console.Cols (Field)));
            Ada.Text_IO.Put (Value (Value'Last - Console.Sizes (Field) .. Value'Last));
            return;
         end if;
      end if;
      Ada.Text_IO.Put (Value);
   end Print_Field;

   --  ------------------------------
   --  Print the title for the given field.
   --  ------------------------------
   overriding
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String) is
      use type Ada.Text_IO.Count;

      Pos : constant Ada.Text_IO.Count := Ada.Text_IO.Count (Console.Cols (Field));
   begin
      if Pos > 1 then
         Ada.Text_IO.Set_Col (Pos + Ada.Text_IO.Count (Console.Sizes (Field)) - Title'Length);
      end if;
      Ada.Text_IO.Put (Title);
   end Print_Title;

   --  ------------------------------
   --  Start a new title in a report.
   --  ------------------------------
   overriding
   procedure Start_Title (Console : in out Console_Type) is
   begin
      Console.Field_Count := 0;
      Console.Sizes := (others => 0);
      Console.Cols := (others => 1);
   end Start_Title;

   --  ------------------------------
   --  Finish a new title in a report.
   --  ------------------------------
   procedure End_Title (Console : in out Console_Type) is
      pragma Unreferenced (Console);
   begin
      Ada.Text_IO.New_Line;
   end End_Title;

   --  ------------------------------
   --  Start a new row in a report.
   --  ------------------------------
   overriding
   procedure Start_Row (Console : in out Console_Type) is
      pragma Unreferenced (Console);
   begin
      null;
   end Start_Row;

   --  ------------------------------
   --  Finish a new row in a report.
   --  ------------------------------
   overriding
   procedure End_Row (Console : in out Console_Type) is
      pragma Unreferenced (Console);
   begin
      Ada.Text_IO.New_Line;
   end End_Row;

end MAT.Consoles.Text;
