-----------------------------------------------------------------------
--  mat-consoles - Console interface
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

package body MAT.Consoles is

   --  ------------------------------
   --  Print the title for the given field and setup the associated field size.
   --  ------------------------------
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String;
                          Length  : in Positive) is
   begin
      Console.Sizes (Field) := Length;
      if Console.Field_Count >= 1 then
         Console.Cols (Field) := Console.Cols (Console.Fields (Console.Field_Count))
           + Console.Sizes (Console.Fields (Console.Field_Count));
      else
         Console.Cols (Field) := 1;
      end if;
      Console.Field_Count := Console.Field_Count + 1;
      Console.Fields (Console.Field_Count) := Field;
      Console_Type'Class (Console).Print_Title (Field, Title);
   end Print_Title;

   --  ------------------------------
   --  Format the address and print it for the given field.
   --  ------------------------------
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Addr    : in MAT.Types.Target_Addr) is
      Value : constant String := MAT.Types.Hex_Image (Addr);
   begin
      Console_Type'Class (Console).Print_Field (Field, Value);
   end Print_Field;

   --  ------------------------------
   --  Format the size and print it for the given field.
   --  ------------------------------
   procedure Print_Size (Console : in out Console_Type;
                         Field   : in Field_Type;
                         Size    : in MAT.Types.Target_Size) is
      Value : constant String := MAT.Types.Target_Size'Image (Size);
   begin
      Console_Type'Class (Console).Print_Field (Field, Value);
   end Print_Size;

   --  ------------------------------
   --  Format the integer and print it for the given field.
   --  ------------------------------
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in Integer) is
      Val : constant String := Integer'Image (Value);
   begin
      Console_Type'Class (Console).Print_Field (Field, Val);
   end Print_Field;

   --  ------------------------------
   --  Format the integer and print it for the given field.
   --  ------------------------------
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Console_Type'Class (Console).Print_Field (Field, Ada.Strings.Unbounded.To_String (Value));
   end Print_Field;

end MAT.Consoles;
