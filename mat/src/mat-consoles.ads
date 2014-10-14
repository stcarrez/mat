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
with MAT.Types;
package MAT.Consoles is

   type Field_Type is (F_ADDR,
                       F_SIZE,
                       F_THREAD);

   type Console_Type is abstract tagged limited private;

   --  Print the field value for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in String) is abstract;

   --  Print the title for the given field.
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String) is abstract;

   --  Format the address and print it for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Addr    : in MAT.Types.Target_Addr);

   --  Format the size and print it for the given field.
   procedure Print_Size (Console : in out Console_Type;
                         Field   : in Field_Type;
                         Size    : in MAT.Types.Target_Size);

   --  Format the integer and print it for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in Integer);

private

   type Console_Type is abstract tagged limited record
      N : Natural;
   end record;

end MAT.Consoles;
