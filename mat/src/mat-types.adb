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
package body MAT.Types is

   --  ------------------------------
   --  Return an hexadecimal string representation of the value.
   --  ------------------------------
   function Hex_Image (Value  : in Uint32;
                       Length : in Positive := 8) return String is
      use type Interfaces.Unsigned_32;

      Conversion : constant String (1 .. 16) := "0123456789ABCDEF";
      S : String (1 .. Length) := (others => '0');
      P : Uint32 := Value;
      N : Uint32;
      I : Positive := Length;
   begin
      while P /= 0 loop
         N := P mod 16;
         P := P / 16;
         S (I) := Conversion (Natural (N + 1));
         exit when I = 1;
         I := I - 1;
      end loop;
      return S;
   end Hex_Image;

end MAT.Types;
