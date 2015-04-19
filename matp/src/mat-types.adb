-----------------------------------------------------------------------
--  mat-types -- Global types
--  Copyright (C) 2014, 2015 Stephane Carrez
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

   --  ------------------------------
   --  Return an hexadecimal string representation of the value.
   --  ------------------------------
   function Hex_Image (Value  : in Uint64;
                       Length : in Positive := 16) return String is
      use type Interfaces.Unsigned_64;

      Conversion : constant String (1 .. 16) := "0123456789ABCDEF";
      S : String (1 .. Length) := (others => '0');
      P : Uint64 := Value;
      N : Uint64;
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

   --  ------------------------------
   --  Format the target time to a printable representation.
   --  ------------------------------
   function Tick_Image (Value : in Target_Tick_Ref) return String is
      use Interfaces;

      Sec  : constant Unsigned_32 := Unsigned_32 (Interfaces.Shift_Right (Uint64 (Value), 32));
      Usec : constant Unsigned_32 := Interfaces.Unsigned_32 (Value and 16#0ffffffff#);
      Frac : constant String := Interfaces.Unsigned_32'Image (Usec);
      Img  : String (1 .. 6) := (others => '0');
   begin
      Img (Img'Last - Frac'Length + 2 .. Img'Last) := Frac (Frac'First + 1 .. Frac'Last);
      return Interfaces.Unsigned_32'Image (Sec) & "." & Img;
   end Tick_Image;

   function "-" (Left, Right : in Target_Tick_Ref) return Target_Tick_Ref is
      use Interfaces;

      Res   : Target_Tick_Ref := Target_Tick_Ref (Uint64 (Left) - Uint64 (Right));
      Usec1 : constant Unsigned_32 := Interfaces.Unsigned_32 (Left and 16#0ffffffff#);
      Usec2 : constant Unsigned_32 := Interfaces.Unsigned_32 (Right and 16#0ffffffff#);
   begin
      if Usec1 < Usec2 then
         Res := Res and 16#ffffffff00000000#;
         Res := Target_Tick_Ref (Uint64 (Res) - 16#100000000#);
         Res := Res or Target_Tick_Ref (1000000 - (Usec2 - Usec1));
      end if;
      return Res;
   end "-";

   --  ------------------------------
   --  Convert the hexadecimal string into an unsigned integer.
   --  ------------------------------
   function Hex_Value (Value : in String) return Uint64 is
      use type Interfaces.Unsigned_64;

      Result : Uint64 := 0;
   begin
      if Value'Length = 0 then
         raise Constraint_Error with "Empty string";
      end if;
      for I in Value'Range loop
         declare
            C : constant Character := Value (I);
         begin
            if C >= '0' and C <= '9' then
               Result := (Result * 16) + (Character'Pos (C) - Character'Pos ('0'));
            elsif C >= 'A' and C <= 'F' then
               Result := (Result * 16) + (Character'Pos (C) - Character'Pos ('F') + 10);
            elsif C >= 'a' and C <= 'f' then
               Result := (Result * 16) + (Character'Pos (C) - Character'Pos ('f') + 10);
            else
               raise Constraint_Error with "Invalid character: " & C;
            end if;
         end;
      end loop;
      return Result;
   end Hex_Value;

end MAT.Types;
