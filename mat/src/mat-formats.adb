-----------------------------------------------------------------------
--  mat-formats - Format various types for the console or GUI interface
--  Copyright (C) 2015 Stephane Carrez
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

package body MAT.Formats is

   --  ------------------------------
   --  Format the address into a string.
   --  ------------------------------
   function Addr (Value : in MAT.Types.Target_Addr) return String is
      Hex : constant String := MAT.Types.Hex_Image (Value);
   begin
      return Hex;
   end Addr;

   --  ------------------------------
   --  Format the size into a string.
   --  ------------------------------
   function Size (Value : in MAT.Types.Target_Size) return String is
      Result : constant String := MAT.Types.Target_Size'Image (Value);
   begin
      if Result (Result'First) = ' ' then
         return Result (Result'First + 1 .. Result'Last);
      else
         return Result;
      end if;
   end Size;

   function Location (File : in Ada.Strings.Unbounded.Unbounded_String) return String is
      Pos : constant Natural := Ada.Strings.Unbounded.Index (File, "/", Ada.Strings.Backward);
      Len : constant Natural := Ada.Strings.Unbounded.Length (File);
   begin
      if Pos /= 0 then
         return Ada.Strings.Unbounded.Slice (File, Pos + 1, Len);
      else
         return Ada.Strings.Unbounded.To_String (File);
      end if;
   end Location;

   --  ------------------------------
   --  Format a file, line, function information into a string.
   --  ------------------------------
   function Location (File : in Ada.Strings.Unbounded.Unbounded_String;
                      Line : in Natural;
                      Func : in Ada.Strings.Unbounded.Unbounded_String) return String is
   begin
      if Ada.Strings.Unbounded.Length (File) = 0 then
         return Ada.Strings.Unbounded.To_String (Func);
      elsif Line > 0 then
         declare
            Num : constant String := Natural'Image (Line);
         begin
            return Ada.Strings.Unbounded.To_String (Func) & " ("
              & Location (File) & ":" & Num (Num'First + 1 .. Num'Last) & ")";
         end;
      else
         return Ada.Strings.Unbounded.To_String (Func) & " (" & Location (File) & ")";
      end if;
   end Location;

end MAT.Formats;
