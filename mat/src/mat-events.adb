-----------------------------------------------------------------------
--  gprofiler-events - Profiler Events Description
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
with Ada.Text_IO; use Ada.Text_IO;
with MAT.Types;
package body MAT.Events is

   procedure Dump (Value : in Event_Data; Attr : in Attribute);
   function Hex_Image (Val : MAT.Types.Uint32; Len : Positive) return String;

   function Hex_Image (Val : in MAT.Types.Uint32; Len : Positive) return String is
      Conversion : constant String (1 .. 16) := "0123456789ABCDEF";
      S : String (1 .. Len) := (others => '0');
      P : MAT.Types.Uint32 := Val;
      N : MAT.Types.Uint32;
      I : Positive := Len;
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

   procedure Dump (Value : in Event_Data; Attr : in Attribute) is
   begin
      Put ("  " & Attr.Name.all);
      Set_Col (30);
      case Value.Kind is
         when T_UINT8 =>
            Put (MAT.Types.Uint8'Image (Value.U8));

         when T_UINT16 =>
            Put (MAT.Types.Uint16'Image (Value.U16));

         when T_UINT32 | T_SIZE_T =>
            Put (MAT.Types.Uint32'Image (Value.U32));

         when T_POINTER =>
            Put (Hex_Image (Value.U32, 8));

         when T_UINT64 =>
            Put (MAT.Types.Uint64'Image (Value.U64));

         when T_PROBE =>
            Put_Line ("Th " & MAT.Types.Uint32'Image (Value.Probe.Thread));
            Set_Col (29);
            for I in 1 .. Natural (Value.Probe.Cur_Depth) loop
               exit when I > Value.Probe.Frame'Last;
               if Col > 80 - 14 then
                  Set_Col (29);
               end if;
               Put (" {"
                    & Hex_Image (Unsigned_32 (Value.Probe.Frame (I)), 8)
                    & "}");
            end loop;

         when others =>
            null;
      end case;
      New_Line;
   end Dump;

   procedure Dump (Table : in Event_Data_Table;
                   Def   : in Event_Description) is
   begin
      Put_Line (Ada.Strings.Unbounded.To_String (Def.Name) & ":");
      for I in 1 .. Def.Nb_Attributes loop
         Dump (Table (I), Def.Def (I));
      end loop;
   end Dump;

end MAT.Events;
