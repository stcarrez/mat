-----------------------------------------------------------------------
--  Frames - Representation of stack frames
--  Copyright (C) 2014, 2019, 2021 Stephane Carrez
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
with Ada.Text_IO; use Ada.Text_IO;
with MAT.Types;
with System.Address_Image;
with Interfaces;
procedure MAT.Frames.Print (File : in File_Type;
                            F : in Frame_Type) is

   use MAT.Types;
   use Interfaces;

   Depth : constant Natural := F.Depth;
   Child : Frame_Type;

   procedure Print_Address (File : in File_Type;
                            Addr : in Target_Addr);

   function Hex_Image (Val : Target_Addr; Len : Positive) return String;

   function Hex_Image (Val : Target_Addr; Len : Positive) return String is
      Conversion : constant String (1 .. 16) := "0123456789ABCDEF";
      S : String (1 .. Len) := (others => '0');
      P : Target_Addr := Val;
      N : Target_Addr;
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

   procedure Print_Address (File : in File_Type;
                            Addr : in Target_Addr) is
   begin
      Put (File, Hex_Image (Addr, 20));
   end Print_Address;

begin
   Set_Col (File, Positive_Count (Depth + 1));
   if F.Parent = null then
      Put_Line (File, "R " & Natural'Image (F.Used) & " - "
                  & System.Address_Image (F.all'Address));
   else
      Put_Line (File, "F " & Natural'Image (F.Used) & " - "
                  & System.Address_Image (F.all'Address)
                  & " - P=" & System.Address_Image (F.Parent.all'Address));
   end if;
   Set_Col (File, Positive_Count (Depth + 1));
   Print_Address (File, F.Pc);
   Put_Line (File, " D " & Natural'Image (Depth));
   Child := F.Children;
   while Child /= null loop
      Print (File, Child);
      Child := Child.Next;
   end loop;
end MAT.Frames.Print;
