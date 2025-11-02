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

with Ada.Text_IO;
package body MAT.Events is

   procedure Dump (Value : in Event_Data; Attr : in Attribute);

   procedure Dump (Value : in Event_Data; Attr : in Attribute) is
      use type Ada.Text_IO.Positive_Count;
   begin
      Ada.Text_IO.Put ("  " & Attr.Name.all);
      Ada.Text_IO.Set_Col (30);
      case Value.Kind is
         when T_UINT8 =>
            Ada.Text_IO.Put (MAT.Types.Uint8'Image (Value.U8));

         when T_UINT16 =>
            Ada.Text_IO.Put (MAT.Types.Uint16'Image (Value.U16));

         when T_UINT32 | T_SIZE_T =>
            Ada.Text_IO.Put (MAT.Types.Uint32'Image (Value.U32));

         when T_POINTER =>
            Ada.Text_IO.Put (MAT.Types.Hex_Image (Value.U32, 8));

         when T_UINT64 =>
            Ada.Text_IO.Put (MAT.Types.Uint64'Image (Value.U64));

         when T_PROBE =>
            Ada.Text_IO.Put_Line ("Th " & MAT.Types.Target_Thread_Ref'Image (Value.Probe.Thread));
            Ada.Text_IO.Set_Col (29);
            for I in 1 .. Value.Probe.Cur_Depth loop
               exit when I > Value.Probe.Frame'Last;
               if Ada.Text_IO.Col > 80 - 14 then
                  Ada.Text_IO.Set_Col (29);
               end if;
               Ada.Text_IO.Put (" {"
                                & MAT.Types.Hex_Image (Unsigned_32 (Value.Probe.Frame (I)), 8)
                                & "}");
            end loop;

         when others =>
            null;
      end case;
      Ada.Text_IO.New_Line;
   end Dump;

   procedure Dump (Table : in Event_Data_Table;
                   Def   : in Event_Description) is
   begin
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Def.Name) & ":");
      for I in 1 .. Def.Nb_Attributes loop
         Dump (Table (I), Def.Def (I));
      end loop;
   end Dump;

end MAT.Events;
