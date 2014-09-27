-----------------------------------------------------------------------
--  mat-symbols-targets - Symbol files management
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

package body MAT.Symbols.Targets is

   --  ------------------------------
   --  Open the binary and load the symbols from that file.
   --  ------------------------------
   procedure Open (Symbols : in out Target_Symbols;
                   Path    : in String) is
   begin
      Bfd.Files.Open (Symbols.File, Path, "");
      if Bfd.Files.Check_Format (Symbols.File, Bfd.Files.OBJECT) then
         Bfd.Symbols.Read_Symbols (Symbols.File, Symbols.Symbols);
      end if;
   end Open;

end MAT.Symbols.Targets;
