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

with Bfd.Symbols;
with Bfd.Files;
package MAT.Symbols.Targets is

   type Target_Symbols is limited record
      File    : Bfd.Files.File_Type;
      Symbols : Bfd.Symbols.Symbol_Table;
   end record;

   --  Open the binary and load the symbols from that file.
   procedure Open (Symbols : in out Target_Symbols;
                   Path    : in String);

end MAT.Symbols.Targets;
