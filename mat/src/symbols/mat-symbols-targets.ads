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
with Ada.Strings.Unbounded;

with Bfd.Symbols;
with Bfd.Files;

with Util.Refs;

with MAT.Types;
package MAT.Symbols.Targets is

   type Target_Symbols is new Util.Refs.Ref_Entity with record
      File    : Bfd.Files.File_Type;
      Symbols : Bfd.Symbols.Symbol_Table;
   end record;
   type Target_Symbols_Access is access all Target_Symbols;

   --  Open the binary and load the symbols from that file.
   procedure Open (Symbols : in out Target_Symbols;
                   Path    : in String);

   --  Find the nearest source file and line for the given address.
   procedure Find_Nearest_Line (Symbols : in Target_Symbols;
                                Addr    : in MAT.Types.Target_Addr;
                                Name    : out Ada.Strings.Unbounded.Unbounded_String;
                                Func    : out Ada.Strings.Unbounded.Unbounded_String;
                                Line    : out Natural);

   package Target_Symbols_Refs is
     new Util.Refs.References (Target_Symbols, Target_Symbols_Access);

end MAT.Symbols.Targets;
