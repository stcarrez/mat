-----------------------------------------------------------------------
--  mat-symbols-targets - Symbol files management
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
with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

with Bfd.Symbols;
with Bfd.Files;

with Util.Refs;

with MAT.Types;
with MAT.Memory.Targets;
package MAT.Symbols.Targets is

   --  The <tt>Library_Symbols</tt> holds the symbol table associated with
   --  a shared library loaded by the program.  The <tt>Text_Addr</tt> indicates
   --  the text segment address of the loaded library.
   type Library_Symbols is new Util.Refs.Ref_Entity with record
      Text_Addr : MAT.Types.Target_Addr;
      File      : Bfd.Files.File_Type;
      Symbols   : Bfd.Symbols.Symbol_Table;
   end record;
   type Library_Symbols_Access is access all Library_Symbols;

   package Library_Symbols_Refs is
     new Util.Refs.References (Library_Symbols, Library_Symbols_Access);

   subtype Library_Symbols_Ref is Library_Symbols_Refs.Ref;

   --  The <tt>Symbols_Maps</tt> keeps a sorted list of symbol tables indexed
   --  by their mapping address.
   use type Library_Symbols_Refs.Ref;
   use type MAT.Types.Target_Addr;
   package Symbols_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Addr,
                                      Element_Type => Library_Symbols_Ref);

   subtype Symbols_Map is Symbols_Maps.Map;
   subtype Symbols_Cursor is Symbols_Maps.Cursor;

   type Target_Symbols is new Util.Refs.Ref_Entity with record
      File      : Bfd.Files.File_Type;
      Symbols   : Bfd.Symbols.Symbol_Table;
      Libraries : Symbols_Maps.Map;
   end record;
   type Target_Symbols_Access is access all Target_Symbols;

   --  Open the binary and load the symbols from that file.
   procedure Open (Symbols : in out Target_Symbols;
                   Path    : in String);

   --  Load the symbols associated with a shared library described by the memory region.
   procedure Load_Symbols (Symbols : in out Target_Symbols;
                           Region  : in MAT.Memory.Region_Info);

   --  Find the nearest source file and line for the given address.
   procedure Find_Nearest_Line (Symbols : in Target_Symbols;
                                Addr    : in MAT.Types.Target_Addr;
                                Name    : out Ada.Strings.Unbounded.Unbounded_String;
                                Func    : out Ada.Strings.Unbounded.Unbounded_String;
                                Line    : out Natural);

   package Target_Symbols_Refs is
     new Util.Refs.References (Target_Symbols, Target_Symbols_Access);

   subtype Target_Symbols_Ref is Target_Symbols_Refs.Ref;

end MAT.Symbols.Targets;
