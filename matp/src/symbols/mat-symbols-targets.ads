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
with Bfd.Constants;

with Util.Refs;

with MAT.Types;
with MAT.Consoles;
with MAT.Memory;
package MAT.Symbols.Targets is

   type Symbol_Info is record
      File : Ada.Strings.Unbounded.Unbounded_String;
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Line : Natural;
   end record;

   --  The <tt>Region_Symbols</tt> holds the symbol table associated with the program or
   --  a shared library loaded by the program.  The <tt>Region</tt> indicates
   --  the text segment address of the program or the loaded library.
   type Region_Symbols is new Util.Refs.Ref_Entity with record
      Region     : MAT.Memory.Region_Info;
      Offset     : MAT.Types.Target_Addr;
      File       : Bfd.Files.File_Type;
      Symbols    : Bfd.Symbols.Symbol_Table;
   end record;
   type Region_Symbols_Access is access all Region_Symbols;

   --  Load the symbol table for the associated region.
   procedure Open (Symbols : in out Region_Symbols;
                   Path    : in String);

   package Region_Symbols_Refs is
     new Util.Refs.References (Region_Symbols, Region_Symbols_Access);

   subtype Region_Symbols_Ref is Region_Symbols_Refs.Ref;

   --  The <tt>Symbols_Maps</tt> keeps a sorted list of symbol tables indexed
   --  by their mapping address.
   use type Region_Symbols_Refs.Ref;
   use type MAT.Types.Target_Addr;
   package Symbols_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => MAT.Types.Target_Addr,
                                      Element_Type => Region_Symbols_Ref);

   subtype Symbols_Map is Symbols_Maps.Map;
   subtype Symbols_Cursor is Symbols_Maps.Cursor;

   type Target_Symbols is new Util.Refs.Ref_Entity with record
      Path      : Ada.Strings.Unbounded.Unbounded_String;
      File      : Bfd.Files.File_Type;
      Symbols   : Bfd.Symbols.Symbol_Table;
      Libraries : Symbols_Maps.Map;
      Demangle  : Bfd.Demangle_Flags := Bfd.Constants.DMGL_AUTO;
      Console   : MAT.Consoles.Console_Access;
   end record;
   type Target_Symbols_Access is access all Target_Symbols;

   --  Open the binary and load the symbols from that file.
   procedure Open (Symbols : in out Target_Symbols;
                   Path    : in String);

   --  Load the symbols associated with a shared library described by the memory region.
   procedure Load_Symbols (Symbols     : in out Target_Symbols;
                           Region      : in MAT.Memory.Region_Info;
                           Offset_Addr : in MAT.Types.Target_Addr);

   --  Load the symbols associated with all the shared libraries described by
   --  the memory region map.
   procedure Load_Symbols (Symbols     : in out Target_Symbols;
                           Regions     : in MAT.Memory.Region_Info_Map);

   --  Demangle the symbol.
   procedure Demangle (Symbols : in Target_Symbols;
                       Symbol  : in out Symbol_Info);

   --  Find the nearest source file and line for the given address.
   procedure Find_Nearest_Line (Symbols : in Target_Symbols;
                                Addr    : in MAT.Types.Target_Addr;
                                Symbol  : out Symbol_Info);

   package Target_Symbols_Refs is
     new Util.Refs.References (Target_Symbols, Target_Symbols_Access);

   subtype Target_Symbols_Ref is Target_Symbols_Refs.Ref;

end MAT.Symbols.Targets;
