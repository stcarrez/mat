-----------------------------------------------------------------------
--  mat-symbols-targets - Symbol files management
--  Copyright (C) 2014, 2015, 2019, 2021 Stephane Carrez
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
with Bfd.Sections;
with ELF;
with Util.Strings;
with Util.Files;
with Util.Log.Loggers;
with Ada.Directories;
with MAT.Formats;
package body MAT.Symbols.Targets is

   use Ada.Strings.Unbounded;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Symbols.Targets");

   procedure Find_Nearest_Line (Symbols : in Region_Symbols;
                                Addr    : in MAT.Types.Target_Addr;
                                Symbol  : out Symbol_Info);

   --  ------------------------------
   --  Open the binary and load the symbols from that file.
   --  ------------------------------
   procedure Open (Symbols : in out Target_Symbols;
                   Path    : in String) is
   begin
      Log.Info ("Loading symbols from {0}", Path);
      --
      --        Bfd.Files.Open (Symbols.File, Path, "");
      --        if Bfd.Files.Check_Format (Symbols.File, Bfd.Files.OBJECT) then
      --           Bfd.Symbols.Read_Symbols (Symbols.File, Symbols.Symbols);
      --        end if;
   end Open;

   --  ------------------------------
   --  Load the symbol table for the associated region.
   --  ------------------------------
   procedure Open (Symbols     : in out Region_Symbols;
                   Path        : in String;
                   Search_Path : in String) is
      Pos : Natural;
   begin
      Log.Info ("Loading symbols from {0}", Path);

      if Ada.Directories.Exists (Path) then
         Bfd.Files.Open (Symbols.File, Path, "");
      else
         Pos := Util.Strings.Rindex (Path, '/');
         if Pos > 0 then
            Bfd.Files.Open (Symbols.File,
                            Util.Files.Find_File_Path (Path (Pos + 1 .. Path'Last), Search_Path));
         end if;
      end if;
      if Bfd.Files.Check_Format (Symbols.File, Bfd.Files.OBJECT) then
         Bfd.Symbols.Read_Symbols (Symbols.File, Symbols.Symbols);
      end if;
   end Open;

   --  ------------------------------
   --  Load the symbols associated with a shared library described by the memory region.
   --  ------------------------------
   procedure Load_Symbols (Symbols     : in out Target_Symbols;
                           Region      : in MAT.Memory.Region_Info;
                           Offset_Addr : in MAT.Types.Target_Addr) is
      use type Bfd.File_Flags;

      Pos  : constant Symbols_Cursor := Symbols.Libraries.Find (Region.Start_Addr);
      Syms : Region_Symbols_Ref;
   begin
      if not Symbols_Maps.Has_Element (Pos) then
         Syms := Region_Symbols_Refs.Create;
         Syms.Value.Region := Region;
         Syms.Value.Offset := Offset_Addr;
         Symbols.Libraries.Insert (Region.Start_Addr, Syms);
      else
         Syms := Symbols_Maps.Element (Pos);
      end if;
      if Ada.Strings.Unbounded.Length (Region.Path) > 0 then
         Open (Syms.Value, Ada.Strings.Unbounded.To_String (Region.Path),
               Ada.Strings.Unbounded.To_String (Symbols.Search_Path));
         if Bfd.Files.Is_Open (Syms.Value.File) and
           then (Bfd.Files.Get_File_Flags (Syms.Value.File) and Bfd.Files.EXEC_P) /= 0
         then
            Syms.Value.Offset := 0;
         end if;
      end if;
   end Load_Symbols;

   --  ------------------------------
   --  Load the symbols associated with all the shared libraries described by
   --  the memory region map.
   --  ------------------------------
   procedure Load_Symbols (Symbols     : in out Target_Symbols;
                           Regions     : in MAT.Memory.Region_Info_Map) is
      use type ELF.Elf32_Word;

      Iter    : MAT.Memory.Region_Info_Cursor := Regions.First;
   begin
      while MAT.Memory.Region_Info_Maps.Has_Element (Iter) loop
         declare
            Region : MAT.Memory.Region_Info := MAT.Memory.Region_Info_Maps.Element (Iter);
            Offset : MAT.Types.Target_Addr;
         begin
            if (Region.Flags and ELF.PF_X) /= 0 then
               if Ada.Strings.Unbounded.Length (Region.Path) = 0 then
                  Region.Path := Symbols.Path;
                  Offset := 0;
               else
                  Offset := Region.Start_Addr;
               end if;
               MAT.Symbols.Targets.Load_Symbols (Symbols, Region, Offset);
            end if;

         exception
            when Bfd.OPEN_ERROR =>
               Symbols.Console.Error ("Cannot open symbol library file '"
                                        & Ada.Strings.Unbounded.To_String (Region.Path) & "'");
         end;
         MAT.Memory.Region_Info_Maps.Next (Iter);
      end loop;
   end Load_Symbols;

   --  ------------------------------
   --  Demangle the symbol.
   --  ------------------------------
   procedure Demangle (Symbols : in Target_Symbols;
                       Symbol  : in out Symbol_Info) is
      use type Bfd.Demangle_Flags;

      Pos : constant Natural := Index (Symbol.File, ".", Ada.Strings.Backward);
   begin
      if Symbol.Line = 0 or else Pos = 0 or else Symbol.Symbols.Is_Null then
         return;
      end if;
      declare
         Mode : Bfd.Demangle_Flags := Symbols.Demangle;
         Len  : constant Positive := Length (Symbol.File);
         Ext  : constant String := Slice (Symbol.File, Pos, Len);
         Sym  : constant String := To_String (Symbol.Name);
      begin
         if Mode = Bfd.Constants.DMGL_AUTO and then Ext = ".adb" then
            Mode := Bfd.Constants.DMGL_GNAT;
         end if;
         declare
            Name : constant String := Bfd.Symbols.Demangle (Symbol.Symbols.Value.File, Sym, Mode);
         begin
            if Name'Length > 0 then
               Symbol.Name := To_Unbounded_String (Name);
            end if;
         end;
      end;
   end Demangle;

   procedure Find_Nearest_Line (Symbols : in Region_Symbols;
                                Addr    : in MAT.Types.Target_Addr;
                                Symbol  : out Symbol_Info) is
      Text_Section : Bfd.Sections.Section;
      Pc : constant Bfd.Vma_Type := Bfd.Vma_Type (Addr);
   begin
      if not Bfd.Files.Is_Open (Symbols.File) then
         Symbol.File := Symbols.Region.Path;
         return;
      end if;
      Text_Section := Bfd.Sections.Find_Section (Symbols.File, ".text");
      Bfd.Symbols.Find_Nearest_Line (File    => Symbols.File,
                                     Sec     => Text_Section,
                                     Symbols => Symbols.Symbols,
                                     Addr    => Pc,
                                     Name    => Symbol.File,
                                     Func    => Symbol.Name,
                                     Line    => Symbol.Line);
   end Find_Nearest_Line;

   --  ------------------------------
   --  Find the nearest source file and line for the given address.
   --  ------------------------------
   procedure Find_Nearest_Line (Symbols : in Target_Symbols;
                                Addr    : in MAT.Types.Target_Addr;
                                Symbol  : out Symbol_Info) is
      Pos  : constant Symbols_Cursor := Symbols.Libraries.Floor (Addr);
   begin
      Symbol.Line := 0;
      if Symbols_Maps.Has_Element (Pos) then
         declare
            Syms : constant Region_Symbols_Ref := Symbols_Maps.Element (Pos);
         begin
            if Syms.Value.Region.End_Addr > Addr then
               Symbol.Symbols := Syms;
               Find_Nearest_Line (Symbols => Syms.Value,
                                  Addr    => Addr - Syms.Value.Offset,
                                  Symbol  => Symbol);
               if Symbols.Use_Demangle then
                  Demangle (Symbols, Symbol);
               end if;
               return;
            end if;

         exception
            when Bfd.NOT_FOUND =>
               Symbol.Line := 0;
               Symbol.File := Syms.Value.Region.Path;
               Symbol.Name := Ada.Strings.Unbounded.To_Unbounded_String (MAT.Formats.Addr (Addr));
               return;
         end;
      end if;
      Symbol.Line := 0;
      Symbol.File := Ada.Strings.Unbounded.To_Unbounded_String ("");
      Symbol.Name := Ada.Strings.Unbounded.To_Unbounded_String (MAT.Formats.Addr (Addr));
   end Find_Nearest_Line;

   --  ------------------------------
   --  Find the symbol in the symbol table and return the start and end address.
   --  ------------------------------
   procedure Find_Symbol_Range (Symbols : in Target_Symbols;
                                Name    : in String;
                                From    : out MAT.Types.Target_Addr;
                                To      : out MAT.Types.Target_Addr) is
      use type Bfd.Symbols.Symbol;

      Iter  : Symbols_Cursor := Symbols.Libraries.First;
   begin
      while Symbols_Maps.Has_Element (Iter) loop
         declare
            Syms : constant Region_Symbols_Ref := Symbols_Maps.Element (Iter);
            Sym  : Bfd.Symbols.Symbol;
            Sec  : Bfd.Sections.Section;
         begin
            if not Syms.Is_Null and then Bfd.Files.Is_Open (Syms.Value.File) then
               Sym := Bfd.Symbols.Get_Symbol (Syms.Value.Symbols, Name);
               if Sym /= Bfd.Symbols.Null_Symbol then
                  Sec := Bfd.Symbols.Get_Section (Sym);
                  if not Bfd.Sections.Is_Undefined_Section (Sec) then
                     From := MAT.Types.Target_Addr (Bfd.Symbols.Get_Value (Sym));
                     From := From + Syms.Value.Offset;
                     To := From + MAT.Types.Target_Addr (Bfd.Symbols.Get_Symbol_Size (Sym));
                     return;
                  end if;
               end if;
            end if;
         end;
         Symbols_Maps.Next (Iter);
      end loop;
   end Find_Symbol_Range;

   --  ------------------------------
   --  Find the symbol region in the symbol table which contains the given address
   --  and return the start and end address.
   --  ------------------------------
   procedure Find_Symbol_Range (Symbols : in Target_Symbols;
                                Addr    : in MAT.Types.Target_Addr;
                                From    : out MAT.Types.Target_Addr;
                                To      : out MAT.Types.Target_Addr) is
      use type Bfd.Symbols.Symbol;

      Pos  : constant Symbols_Cursor := Symbols.Libraries.Floor (Addr);
   begin
      if Symbols_Maps.Has_Element (Pos) then
         declare
            Syms   : constant Region_Symbols_Ref := Symbols_Maps.Element (Pos);
            Symbol : Symbol_Info;
            Sym    : Bfd.Symbols.Symbol;
            Sec    : Bfd.Sections.Section;
         begin
            if Syms.Value.Region.End_Addr > Addr then
               Symbol.Line := 0;
               Symbol.Symbols := Syms;
               Find_Nearest_Line (Symbols => Syms.Value,
                                  Addr    => Addr - Syms.Value.Offset,
                                  Symbol  => Symbol);

               Sym := Bfd.Symbols.Get_Symbol (Syms.Value.Symbols, To_String (Symbol.Name));
               if Sym /= Bfd.Symbols.Null_Symbol then
                  Sec := Bfd.Symbols.Get_Section (Sym);
                  if not Bfd.Sections.Is_Undefined_Section (Sec) then
                     From := MAT.Types.Target_Addr (Bfd.Symbols.Get_Value (Sym));
                     From := From + Syms.Value.Offset;
                     To := From + MAT.Types.Target_Addr (Bfd.Symbols.Get_Symbol_Size (Sym));
                     return;
                  end if;
               end if;
            end if;
         end;
      end if;
      From := Addr;
      To := Addr;

   exception
      when Bfd.NOT_FOUND =>
         From := Addr;
         To := Addr;
   end Find_Symbol_Range;

end MAT.Symbols.Targets;
