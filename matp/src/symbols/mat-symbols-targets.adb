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
with Bfd.Sections;
with Util.Log.Loggers;
package body MAT.Symbols.Targets is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("MAT.Symbols.Targets");

   --  ------------------------------
   --  Open the binary and load the symbols from that file.
   --  ------------------------------
   procedure Open (Symbols : in out Target_Symbols;
                   Path    : in String) is
   begin
      Log.Info ("Loading symbols from {0}", Path);

      Bfd.Files.Open (Symbols.File, Path, "");
      if Bfd.Files.Check_Format (Symbols.File, Bfd.Files.OBJECT) then
         Bfd.Symbols.Read_Symbols (Symbols.File, Symbols.Symbols);
      end if;
   end Open;

   procedure Open (Symbols : in out Region_Symbols;
                   Path    : in String) is
   begin
      Log.Info ("Loading symbols from {0}", Path);

      Bfd.Files.Open (Symbols.File, Path, "");
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
         Open (Syms.Value.all, Ada.Strings.Unbounded.To_String (Region.Path));
      end if;
   end Load_Symbols;

   procedure Find_Nearest_Line (Symbols : in Region_Symbols;
                                Addr    : in MAT.Types.Target_Addr;
                                Name    : out Ada.Strings.Unbounded.Unbounded_String;
                                Func    : out Ada.Strings.Unbounded.Unbounded_String;
                                Line    : out Natural) is
      use type Bfd.Vma_Type;

      Text_Section : Bfd.Sections.Section;
      Pc : Bfd.Vma_Type := Bfd.Vma_Type (Addr);
   begin
      if not Bfd.Files.Is_Open (Symbols.File) then
         Func := Symbols.Region.Path;
         return;
      end if;
      Text_Section := Bfd.Sections.Find_Section (Symbols.File, ".text");
--        if Text_Section.Vma > Pc or else Text_Section.Vma + Text_Section.Size < Pc then
--
--        end if;
      Bfd.Symbols.Find_Nearest_Line (File    => Symbols.File,
                                     Sec     => Text_Section,
                                     Symbols => Symbols.Symbols,
                                     Addr    => Pc,
                                     Name    => Name,
                                     Func    => Func,
                                     Line    => Line);
   end Find_Nearest_Line;

   --  ------------------------------
   --  Find the nearest source file and line for the given address.
   --  ------------------------------
   procedure Find_Nearest_Line (Symbols : in Target_Symbols;
                                Addr    : in MAT.Types.Target_Addr;
                                Name    : out Ada.Strings.Unbounded.Unbounded_String;
                                Func    : out Ada.Strings.Unbounded.Unbounded_String;
                                Line    : out Natural) is
      Pos  : constant Symbols_Cursor := Symbols.Libraries.Floor (Addr);
      Text_Section : Bfd.Sections.Section;
   begin
      Line := 0;
      if Symbols_Maps.Has_Element (Pos) then
         declare
            Syms   : Region_Symbols_Ref := Symbols_Maps.Element (Pos);
         begin
            if Syms.Value.Region.End_Addr > Addr then
               Find_Nearest_Line (Symbols => Syms.Value.all,
                                  Addr    => Addr - Syms.Value.Offset,
                                  Name    => Name,
                                  Func    => Func,
                                  Line    => Line);
               return;
            end if;
         end;
      end if;
      if Bfd.Files.Is_Open (Symbols.File) then
         Text_Section := Bfd.Sections.Find_Section (Symbols.File, ".text");
         Bfd.Symbols.Find_Nearest_Line (File    => Symbols.File,
                                        Sec     => Text_Section,
                                        Symbols => Symbols.Symbols,
                                        Addr    => Bfd.Vma_Type (Addr),
                                        Name    => Name,
                                        Func    => Func,
                                        Line    => Line);
      else
         Line := 0;
         Name := Ada.Strings.Unbounded.To_Unbounded_String ("");
         Func := Ada.Strings.Unbounded.To_Unbounded_String ("");
      end if;

   exception
      when Bfd.NOT_FOUND =>
         Line := 0;
         Name := Ada.Strings.Unbounded.To_Unbounded_String ("");
         Func := Ada.Strings.Unbounded.To_Unbounded_String ("");
   end Find_Nearest_Line;

end MAT.Symbols.Targets;
