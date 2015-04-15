-----------------------------------------------------------------------
--  elf -- ELF information
--  Copyright (C) 2015 Stephane Carrez
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
package ELF is

   pragma Preelaborate;

   subtype Elf32_Word is Interfaces.Unsigned_32;
   subtype Elf64_Word is Interfaces.Unsigned_64;

   --  Legal values for the ELF header p_type (segment type).
   PT_NULL    : constant Elf32_Word := 0;   --  Program header table entry unused.
   PT_LOAD    : constant Elf32_Word := 1;   --  Loadable program segment
   PT_DYNAMIC : constant Elf32_Word := 2;   --  Dynamic linking information
   PT_INTERP  : constant Elf32_Word := 3;   --  Program interpreter
   PT_NOTE    : constant Elf32_Word := 4;   --  Auxiliary information
   PT_SHLIB   : constant Elf32_Word := 5;   --  Reserved
   PT_PHDR    : constant Elf32_Word := 6;   --  Entry for header table itself
   PT_TLS     : constant Elf32_Word := 7;   --  Thread-local storage segment
   PT_NUM     : constant Elf32_Word := 8;   --  Number of defined types
   PT_LOOS    : constant Elf32_Word := 16#60000000#;       --  Start of OS-specific
   PT_GNU_EH_FRAME : constant Elf32_Word := 16#6474e550#;  --  GCC .eh_frame_hdr segment
   PT_GNU_STACK    : constant Elf32_Word := 16#6474e551#;  --  Indicates stack executability
   PT_GNU_RELRO    : constant Elf32_Word := 16#6474e552#;  --  Read-only after relocation
   PT_LOSUNW       : constant Elf32_Word := 16#6ffffffa#;
   PT_SUNWBSS      : constant Elf32_Word := 16#6ffffffa#;  --  Sun Specific segment
   PT_SUNWSTACK    : constant Elf32_Word := 16#6ffffffb#;  --  Stack segment
   PT_HISUNW       : constant Elf32_Word := 16#6fffffff#;
   PT_HIOS         : constant Elf32_Word := 16#6fffffff#;  --  End of OS-specific
   PT_LOPROC       : constant Elf32_Word := 16#70000000#;  --  Start of processor-specific
   PT_HIPROC       : constant Elf32_Word := 16#7fffffff#;  --  End of processor-specific

   --  Legal values for ELF header p_flags (segment flags).
   PF_X        : constant Elf32_Word := 1;  --  Segment is executable.
   PF_W        : constant Elf32_Word := 2;  --  Segment is writable.
   PF_R        : constant Elf32_Word := 4;  --  Segment is readable.

end ELF;
