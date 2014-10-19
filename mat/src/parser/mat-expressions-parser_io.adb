-----------------------------------------------------------------------
--  mat-expressions-parser_io -- Input IO for Lex parser
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
package body MAT.Expressions.Parser_IO is

   Input : Ada.Strings.Unbounded.Unbounded_String;
   Pos   : Natural := 0;

   procedure Set_Input (Content : in String) is
   begin
      Input := Ada.Strings.Unbounded.To_Unbounded_String (Content);
      Pos   := 1;
      MAT.Expressions.lexer_dfa.yy_init  := True;
      MAT.Expressions.lexer_dfa.yy_start := 0;
   end Set_Input;

   --  gets input and stuffs it into 'buf'.  number of characters read, or YY_NULL,
   --  is returned in 'result'.

   procedure YY_INPUT (Buf      : out unbounded_character_array;
                       Result   : out Integer;
                       Max_Size : in Integer) is
      I   : Integer := 1;
      Loc : Integer := Buf'First;
   begin
      while I <= Max_Size loop
         if Pos > Ada.Strings.Unbounded.Length (Input) then
            yy_eof_has_been_seen := True;
            Result := I - 1;
            return;
         end if;
         Buf (Loc) := Ada.Strings.Unbounded.Element (Input, Pos);
         Pos := Pos + 1;

         Loc := Loc + 1;
         I := I + 1;
      end loop;
      Result := I - 1;
   end YY_INPUT;

   --  yy_get_next_buffer - try to read in new buffer
   --
   --  returns a code representing an action
   --     EOB_ACT_LAST_MATCH -
   --     EOB_ACT_RESTART_SCAN - restart the scanner
   --     EOB_ACT_END_OF_FILE - end of file

   function yy_get_next_buffer return eob_action_type is
      dest           : Integer := 0;
      source         : Integer := yytext_ptr - 1; --  copy prev. char, too
      number_to_move : Integer;
      ret_val        : eob_action_type;
      num_to_read    : Integer;
   begin
      if yy_c_buf_p > yy_n_chars + 1 then
         raise NULL_IN_INPUT;
      end if;

      --  try to read more data

      --  first move last chars to start of buffer
      number_to_move := yy_c_buf_p - yytext_ptr;

      for i in 0 .. number_to_move - 1 loop
         yy_ch_buf (dest) := yy_ch_buf (source);
         dest := dest + 1;
         source := source + 1;
      end loop;

      if yy_eof_has_been_seen then
         --  don't do the read, it's not guaranteed to return an EOF,
         --  just force an EOF

         yy_n_chars := 0;
      else
         num_to_read := YY_BUF_SIZE - number_to_move - 1;

         if num_to_read > YY_READ_BUF_SIZE then
            num_to_read := YY_READ_BUF_SIZE;
         end if;

         --  read in more data
         YY_INPUT (yy_ch_buf (number_to_move .. yy_ch_buf'Last), yy_n_chars, num_to_read);
      end if;
      if yy_n_chars = 0 then
         if number_to_move = 1 then
            ret_val := EOB_ACT_END_OF_FILE;
         else
            ret_val := EOB_ACT_LAST_MATCH;
         end if;

         yy_eof_has_been_seen := True;
      else
         ret_val := EOB_ACT_RESTART_SCAN;
      end if;

      yy_n_chars := yy_n_chars + number_to_move;
      yy_ch_buf (yy_n_chars) := YY_END_OF_BUFFER_CHAR;
      yy_ch_buf (yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

      --  yytext begins at the second character in
      --  yy_ch_buf; the first character is the one which
      --  preceded it before reading in the latest buffer;
      --  it needs to be kept around in case it's a
      --  newline, so yy_get_previous_state() will have
      --  with '^' rules active

      yytext_ptr := 1;

      return ret_val;
   end yy_get_next_buffer;

   function Input_Line return Ada.Text_IO.Count is
   begin
      return 1;
   end Input_Line;

   --  default yywrap function - always treat EOF as an EOF
   function yywrap return Boolean is
   begin
      return True;
   end yywrap;

   procedure Open_Input (Fname : in String) is
      pragma Unreferenced (Fname);
   begin
      yy_init := True;
   end Open_Input;

end MAT.Expressions.Parser_IO;
