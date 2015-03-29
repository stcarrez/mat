-----------------------------------------------------------------------
--  mat-expressions-parser_io -- Input IO for Lex parser
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
with MAT.Expressions.Lexer_dfa;
with Ada.Text_IO;

package MAT.Expressions.Parser_IO is

   use MAT.Expressions.Lexer_dfa;

   user_output_file      : Ada.Text_IO.File_Type;
   NULL_IN_INPUT         : exception;
   AFLEX_INTERNAL_ERROR  : exception;
   UNEXPECTED_LAST_MATCH : exception;
   PUSHBACK_OVERFLOW     : exception;
   AFLEX_SCANNER_JAMMED  : exception;

   type eob_action_type is (EOB_ACT_RESTART_SCAN,
                            EOB_ACT_END_OF_FILE,
                            EOB_ACT_LAST_MATCH);

   YY_END_OF_BUFFER_CHAR :  constant Character := ASCII.NUL;

   --  number of characters read into yy_ch_buf
   yy_n_chars : Integer;

   --  true when we've seen an EOF for the current input file
   yy_eof_has_been_seen : Boolean;

   procedure Set_Input (Content : in String);

   procedure YY_INPUT (Buf      : out MAT.Expressions.Lexer_dfa.unbounded_character_array;
                       Result   : out Integer;
                       Max_Size : in Integer);

   function yy_get_next_buffer return eob_action_type;

   function Input_Line return Ada.Text_IO.Count;

   function yywrap return Boolean;

   procedure Open_Input (Fname : in String);

end MAT.Expressions.Parser_IO;
