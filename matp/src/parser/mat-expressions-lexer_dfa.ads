pragma Style_Checks (Off);
package MAT.Expressions.Lexer_dfa is
aflex_debug : boolean := false;
   --  Warning: This file is automatically generated by AFLEX.
   --           It is useless to modify it. Change the ".Y" & ".L" files instead.

   yytext_ptr : integer; -- points to start of yytext in buffer

   -- yy_ch_buf has to be 2 characters longer than YY_BUF_SIZE because we need
   -- to put in 2 end-of-buffer characters (this is explained where it is
   -- done) at the end of yy_ch_buf

   --  ----------------------------------------------------------------------------
   --  If the buffer size variable YY_READ_BUF_SIZE is too small, then
   --  big comments won't be parsed and the parser stops.
   --  YY_READ_BUF_SIZE should be at least as large as the number of ASCII bytes in
   --  comments that need to be parsed.

   YY_READ_BUF_SIZE : constant integer :=  75_000;
   --  ----------------------------------------------------------------------------

   YY_BUF_SIZE : constant integer := YY_READ_BUF_SIZE * 2; -- size of input buffer
   type unbounded_character_array is array(integer range <>) of character;
   subtype ch_buf_type is unbounded_character_array(0..YY_BUF_SIZE + 1);
   yy_ch_buf : ch_buf_type;
yy_cp, yy_bp : integer;

   -- yy_hold_char holds the character lost when yytext is formed
   yy_hold_char : character;
   yy_c_buf_p : integer;   -- points to current character in buffer

   function YYText return string;
   function YYLength return integer;
   procedure YY_DO_BEFORE_ACTION;
   --These variables are needed between calls to YYLex.
   yy_init : boolean := true; -- do we need to initialize YYLex?
   yy_start : integer := 0; -- current start state number
   subtype yy_state_type is integer;
   yy_last_accepting_state : yy_state_type;
   yy_last_accepting_cpos : integer;
end MAT.Expressions.Lexer_dfa;
