
with MAT.Types;
with MAT.Expressions;
with Ada.Text_IO;
with MAT.Expressions.Lexer_dfa;
with MAT.Expressions.Parser_IO;

package body MAT.Expressions.Lexer is

   use Ada.Text_IO;
   use Ada;
   use MAT.Expressions.Lexer_dfa;
   use MAT.Expressions.Parser_IO;

   Line_Number : Natural := 0;

   pragma Style_Checks (Off);
   pragma Warnings (Off);

   function YYLex return Token is
      subtype Short is Integer range -32768 .. 32767;

      --  returned upon end-of-file
      YY_END_TOK : constant Integer := 0;
      subtype yy_state_type is Integer;
      YY_END_OF_BUFFER : constant := 49;
      INITIAL : constant := 0;
      yy_accept : constant array (0 .. 127) of Short :=
          (0,
        0,    0,   49,   47,    1,   48,   44,   45,   47,   29,
       30,   33,   47,   42,   42,   35,   39,   37,   31,   47,
       32,   45,   45,   45,   45,   45,   45,   45,   45,   45,
       45,   45,   45,   45,   45,   45,    1,   45,    0,   34,
        0,   42,    0,   36,   38,   28,   41,   45,   45,   45,
       45,   17,   45,   16,   45,   45,   45,   45,   18,   23,
       45,   45,   45,    3,   45,   45,   45,   25,   45,   46,
       43,   40,   45,   45,   22,    4,   45,   45,   45,   45,
       45,   27,   45,   45,   45,    5,   45,   45,   45,   45,
       20,   45,   45,   45,   45,   12,   24,   14,   45,   45,

       45,   19,   45,    2,    6,   45,   45,   21,   45,   45,
       45,   45,   45,    7,   26,   11,   15,   45,   10,    8,
       13,   45,   45,   45,   45,    9,    0
       );

      yy_ec : constant array (ASCII.NUL .. Character'Last) of Short := (0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    1,    4,    1,    5,    1,    1,    6,    7,
        8,    1,    1,    9,    1,   10,    1,   11,   12,   12,
       12,   12,   12,   12,   12,   13,   13,    1,    1,   14,
       15,   16,    1,    1,   17,   17,   17,   17,   17,   17,
        5,    5,    5,    5,    5,    5,    5,    5,    5,    5,
        5,    5,    5,    5,    5,    5,    5,    5,    5,    5,
       18,   19,   20,    1,    5,    1,   21,   22,   23,   24,

       25,   26,    5,   27,   28,    5,   29,   30,   31,   32,
       33,    5,    5,   34,   35,   36,    5,   37,   38,   39,
       40,   41,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1, others => 1

       );

      yy_meta : constant array (0 .. 41) of Short :=
          (0,
        1,    1,    1,    1,    2,    1,    1,    1,    1,    2,
        3,    3,    3,    1,    1,    1,    3,    1,    1,    1,
        3,    3,    3,    3,    3,    3,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
        2
       );

      yy_base : constant array (0 .. 129) of Short :=
          (0,
        0,    0,  152,  153,  149,  153,  153,    0,  147,  153,
      153,  153,  139,   32,   36,  133,  153,  132,  153,   47,
      153,   30,   27,  118,  108,  110,  122,   29,  117,  120,
      107,  105,  113,  109,   24,  108,  133,    0,  128,  153,
       57,   62,    0,  153,  153,  153,   65,  109,   96,  101,
      106,    0,  103,    0,   94,  102,   30,   91,    0,    0,
      104,   94,   52,    0,  102,   81,   87,    0,   84,  153,
       68,    0,   85,   93,    0,    0,   84,   91,   83,   89,
       82,    0,   83,   81,   76,    0,   79,   83,   82,   79,
        0,   71,   70,   80,   66,    0,    0,    0,   68,   75,

       69,    0,   77,   69,    0,   71,   59,    0,   71,   68,
       59,   67,   55,    0,    0,    0,    0,   63,    0,    0,
       64,   48,   55,   49,   33,    0,  153,   87,   50
       );

      yy_def : constant array (0 .. 129) of Short :=
          (0,
      127,    1,  127,  127,  127,  127,  127,  128,  127,  127,
      127,  127,  127,  127,  127,  127,  127,  127,  127,  127,
      127,  128,  128,  128,  128,  128,  128,  128,  128,  128,
      128,  128,  128,  128,  128,  128,  127,  128,  127,  127,
      127,  127,  129,  127,  127,  127,  127,  128,  128,  128,
      128,  128,  128,  128,  128,  128,  128,  128,  128,  128,
      128,  128,  128,  128,  128,  128,  128,  128,  128,  127,
      127,  129,  128,  128,  128,  128,  128,  128,  128,  128,
      128,  128,  128,  128,  128,  128,  128,  128,  128,  128,
      128,  128,  128,  128,  128,  128,  128,  128,  128,  128,

      128,  128,  128,  128,  128,  128,  128,  128,  128,  128,
      128,  128,  128,  128,  128,  128,  128,  128,  128,  128,
      128,  128,  128,  128,  128,  128,    0,  127,  127
       );

      yy_nxt : constant array (0 .. 194) of Short :=
          (0,
        4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
       14,   15,   15,   16,   17,   18,    8,   19,   20,   21,
       22,   23,    8,   24,   25,   26,   27,   28,    8,   29,
       30,   31,   32,   33,   34,   35,    8,   36,    8,    8,
        8,   41,   42,   42,   42,   41,   42,   42,   42,   46,
       67,   53,   72,   48,   80,   49,   68,   47,   47,   50,
       59,   51,   81,   60,  126,   52,   54,   71,   71,   71,
       43,   41,   42,   42,   42,   47,   47,   85,   71,   71,
       71,  125,  124,  123,  122,  121,  120,   86,   38,   38,
      119,  118,  117,  116,  115,  114,  113,  112,  111,  110,

      109,  108,  107,  106,  105,  104,  103,  102,  101,  100,
       99,   98,   97,   96,   95,   94,   93,   92,   91,   90,
       89,   88,   87,   84,   83,   82,   79,   78,   77,   76,
       75,   74,   73,   70,   37,   69,   66,   65,   64,   63,
       62,   61,   58,   57,   56,   55,   45,   44,   40,   39,
       37,  127,    3,  127,  127,  127,  127,  127,  127,  127,
      127,  127,  127,  127,  127,  127,  127,  127,  127,  127,
      127,  127,  127,  127,  127,  127,  127,  127,  127,  127,
      127,  127,  127,  127,  127,  127,  127,  127,  127,  127,
      127,  127,  127,  127

       );

      yy_chk : constant array (0 .. 194) of Short :=
          (0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,   14,   14,   14,   14,   15,   15,   15,   15,   20,
       35,   23,  129,   22,   57,   22,   35,   20,   20,   22,
       28,   22,   57,   28,  125,   22,   23,   41,   41,   41,
       14,   42,   42,   42,   42,   47,   47,   63,   71,   71,
       71,  124,  123,  122,  121,  118,  113,   63,  128,  128,
      112,  111,  110,  109,  107,  106,  104,  103,  101,  100,

       99,   95,   94,   93,   92,   90,   89,   88,   87,   85,
       84,   83,   81,   80,   79,   78,   77,   74,   73,   69,
       67,   66,   65,   62,   61,   58,   56,   55,   53,   51,
       50,   49,   48,   39,   37,   36,   34,   33,   32,   31,
       30,   29,   27,   26,   25,   24,   18,   16,   13,    9,
        5,    3,  127,  127,  127,  127,  127,  127,  127,  127,
      127,  127,  127,  127,  127,  127,  127,  127,  127,  127,
      127,  127,  127,  127,  127,  127,  127,  127,  127,  127,
      127,  127,  127,  127,  127,  127,  127,  127,  127,  127,
      127,  127,  127,  127

       );

      yy_act : Integer;
      yy_c   : Short;
      yy_current_state : yy_state_type;

      --  copy whatever the last rule matched to the standard output

      --  enter a start condition.
      --  Using procedure requires a () after the ENTER, but makes everything
      --  much neater.

      procedure ENTER (state : Integer) is
      begin
         yy_start := 1 + 2 * state;
      end ENTER;

      --  action number for EOF rule of a given start state
      function YY_STATE_EOF (state : Integer) return Integer is
      begin
         return YY_END_OF_BUFFER + state + 1;
      end YY_STATE_EOF;

      --  return all but the first 'n' matched characters back to the input stream
      procedure yyless (n : Integer) is
      begin
         yy_ch_buf (yy_cp) := yy_hold_char; --  undo effects of setting up yytext
         yy_cp := yy_bp + n;
         yy_c_buf_p := yy_cp;
         YY_DO_BEFORE_ACTION; -- set up yytext again
      end yyless;

      --  yy_get_previous_state - get the state just before the EOB char was reached

      function yy_get_previous_state return yy_state_type is
         yy_current_state : yy_state_type;
         yy_c : Short;
      begin
         yy_current_state := yy_start;

         for yy_cp in yytext_ptr .. yy_c_buf_p - 1 loop
            yy_c := yy_ec (yy_ch_buf (yy_cp));
            if yy_accept (yy_current_state) /= 0 then
               yy_last_accepting_state := yy_current_state;
               yy_last_accepting_cpos := yy_cp;
            end if;
            while yy_chk (yy_base (yy_current_state) + yy_c) /= yy_current_state loop
               yy_current_state := yy_def (yy_current_state);
               if yy_current_state >= 128 then
                  yy_c := yy_meta (yy_c);
               end if;
            end loop;
            yy_current_state := yy_nxt (yy_base (yy_current_state) + yy_c);
         end loop;

         return yy_current_state;
      end yy_get_previous_state;

      procedure yyrestart (input_file : File_Type) is
      begin
         Open_Input (Ada.Text_IO.Name (input_file));
      end yyrestart;

   begin -- of YYLex
      <<new_file>>
      --  this is where we enter upon encountering an end-of-file and
      --  yyWrap () indicating that we should continue processing

      if yy_init then
         if yy_start = 0 then
            yy_start := 1;      -- first start state
         end if;

         --  we put in the '\n' and start reading from [1] so that an
         --  initial match-at-newline will be true.

         yy_ch_buf (0) := ASCII.LF;
         yy_n_chars := 1;

         --  we always need two end-of-buffer characters. The first causes
         --  a transition to the end-of-buffer state. The second causes
         --  a jam in that state.

         yy_ch_buf (yy_n_chars) := YY_END_OF_BUFFER_CHAR;
         yy_ch_buf (yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

         yy_eof_has_been_seen := False;

         yytext_ptr := 1;
         yy_c_buf_p := yytext_ptr;
         yy_hold_char := yy_ch_buf (yy_c_buf_p);
         yy_init := False;
      end if; -- yy_init

      loop                -- loops until end-of-file is reached

         yy_cp := yy_c_buf_p;

         --  support of yytext
         yy_ch_buf (yy_cp) := yy_hold_char;

         --  yy_bp points to the position in yy_ch_buf of the start of the
         --  current run.
         yy_bp := yy_cp;
         yy_current_state := yy_start;
         loop
               yy_c := yy_ec (yy_ch_buf (yy_cp));
               if yy_accept (yy_current_state) /= 0 then
                  yy_last_accepting_state := yy_current_state;
                  yy_last_accepting_cpos := yy_cp;
               end if;
               while yy_chk (yy_base (yy_current_state) + yy_c) /= yy_current_state loop
                  yy_current_state := yy_def (yy_current_state);
                  if yy_current_state >= 128 then
                     yy_c := yy_meta (yy_c);
                  end if;
               end loop;
               yy_current_state := yy_nxt (yy_base (yy_current_state) + yy_c);
            yy_cp := yy_cp + 1;
            if yy_current_state = 127 then
                exit;
            end if;
         end loop;
         yy_cp := yy_last_accepting_cpos;
         yy_current_state := yy_last_accepting_state;

   <<next_action>>
         yy_act := yy_accept (yy_current_state);
         YY_DO_BEFORE_ACTION;

         if aflex_debug then  -- output acceptance info. for (-d) debug mode
            Ada.Text_IO.Put (Standard_Error, "  -- Aflex.YYLex accept rule #");
            Ada.Text_IO.Put (Standard_Error, Integer'Image (yy_act));
            Ada.Text_IO.Put_Line (Standard_Error, "(""" & YYText & """)");
         end if;

   <<do_action>>   -- this label is used only to access EOF actions
         case yy_act is

            when 0 => -- must backtrack
            -- undo the effects of YY_DO_BEFORE_ACTION
            yy_ch_buf (yy_cp) := yy_hold_char;
            yy_cp := yy_last_accepting_cpos;
            yy_current_state := yy_last_accepting_state;
            goto next_action;



         when 1 =>
--# line 4 "mat-expressions-lexer.l"
             null; 

         when 2 =>
--# line 6 "mat-expressions-lexer.l"
            return T_WITH;

         when 3 =>
--# line 7 "mat-expressions-lexer.l"
            return T_OR;

         when 4 =>
--# line 8 "mat-expressions-lexer.l"
            return T_AND;

         when 5 =>
--# line 9 "mat-expressions-lexer.l"
            return T_NOT;

         when 6 =>
--# line 10 "mat-expressions-lexer.l"
            return T_AFTER;

         when 7 =>
--# line 11 "mat-expressions-lexer.l"
            return T_BEFORE;

         when 8 =>
--# line 12 "mat-expressions-lexer.l"
            return T_WITHIN;

         when 9 =>
--# line 13 "mat-expressions-lexer.l"
            return T_REALLOCATION;

         when 10 =>
--# line 14 "mat-expressions-lexer.l"
            return T_THREAD;

         when 11 =>
--# line 15 "mat-expressions-lexer.l"
            return T_MALLOC;

         when 12 =>
--# line 16 "mat-expressions-lexer.l"
            return T_FREE;

         when 13 =>
--# line 17 "mat-expressions-lexer.l"
            return T_REALLOC;

         when 14 =>
--# line 18 "mat-expressions-lexer.l"
            return T_LEAK;

         when 15 =>
--# line 19 "mat-expressions-lexer.l"
            return T_NO_FREE;

         when 16 =>
--# line 20 "mat-expressions-lexer.l"
            return T_BY;

         when 17 =>
--# line 21 "mat-expressions-lexer.l"
            return T_AT;

         when 18 =>
--# line 22 "mat-expressions-lexer.l"
            return T_IN;

         when 19 =>
--# line 23 "mat-expressions-lexer.l"
            return T_SIZE;

         when 20 =>
--# line 24 "mat-expressions-lexer.l"
            return T_ADDR;

         when 21 =>
--# line 25 "mat-expressions-lexer.l"
            return T_EVENT;

         when 22 =>
--# line 26 "mat-expressions-lexer.l"
            return T_ALL;

         when 23 =>
--# line 27 "mat-expressions-lexer.l"
            return T_IS;

         when 24 =>
--# line 28 "mat-expressions-lexer.l"
            return T_FROM;

         when 25 =>
--# line 29 "mat-expressions-lexer.l"
            return T_TO;

         when 26 =>
--# line 30 "mat-expressions-lexer.l"
            return T_DIRECT;

         when 27 =>
--# line 31 "mat-expressions-lexer.l"
            return T_HAS;

         when 28 =>
--# line 32 "mat-expressions-lexer.l"
               Line_Number := Line_Number + 1;  

         when 29 =>
--# line 33 "mat-expressions-lexer.l"
            return '(';

         when 30 =>
--# line 34 "mat-expressions-lexer.l"
            return ')';

         when 31 =>
--# line 35 "mat-expressions-lexer.l"
            return '[';

         when 32 =>
--# line 36 "mat-expressions-lexer.l"
            return ']';

         when 33 =>
--# line 37 "mat-expressions-lexer.l"
            return ',';

         when 34 =>
--# line 38 "mat-expressions-lexer.l"
            return T_RANGE;

         when 35 =>
--# line 39 "mat-expressions-lexer.l"
            return T_LT;

         when 36 =>
--# line 40 "mat-expressions-lexer.l"
            return T_LE;

         when 37 =>
--# line 41 "mat-expressions-lexer.l"
            return T_GT;

         when 38 =>
--# line 42 "mat-expressions-lexer.l"
            return T_GE;

         when 39 =>
--# line 43 "mat-expressions-lexer.l"
            return T_EQ;

         when 40 =>
--# line 45 "mat-expressions-lexer.l"
              yylval.low := MAT.Types.Hex_Value (YYText (YYText'First + 2 .. YYText'Last));
		           return T_INT;
		        

         when 41 =>
--# line 48 "mat-expressions-lexer.l"
            
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_INT;


         when 42 =>
--# line 52 "mat-expressions-lexer.l"
            
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_INT;
		

         when 43 =>
--# line 56 "mat-expressions-lexer.l"
            
                   yylval.low := MAT.Types.Uint64 (MAT.Types.Tick_Value (YYText));
		           return T_TIME;
		

         when 44 =>
--# line 60 "mat-expressions-lexer.l"
            
		  return T_STRING;
		

         when 45 =>
--# line 63 "mat-expressions-lexer.l"
            
		  return T_NAME;
		

         when 46 =>
--# line 66 "mat-expressions-lexer.l"
             Line_Number := Line_Number + 1;  

         when 47 =>
--# line 67 "mat-expressions-lexer.l"
              null;   

         when 48 =>
--# line 68 "mat-expressions-lexer.l"
            raise AFLEX_SCANNER_JAMMED;
         when YY_END_OF_BUFFER + INITIAL + 1 =>
            return End_Of_Input;

         when YY_END_OF_BUFFER =>
            --  undo the effects of YY_DO_BEFORE_ACTION
            yy_ch_buf (yy_cp) := yy_hold_char;

            yytext_ptr := yy_bp;

            case yy_get_next_buffer is
               when EOB_ACT_END_OF_FILE =>
                  if yyWrap then
                     --  note: because we've taken care in
                     --  yy_get_next_buffer() to have set up yytext,
                     --  we can now set up yy_c_buf_p so that if some
                     --  total hoser (like aflex itself) wants
                     --  to call the scanner after we return the
                     --  End_Of_Input, it'll still work - another
                     --  End_Of_Input will get returned.

                     yy_c_buf_p := yytext_ptr;

                     yy_act := YY_STATE_EOF ((yy_start - 1) / 2);

                     goto do_action;
                  else
                     --  start processing a new file
                     yy_init := True;
                     goto new_file;
                  end if;

               when EOB_ACT_RESTART_SCAN =>
                  yy_c_buf_p := yytext_ptr;
                  yy_hold_char := yy_ch_buf (yy_c_buf_p);

               when EOB_ACT_LAST_MATCH =>
                  yy_c_buf_p := yy_n_chars;
                  yy_current_state := yy_get_previous_state;
                  yy_cp := yy_c_buf_p;
                  yy_bp := yytext_ptr;
                  goto next_action;
            end case; --  case yy_get_next_buffer()

         when others =>
            Ada.Text_IO.Put ("action # ");
            Ada.Text_IO.Put (Integer'Image (yy_act));
            Ada.Text_IO.New_Line;
            raise AFLEX_INTERNAL_ERROR;
         end case; --  case (yy_act)
      end loop; --  end of loop waiting for end of file
   end YYLex;

--# line 68 "mat-expressions-lexer.l"
   pragma Style_Checks (On);

end MAT.Expressions.Lexer;



