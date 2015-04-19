
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
      yy_act : Integer;
      yy_c   : Short;

      --  returned upon end-of-file
      YY_END_TOK : constant Integer := 0;
      YY_END_OF_BUFFER : constant := 44;
      subtype yy_state_type is Integer;
      yy_current_state : yy_state_type;
INITIAL : constant := 0;
      yy_accept : constant array (0 .. 113) of Short :=
          (0,
        0,    0,   44,   42,    1,   43,   39,   40,   42,   25,
       26,   29,   40,   38,   38,   31,   35,   33,   27,   42,
       28,   40,   40,   40,   40,   40,   40,   40,   40,   40,
       40,   40,   40,   40,    1,   40,    0,   30,   38,    0,
       32,   34,   24,   37,   40,   40,   40,   40,   15,   40,
       14,   40,   40,   40,   16,   21,   40,   40,    3,   40,
       40,   40,   40,   41,   36,   40,   40,   20,    4,   40,
       40,   40,   40,   40,   40,    5,   40,   40,   40,   40,
       18,   40,   40,   40,   40,   12,   22,   40,   40,   17,
       40,    2,    6,   40,   40,   19,   40,   40,   40,   40,

        7,   23,   11,   40,   10,    8,   13,   40,   40,   40,
       40,    9,    0
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

       25,   26,    5,   27,   28,    5,    5,   29,   30,   31,
       32,    5,    5,   33,   34,   35,    5,   36,   37,   38,
       39,   40,    1,    1,    1,    1,    1, others => 1

       );

      yy_meta : constant array (0 .. 40) of Short :=
          (0,
        1,    1,    1,    1,    2,    1,    1,    1,    1,    2,
        3,    3,    3,    1,    1,    1,    3,    1,    1,    1,
        3,    3,    3,    3,    3,    3,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,    2,    2,    2
       );

      yy_base : constant array (0 .. 115) of Short :=
          (0,
        0,    0,  132,  133,  129,  133,  133,    0,  127,  133,
      133,  133,  119,   30,   33,  113,  133,  112,  133,   44,
      133,   28,   23,   98,   89,   91,   19,  102,   90,   88,
       95,   91,   91,   89,  114,    0,  109,    0,   53,    0,
      133,  133,  133,   49,   90,   78,   83,   87,    0,   84,
        0,   76,   83,   26,    0,    0,   78,   71,    0,   84,
       64,   70,   67,  133,    0,   68,   75,    0,    0,   67,
       73,   66,   71,   65,   65,    0,   64,   67,   66,   63,
        0,   56,   55,   64,   51,    0,    0,   53,   55,    0,
       62,   54,    0,   56,   45,    0,   56,   46,   53,   45,

        0,    0,    0,   52,    0,    0,   53,   38,   44,   39,
       36,    0,  133,   67,   46
       );

      yy_def : constant array (0 .. 115) of Short :=
          (0,
      113,    1,  113,  113,  113,  113,  113,  114,  113,  113,
      113,  113,  114,  113,  113,  113,  113,  113,  113,  113,
      113,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  113,  114,  113,  114,  113,  115,
      113,  113,  113,  113,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  113,  115,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,

      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,    0,  113,  113
       );

      yy_nxt : constant array (0 .. 173) of Short :=
          (0,
        4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
       14,   15,   15,   16,   17,   18,    8,   19,   20,   21,
       22,   23,    8,   24,   25,   26,    8,   27,    8,   28,
       29,   30,   31,   32,   33,    8,   34,    8,    8,    8,
       39,   39,   39,   39,   39,   39,   43,   50,   65,   55,
       73,   45,   56,   46,   44,   44,   47,   74,   48,   44,
       44,   51,   49,   39,   39,   39,  112,   40,   36,   36,
      111,  110,  109,  108,  107,  106,  105,  104,  103,  102,
      101,  100,   99,   98,   97,   96,   95,   94,   93,   92,
       91,   90,   89,   88,   87,   86,   85,   84,   83,   82,

       81,   80,   79,   78,   77,   76,   75,   72,   71,   70,
       69,   68,   67,   66,   64,   35,   63,   62,   61,   60,
       59,   58,   57,   54,   53,   52,   42,   41,   38,   37,
       35,  113,    3,  113,  113,  113,  113,  113,  113,  113,
      113,  113,  113,  113,  113,  113,  113,  113,  113,  113,
      113,  113,  113,  113,  113,  113,  113,  113,  113,  113,
      113,  113,  113,  113,  113,  113,  113,  113,  113,  113,
      113,  113,  113
       );

      yy_chk : constant array (0 .. 173) of Short :=
          (0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
       14,   14,   14,   15,   15,   15,   20,   23,  115,   27,
       54,   22,   27,   22,   20,   20,   22,   54,   22,   44,
       44,   23,   22,   39,   39,   39,  111,   14,  114,  114,
      110,  109,  108,  107,  104,  100,   99,   98,   97,   95,
       94,   92,   91,   89,   88,   85,   84,   83,   82,   80,
       79,   78,   77,   75,   74,   73,   72,   71,   70,   67,

       66,   63,   62,   61,   60,   58,   57,   53,   52,   50,
       48,   47,   46,   45,   37,   35,   34,   33,   32,   31,
       30,   29,   28,   26,   25,   24,   18,   16,   13,    9,
        5,    3,  113,  113,  113,  113,  113,  113,  113,  113,
      113,  113,  113,  113,  113,  113,  113,  113,  113,  113,
      113,  113,  113,  113,  113,  113,  113,  113,  113,  113,
      113,  113,  113,  113,  113,  113,  113,  113,  113,  113,
      113,  113,  113
       );


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

      --  redefine this if you have something you want each time.
      procedure YY_USER_ACTION is
      begin
         null;
      end YY_USER_ACTION;

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
               if yy_current_state >= 114 then
                  yy_c := yy_meta (yy_c);
               end if;
            end loop;
            yy_current_state := yy_nxt (yy_base (yy_current_state) + yy_c);
         end loop;

         return yy_current_state;
      end yy_get_previous_state;

      procedure yyrestart (input_file : File_Type) is
      begin
         Open_Input (Text_IO.Name (input_file));
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
                  if yy_current_state >= 114 then
                     yy_c := yy_meta (yy_c);
                  end if;
               end loop;
               yy_current_state := yy_nxt (yy_base (yy_current_state) + yy_c);
            yy_cp := yy_cp + 1;
            if yy_current_state = 113 then
                exit;
            end if;
         end loop;
         yy_cp := yy_last_accepting_cpos;
         yy_current_state := yy_last_accepting_state;

   <<next_action>>
         yy_act := yy_accept (yy_current_state);
         YY_DO_BEFORE_ACTION;
         YY_USER_ACTION;

         if aflex_debug then  -- output acceptance info. for (-d) debug mode
            Text_IO.Put (Standard_Error, "--accepting rule #");
            Text_IO.Put (Standard_Error, Integer'Image (yy_act));
            Text_IO.Put_Line (Standard_Error, "(""" & YYText & """)");
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
            return T_BY;

         when 15 => 
--# line 19 "mat-expressions-lexer.l"
            return T_AT;

         when 16 => 
--# line 20 "mat-expressions-lexer.l"
            return T_IN;

         when 17 => 
--# line 21 "mat-expressions-lexer.l"
            return T_SIZE;

         when 18 => 
--# line 22 "mat-expressions-lexer.l"
            return T_ADDR;

         when 19 => 
--# line 23 "mat-expressions-lexer.l"
            return T_EVENT;

         when 20 => 
--# line 24 "mat-expressions-lexer.l"
            return T_ALL;

         when 21 => 
--# line 25 "mat-expressions-lexer.l"
            return T_IS;

         when 22 => 
--# line 26 "mat-expressions-lexer.l"
            return T_FROM;

         when 23 => 
--# line 27 "mat-expressions-lexer.l"
            return T_DIRECT;

         when 24 => 
--# line 28 "mat-expressions-lexer.l"
               Line_Number := Line_Number + 1;  

         when 25 => 
--# line 29 "mat-expressions-lexer.l"
            return '(';

         when 26 => 
--# line 30 "mat-expressions-lexer.l"
            return ')';

         when 27 => 
--# line 31 "mat-expressions-lexer.l"
            return '[';

         when 28 => 
--# line 32 "mat-expressions-lexer.l"
            return ']';

         when 29 => 
--# line 33 "mat-expressions-lexer.l"
            return ',';

         when 30 => 
--# line 34 "mat-expressions-lexer.l"
            return T_RANGE;

         when 31 => 
--# line 35 "mat-expressions-lexer.l"
            return T_LT;

         when 32 => 
--# line 36 "mat-expressions-lexer.l"
            return T_LE;

         when 33 => 
--# line 37 "mat-expressions-lexer.l"
            return T_GT;

         when 34 => 
--# line 38 "mat-expressions-lexer.l"
            return T_GE;

         when 35 => 
--# line 39 "mat-expressions-lexer.l"
            return T_EQ;

         when 36 => 
--# line 41 "mat-expressions-lexer.l"
              yylval.low := MAT.Types.Hex_Value (YYText (YYText'First + 2 .. YYText'Last));
		           return T_INT;
		        

         when 37 => 
--# line 44 "mat-expressions-lexer.l"
            
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_INT;


         when 38 => 
--# line 48 "mat-expressions-lexer.l"
            
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_INT;
		

         when 39 => 
--# line 52 "mat-expressions-lexer.l"
            
		  return T_STRING;
		

         when 40 => 
--# line 55 "mat-expressions-lexer.l"
            
		  return T_NAME;
		

         when 41 => 
--# line 58 "mat-expressions-lexer.l"
             Line_Number := Line_Number + 1;  

         when 42 => 
--# line 59 "mat-expressions-lexer.l"
              null;   

         when 43 => 
--# line 60 "mat-expressions-lexer.l"
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
                  when others =>
                     null;
               end case; --  case yy_get_next_buffer()

            when others =>
               Text_IO.Put ("action # ");
               Text_IO.Put (Integer'Image (yy_act));
               Text_IO.New_Line;
               raise AFLEX_INTERNAL_ERROR;
         end case; --  case (yy_act)
      end loop; --  end of loop waiting for end of file
   end YYLex;
--# line 60 "mat-expressions-lexer.l"
   pragma Style_Checks (On);

end MAT.Expressions.Lexer;



