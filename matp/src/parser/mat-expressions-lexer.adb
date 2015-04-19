
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
      YY_END_OF_BUFFER : constant := 45;
      subtype yy_state_type is Integer;
      yy_current_state : yy_state_type;
INITIAL : constant := 0;
      yy_accept : constant array (0 .. 116) of Short :=
          (0,
        0,    0,   45,   43,    1,   44,   40,   41,   43,   26,
       27,   30,   41,   39,   39,   32,   36,   34,   28,   43,
       29,   41,   41,   41,   41,   41,   41,   41,   41,   41,
       41,   41,   41,   41,   41,    1,   41,    0,   31,   39,
        0,   33,   35,   25,   38,   41,   41,   41,   41,   15,
       41,   14,   41,   41,   41,   41,   16,   21,   41,   41,
        3,   41,   41,   41,   41,   42,   37,   41,   41,   20,
        4,   41,   41,   41,   41,   41,   24,   41,    5,   41,
       41,   41,   41,   18,   41,   41,   41,   41,   12,   22,
       41,   41,   17,   41,    2,    6,   41,   41,   19,   41,

       41,   41,   41,    7,   23,   11,   41,   10,    8,   13,
       41,   41,   41,   41,    9,    0
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

      yy_base : constant array (0 .. 118) of Short :=
          (0,
        0,    0,  134,  135,  131,  135,  135,    0,  129,  135,
      135,  135,  121,   30,   33,  115,  135,  114,  135,   44,
      135,   28,   23,  100,   91,   93,  104,   19,  103,   91,
       89,   96,   92,   92,   90,  115,    0,  110,    0,   53,
        0,  135,  135,  135,   49,   91,   79,   84,   88,    0,
       85,    0,   77,   84,   26,   74,    0,    0,   78,   71,
        0,   84,   64,   70,   67,  135,    0,   68,   75,    0,
        0,   67,   73,   66,   71,   65,    0,   65,    0,   64,
       67,   66,   63,    0,   56,   55,   64,   51,    0,    0,
       53,   55,    0,   62,   54,    0,   56,   45,    0,   56,

       46,   53,   45,    0,    0,    0,   52,    0,    0,   53,
       38,   44,   39,   36,    0,  135,   67,   46
       );

      yy_def : constant array (0 .. 118) of Short :=
          (0,
      116,    1,  116,  116,  116,  116,  116,  117,  116,  116,
      116,  116,  117,  116,  116,  116,  116,  116,  116,  116,
      116,  117,  117,  117,  117,  117,  117,  117,  117,  117,
      117,  117,  117,  117,  117,  116,  117,  116,  117,  116,
      118,  116,  116,  116,  116,  117,  117,  117,  117,  117,
      117,  117,  117,  117,  117,  117,  117,  117,  117,  117,
      117,  117,  117,  117,  117,  116,  118,  117,  117,  117,
      117,  117,  117,  117,  117,  117,  117,  117,  117,  117,
      117,  117,  117,  117,  117,  117,  117,  117,  117,  117,
      117,  117,  117,  117,  117,  117,  117,  117,  117,  117,

      117,  117,  117,  117,  117,  117,  117,  117,  117,  117,
      117,  117,  117,  117,  117,    0,  116,  116
       );

      yy_nxt : constant array (0 .. 175) of Short :=
          (0,
        4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
       14,   15,   15,   16,   17,   18,    8,   19,   20,   21,
       22,   23,    8,   24,   25,   26,   27,   28,    8,   29,
       30,   31,   32,   33,   34,    8,   35,    8,    8,    8,
       40,   40,   40,   40,   40,   40,   44,   51,   67,   57,
       75,   46,   58,   47,   45,   45,   48,   76,   49,   45,
       45,   52,   50,   40,   40,   40,  115,   41,   37,   37,
      114,  113,  112,  111,  110,  109,  108,  107,  106,  105,
      104,  103,  102,  101,  100,   99,   98,   97,   96,   95,
       94,   93,   92,   91,   90,   89,   88,   87,   86,   85,

       84,   83,   82,   81,   80,   79,   78,   77,   74,   73,
       72,   71,   70,   69,   68,   66,   36,   65,   64,   63,
       62,   61,   60,   59,   56,   55,   54,   53,   43,   42,
       39,   38,   36,  116,    3,  116,  116,  116,  116,  116,
      116,  116,  116,  116,  116,  116,  116,  116,  116,  116,
      116,  116,  116,  116,  116,  116,  116,  116,  116,  116,
      116,  116,  116,  116,  116,  116,  116,  116,  116,  116,
      116,  116,  116,  116,  116
       );

      yy_chk : constant array (0 .. 175) of Short :=
          (0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
       14,   14,   14,   15,   15,   15,   20,   23,  118,   28,
       55,   22,   28,   22,   20,   20,   22,   55,   22,   45,
       45,   23,   22,   40,   40,   40,  114,   14,  117,  117,
      113,  112,  111,  110,  107,  103,  102,  101,  100,   98,
       97,   95,   94,   92,   91,   88,   87,   86,   85,   83,
       82,   81,   80,   78,   76,   75,   74,   73,   72,   69,

       68,   65,   64,   63,   62,   60,   59,   56,   54,   53,
       51,   49,   48,   47,   46,   38,   36,   35,   34,   33,
       32,   31,   30,   29,   27,   26,   25,   24,   18,   16,
       13,    9,    5,    3,  116,  116,  116,  116,  116,  116,
      116,  116,  116,  116,  116,  116,  116,  116,  116,  116,
      116,  116,  116,  116,  116,  116,  116,  116,  116,  116,
      116,  116,  116,  116,  116,  116,  116,  116,  116,  116,
      116,  116,  116,  116,  116
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
               if yy_current_state >= 117 then
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
                  if yy_current_state >= 117 then
                     yy_c := yy_meta (yy_c);
                  end if;
               end loop;
               yy_current_state := yy_nxt (yy_base (yy_current_state) + yy_c);
            yy_cp := yy_cp + 1;
            if yy_current_state = 116 then
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
            return T_HAS;

         when 25 => 
--# line 29 "mat-expressions-lexer.l"
               Line_Number := Line_Number + 1;  

         when 26 => 
--# line 30 "mat-expressions-lexer.l"
            return '(';

         when 27 => 
--# line 31 "mat-expressions-lexer.l"
            return ')';

         when 28 => 
--# line 32 "mat-expressions-lexer.l"
            return '[';

         when 29 => 
--# line 33 "mat-expressions-lexer.l"
            return ']';

         when 30 => 
--# line 34 "mat-expressions-lexer.l"
            return ',';

         when 31 => 
--# line 35 "mat-expressions-lexer.l"
            return T_RANGE;

         when 32 => 
--# line 36 "mat-expressions-lexer.l"
            return T_LT;

         when 33 => 
--# line 37 "mat-expressions-lexer.l"
            return T_LE;

         when 34 => 
--# line 38 "mat-expressions-lexer.l"
            return T_GT;

         when 35 => 
--# line 39 "mat-expressions-lexer.l"
            return T_GE;

         when 36 => 
--# line 40 "mat-expressions-lexer.l"
            return T_EQ;

         when 37 => 
--# line 42 "mat-expressions-lexer.l"
              yylval.low := MAT.Types.Hex_Value (YYText (YYText'First + 2 .. YYText'Last));
		           return T_INT;
		        

         when 38 => 
--# line 45 "mat-expressions-lexer.l"
            
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_INT;


         when 39 => 
--# line 49 "mat-expressions-lexer.l"
            
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_INT;
		

         when 40 => 
--# line 53 "mat-expressions-lexer.l"
            
		  return T_STRING;
		

         when 41 => 
--# line 56 "mat-expressions-lexer.l"
            
		  return T_NAME;
		

         when 42 => 
--# line 59 "mat-expressions-lexer.l"
             Line_Number := Line_Number + 1;  

         when 43 => 
--# line 60 "mat-expressions-lexer.l"
              null;   

         when 44 => 
--# line 61 "mat-expressions-lexer.l"
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
--# line 61 "mat-expressions-lexer.l"
   pragma Style_Checks (On);

end MAT.Expressions.Lexer;



