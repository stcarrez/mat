
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
      YY_END_OF_BUFFER : constant := 47;
      subtype yy_state_type is Integer;
      yy_current_state : yy_state_type;
INITIAL : constant := 0;
      yy_accept : constant array (0 .. 124) of Short :=
          (0,
        0,    0,   47,   45,    1,   46,   42,   43,   45,   28,
       29,   32,   45,   41,   41,   34,   38,   36,   30,   45,
       31,   43,   43,   43,   43,   43,   43,   43,   43,   43,
       43,   43,   43,   43,   43,   43,    1,   43,    0,   33,
       41,    0,   35,   37,   27,   40,   43,   43,   43,   43,
       17,   43,   16,   43,   43,   43,   43,   18,   23,   43,
       43,   43,    3,   43,   43,   43,   43,   44,   39,   43,
       43,   22,    4,   43,   43,   43,   43,   43,   26,   43,
       43,   43,    5,   43,   43,   43,   43,   20,   43,   43,
       43,   43,   12,   24,   14,   43,   43,   43,   19,   43,

        2,    6,   43,   43,   21,   43,   43,   43,   43,   43,
        7,   25,   11,   15,   43,   10,    8,   13,   43,   43,
       43,   43,    9,    0
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
       40,   41,    1,    1,    1,    1,    1, others => 1

       );

      yy_meta : constant array (0 .. 41) of Short :=
          (0,
        1,    1,    1,    1,    2,    1,    1,    1,    1,    2,
        3,    3,    3,    1,    1,    1,    3,    1,    1,    1,
        3,    3,    3,    3,    3,    3,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
        2
       );

      yy_base : constant array (0 .. 126) of Short :=
          (0,
        0,    0,  142,  143,  139,  143,  143,    0,  137,  143,
      143,  143,  129,   31,   34,  123,  143,  122,  143,   45,
      143,   28,   25,  108,   98,  100,  112,   27,  107,  110,
       97,   95,  103,   99,   99,   97,  122,    0,  117,  143,
       55,    0,  143,  143,  143,   60,   98,   85,   90,   95,
        0,   92,    0,   83,   91,   28,   80,    0,    0,   93,
       83,   37,    0,   91,   70,   76,   73,  143,    0,   74,
       82,    0,    0,   73,   80,   72,   78,   71,    0,   72,
       70,   65,    0,   68,   72,   71,   68,    0,   60,   59,
       69,   55,    0,    0,    0,   57,   64,   58,    0,   66,

       58,    0,   60,   48,    0,   60,   57,   48,   56,   47,
        0,    0,    0,    0,   55,    0,    0,   56,   40,   41,
       22,   19,    0,  143,   72,   46
       );

      yy_def : constant array (0 .. 126) of Short :=
          (0,
      124,    1,  124,  124,  124,  124,  124,  125,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  125,  125,  125,  125,  125,  125,  125,  125,  125,
      125,  125,  125,  125,  125,  125,  124,  125,  124,  124,
      124,  126,  124,  124,  124,  124,  125,  125,  125,  125,
      125,  125,  125,  125,  125,  125,  125,  125,  125,  125,
      125,  125,  125,  125,  125,  125,  125,  124,  126,  125,
      125,  125,  125,  125,  125,  125,  125,  125,  125,  125,
      125,  125,  125,  125,  125,  125,  125,  125,  125,  125,
      125,  125,  125,  125,  125,  125,  125,  125,  125,  125,

      125,  125,  125,  125,  125,  125,  125,  125,  125,  125,
      125,  125,  125,  125,  125,  125,  125,  125,  125,  125,
      125,  125,  125,    0,  124,  124
       );

      yy_nxt : constant array (0 .. 184) of Short :=
          (0,
        4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
       14,   15,   15,   16,   17,   18,    8,   19,   20,   21,
       22,   23,    8,   24,   25,   26,   27,   28,    8,   29,
       30,   31,   32,   33,   34,   35,    8,   36,    8,    8,
        8,   41,   41,   41,   41,   41,   41,   45,   69,   52,
      123,   47,   77,   48,  122,   46,   46,   49,   58,   50,
       78,   59,   82,   51,   53,   41,   41,   41,  121,   42,
       46,   46,   83,   38,   38,  120,  119,  118,  117,  116,
      115,  114,  113,  112,  111,  110,  109,  108,  107,  106,
      105,  104,  103,  102,  101,  100,   99,   98,   97,   96,

       95,   94,   93,   92,   91,   90,   89,   88,   87,   86,
       85,   84,   81,   80,   79,   76,   75,   74,   73,   72,
       71,   70,   68,   37,   67,   66,   65,   64,   63,   62,
       61,   60,   57,   56,   55,   54,   44,   43,   40,   39,
       37,  124,    3,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124
       );

      yy_chk : constant array (0 .. 184) of Short :=
          (0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,   14,   14,   14,   15,   15,   15,   20,  126,   23,
      122,   22,   56,   22,  121,   20,   20,   22,   28,   22,
       56,   28,   62,   22,   23,   41,   41,   41,  120,   14,
       46,   46,   62,  125,  125,  119,  118,  115,  110,  109,
      108,  107,  106,  104,  103,  101,  100,   98,   97,   96,
       92,   91,   90,   89,   87,   86,   85,   84,   82,   81,

       80,   78,   77,   76,   75,   74,   71,   70,   67,   66,
       65,   64,   61,   60,   57,   55,   54,   52,   50,   49,
       48,   47,   39,   37,   36,   35,   34,   33,   32,   31,
       30,   29,   27,   26,   25,   24,   18,   16,   13,    9,
        5,    3,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124
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
               if yy_current_state >= 125 then
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
                  if yy_current_state >= 125 then
                     yy_c := yy_meta (yy_c);
                  end if;
               end loop;
               yy_current_state := yy_nxt (yy_base (yy_current_state) + yy_c);
            yy_cp := yy_cp + 1;
            if yy_current_state = 124 then
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
            return T_DIRECT;

         when 26 => 
--# line 30 "mat-expressions-lexer.l"
            return T_HAS;

         when 27 => 
--# line 31 "mat-expressions-lexer.l"
               Line_Number := Line_Number + 1;  

         when 28 => 
--# line 32 "mat-expressions-lexer.l"
            return '(';

         when 29 => 
--# line 33 "mat-expressions-lexer.l"
            return ')';

         when 30 => 
--# line 34 "mat-expressions-lexer.l"
            return '[';

         when 31 => 
--# line 35 "mat-expressions-lexer.l"
            return ']';

         when 32 => 
--# line 36 "mat-expressions-lexer.l"
            return ',';

         when 33 => 
--# line 37 "mat-expressions-lexer.l"
            return T_RANGE;

         when 34 => 
--# line 38 "mat-expressions-lexer.l"
            return T_LT;

         when 35 => 
--# line 39 "mat-expressions-lexer.l"
            return T_LE;

         when 36 => 
--# line 40 "mat-expressions-lexer.l"
            return T_GT;

         when 37 => 
--# line 41 "mat-expressions-lexer.l"
            return T_GE;

         when 38 => 
--# line 42 "mat-expressions-lexer.l"
            return T_EQ;

         when 39 => 
--# line 44 "mat-expressions-lexer.l"
              yylval.low := MAT.Types.Hex_Value (YYText (YYText'First + 2 .. YYText'Last));
		           return T_INT;
		        

         when 40 => 
--# line 47 "mat-expressions-lexer.l"
            
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_INT;


         when 41 => 
--# line 51 "mat-expressions-lexer.l"
            
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_INT;
		

         when 42 => 
--# line 55 "mat-expressions-lexer.l"
            
		  return T_STRING;
		

         when 43 => 
--# line 58 "mat-expressions-lexer.l"
            
		  return T_NAME;
		

         when 44 => 
--# line 61 "mat-expressions-lexer.l"
             Line_Number := Line_Number + 1;  

         when 45 => 
--# line 62 "mat-expressions-lexer.l"
              null;   

         when 46 => 
--# line 63 "mat-expressions-lexer.l"
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
--# line 63 "mat-expressions-lexer.l"
   pragma Style_Checks (On);

end MAT.Expressions.Lexer;



