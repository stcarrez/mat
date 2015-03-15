
with MAT.Types;
with MAT.Expressions;
with Ada.Text_IO;
with MAT.Expressions.Lexer_dfa;
with MAT.Expressions.Parser_io;

pragma Style_Checks (Off);
package body MAT.Expressions.Lexer is

   use Ada.Text_IO;
   use Ada;
   use MAT.Expressions.Lexer_dfa;
   use MAT.Expressions.Parser_io;

   lval : MAT.Types.Uint64;
   Line_Number : Natural := 0;
 
   function YYLex return Token is
      subtype Short is Integer range -32768 .. 32767;
      yy_act : Integer;
      yy_c   : Short;

      --  returned upon end-of-file
      YY_END_TOK : constant Integer := 0;
      YY_END_OF_BUFFER : constant := 34;
      subtype yy_state_type is Integer;
      yy_current_state : yy_state_type;
INITIAL : constant := 0;
      yy_accept : constant array (0 .. 89) of Short :=
          (0,
        0,    0,   34,   32,    1,   33,   29,   30,   32,   20,
       21,   24,   30,   28,   28,   22,   32,   23,   30,   30,
       30,   30,   30,   30,   30,   30,   30,   30,    1,   30,
        0,   25,   28,    0,   19,   27,   30,   30,   30,   30,
       11,   30,   10,   30,   30,   12,   16,   30,    3,   30,
       30,   30,   31,   26,   30,   30,   15,    4,   30,   30,
       30,    5,   30,   30,   30,   14,   30,   30,   30,   17,
       30,   13,    2,    6,   30,   30,   30,   30,    7,   18,
       30,    8,   30,   30,   30,   30,   30,    9,    0
       );

      yy_ec : constant array (ASCII.NUL .. Character'Last) of Short := (0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    1,    4,    1,    5,    1,    1,    6,    7,
        8,    1,    1,    9,    1,   10,    1,   11,   12,   12,
       12,   12,   12,   12,   12,   13,   13,    1,    1,    1,
        1,    1,    1,    1,   14,   14,   14,   14,   14,   14,
        5,    5,    5,    5,    5,    5,    5,    5,    5,    5,
        5,    5,    5,    5,    5,    5,    5,    5,    5,    5,
       15,   16,   17,    1,    5,    1,   18,   19,   20,   21,

       22,   23,    5,   24,   25,    5,    5,   26,   27,   28,
       29,    5,    5,   30,   31,   32,    5,    5,   33,   34,
       35,   36,    1,    1,    1,    1,    1, others => 1

       );

      yy_meta : constant array (0 .. 36) of Short :=
          (0,
        1,    1,    1,    1,    2,    1,    1,    1,    1,    2,
        3,    3,    3,    3,    1,    1,    1,    3,    3,    3,
        3,    3,    3,    2,    2,    2,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2
       );

      yy_base : constant array (0 .. 91) of Short :=
          (0,
        0,    0,  110,  111,  107,  111,  111,    0,  105,  111,
      111,  111,   97,   26,   29,  111,   40,  111,   27,   22,
       81,   75,   18,   75,   73,   80,   76,   75,   97,    0,
       92,    0,   50,    0,  111,   53,   76,   64,   69,   73,
        0,   70,    0,   62,   62,    0,    0,   58,    0,   71,
       52,   55,  111,    0,   56,   63,    0,    0,   55,   61,
       55,    0,   55,   58,   55,    0,   48,   47,   56,    0,
       49,    0,   49,    0,   51,   40,   42,   42,    0,    0,
       49,    0,   50,   26,   31,   25,   19,    0,  111,   64,
       42

       );

      yy_def : constant array (0 .. 91) of Short :=
          (0,
       89,    1,   89,   89,   89,   89,   89,   90,   89,   89,
       89,   89,   90,   89,   89,   89,   89,   89,   90,   90,
       90,   90,   90,   90,   90,   90,   90,   90,   89,   90,
       89,   90,   89,   91,   89,   89,   90,   90,   90,   90,
       90,   90,   90,   90,   90,   90,   90,   90,   90,   90,
       90,   90,   89,   91,   90,   90,   90,   90,   90,   90,
       90,   90,   90,   90,   90,   90,   90,   90,   90,   90,
       90,   90,   90,   90,   90,   90,   90,   90,   90,   90,
       90,   90,   90,   90,   90,   90,   90,   90,    0,   89,
       89

       );

      yy_nxt : constant array (0 .. 147) of Short :=
          (0,
        4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
       14,   15,   15,    8,   16,   17,   18,   19,   20,    8,
       21,    8,   22,    8,   23,    8,    8,   24,   25,   26,
       27,    8,   28,    8,    8,    8,   33,   33,   33,   33,
       33,   33,   35,   42,   54,   46,   88,   37,   47,   38,
       36,   36,   39,   87,   40,   86,   43,   85,   41,   34,
       33,   33,   33,   36,   36,   30,   30,   84,   83,   82,
       81,   80,   79,   78,   77,   76,   75,   74,   73,   72,
       71,   70,   69,   68,   67,   66,   65,   64,   63,   62,
       61,   60,   59,   58,   57,   56,   55,   53,   29,   52,

       51,   50,   49,   48,   45,   44,   32,   31,   29,   89,
        3,   89,   89,   89,   89,   89,   89,   89,   89,   89,
       89,   89,   89,   89,   89,   89,   89,   89,   89,   89,
       89,   89,   89,   89,   89,   89,   89,   89,   89,   89,
       89,   89,   89,   89,   89,   89,   89
       );

      yy_chk : constant array (0 .. 147) of Short :=
          (0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,   14,   14,   14,   15,
       15,   15,   17,   20,   91,   23,   87,   19,   23,   19,
       17,   17,   19,   86,   19,   85,   20,   84,   19,   14,
       33,   33,   33,   36,   36,   90,   90,   83,   81,   78,
       77,   76,   75,   73,   71,   69,   68,   67,   65,   64,
       63,   61,   60,   59,   56,   55,   52,   51,   50,   48,
       45,   44,   42,   40,   39,   38,   37,   31,   29,   28,

       27,   26,   25,   24,   22,   21,   13,    9,    5,    3,
       89,   89,   89,   89,   89,   89,   89,   89,   89,   89,
       89,   89,   89,   89,   89,   89,   89,   89,   89,   89,
       89,   89,   89,   89,   89,   89,   89,   89,   89,   89,
       89,   89,   89,   89,   89,   89,   89
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
               if yy_current_state >= 90 then
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
                  if yy_current_state >= 90 then
                     yy_c := yy_meta (yy_c);
                  end if;
               end loop;
               yy_current_state := yy_nxt (yy_base (yy_current_state) + yy_c);
            yy_cp := yy_cp + 1;
            if yy_current_state = 89 then
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
            return T_With;

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
            return T_BY;

         when 11 => 
--# line 15 "mat-expressions-lexer.l"
            return T_AT;

         when 12 => 
--# line 16 "mat-expressions-lexer.l"
            return T_IN;

         when 13 => 
--# line 17 "mat-expressions-lexer.l"
            return T_SIZE;

         when 14 => 
--# line 18 "mat-expressions-lexer.l"
            return T_ADDR;

         when 15 => 
--# line 19 "mat-expressions-lexer.l"
            return T_ALL;

         when 16 => 
--# line 20 "mat-expressions-lexer.l"
            return T_IS;

         when 17 => 
--# line 21 "mat-expressions-lexer.l"
            return T_FROM;

         when 18 => 
--# line 22 "mat-expressions-lexer.l"
            return T_DIRECT;

         when 19 => 
--# line 23 "mat-expressions-lexer.l"
               Line_Number := Line_Number + 1;  

         when 20 => 
--# line 24 "mat-expressions-lexer.l"
            return '(';

         when 21 => 
--# line 25 "mat-expressions-lexer.l"
            return ')';

         when 22 => 
--# line 26 "mat-expressions-lexer.l"
            return '[';

         when 23 => 
--# line 27 "mat-expressions-lexer.l"
            return ']';

         when 24 => 
--# line 28 "mat-expressions-lexer.l"
            return ',';

         when 25 => 
--# line 29 "mat-expressions-lexer.l"
            return T_RANGE;

         when 26 => 
--# line 31 "mat-expressions-lexer.l"
              yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_Int;
		        

         when 27 => 
--# line 34 "mat-expressions-lexer.l"
            
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_Int;


         when 28 => 
--# line 38 "mat-expressions-lexer.l"
            
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_Int;
		

         when 29 => 
--# line 42 "mat-expressions-lexer.l"
            
		  return T_String;
		

         when 30 => 
--# line 45 "mat-expressions-lexer.l"
            
		  return T_Name;
		

         when 31 => 
--# line 48 "mat-expressions-lexer.l"
             Line_Number := Line_Number + 1;  

         when 32 => 
--# line 49 "mat-expressions-lexer.l"
              null;   

         when 33 => 
--# line 50 "mat-expressions-lexer.l"
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
--# line 50 "mat-expressions-lexer.l"

end MAT.Expressions.Lexer;



