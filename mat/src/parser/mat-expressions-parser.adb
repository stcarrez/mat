
pragma Style_Checks (Off);
with Interfaces;
with MAT.Expressions.Parser_Goto;
with MAT.Expressions.Parser_Tokens; 
with MAT.Expressions.Parser_Shift_Reduce;
with MAT.Expressions.Parser_IO;
with MAT.Expressions.Lexer;
with MAT.Expressions.Lexer_DFA;
with Ada.Text_IO;
package body MAT.Expressions.Parser is

   use Ada;
   use MAT.Expressions.Lexer;
   use type Interfaces.Unsigned_64;

   procedure yyparse;

   procedure yyerror (Message : in String := "syntax error");

   function To_Event_Id_Type (Value : in MAT.Types.Uint64) return MAT.Events.Event_Id_Type;
   function To_Thread_Ref (Value : in MAT.Types.Uint64) return MAT.Types.Target_Thread_Ref;

   Expr : MAT.Expressions.Expression_Type;

   function To_Event_Id_Type (Value : in MAT.Types.Uint64)
      return MAT.Events.Event_Id_Type is
   begin
      if Value > MAT.Types.Uint64 (MAT.Events.Event_Id_Type'Last) then
         return MAT.Events.Event_Id_Type'Last;
      else
         return MAT.Events.Event_Id_Type (Value);
      end if;
   end To_Event_Id_Type;

   function To_Thread_Ref (Value : in MAT.Types.Uint64)
      return MAT.Types.Target_Thread_Ref is
   begin
      if Value > MAT.Types.Uint64 (MAT.Types.Target_Thread_Ref'Last) then
         return MAT.Types.Target_Thread_Ref'Last;
      else
         return MAT.Types.Target_Thread_Ref (Value);
      end if;
   end To_Thread_Ref;

   procedure yyerror (Message : in String := "syntax error") is
      pragma Unreferenced (Message);
   begin
      error_count := error_count + 1;
   end yyerror;

   function Parse (Content : in String) return MAT.Expressions.Expression_Type is
   begin
      MAT.Expressions.Parser_IO.Set_Input (Content);
      Expr := MAT.Expressions.EMPTY;
      yyparse;
      return Expr;
   end Parse;


   procedure YYParse is
      --  Rename User Defined Packages to Internal Names.
      package yy_goto_tables renames
         MAT.Expressions.Parser_Goto;
      package yy_shift_reduce_tables renames
         MAT.Expressions.Parser_Shift_Reduce;
      package yy_tokens renames
         MAT.Expressions.Parser_Tokens;

      use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

      procedure handle_error;

      subtype goto_row is yy_goto_tables.Row;
      subtype reduce_row is yy_shift_reduce_tables.Row;

      package yy is

         --  the size of the value and state stacks
         --  Affects error 'Stack size exceeded on state_stack'
         stack_size : constant Natural :=  256;

         --  subtype rule         is Natural;
         subtype parse_state is Natural;
         --  subtype nonterminal  is Integer;

         --  encryption constants
         default           : constant := -1;
         first_shift_entry : constant := 0;
         accept_code       : constant := -3001;
         error_code        : constant := -3000;

         --  stack data used by the parser
         tos                : Natural := 0;
         value_stack        : array (0 .. stack_size) of yy_tokens.YYSType;
         state_stack        : array (0 .. stack_size) of parse_state;

         --  current input symbol and action the parser is on
         action             : Integer;
         rule_id            : Rule;
         input_symbol       : yy_tokens.Token := ERROR;

         --  error recovery flag
         error_flag : Natural := 0;
         --  indicates  3 - (number of valid shifts after an error occurs)

         look_ahead : Boolean := True;
         index      : reduce_row;

         --  Is Debugging option on or off
         debug : constant Boolean := False;
      end yy;

      procedure shift_debug (state_id : yy.parse_state; lexeme : yy_tokens.Token);
      procedure reduce_debug (rule_id : Rule; state_id : yy.parse_state);

      function goto_state
         (state : yy.parse_state;
          sym   : Nonterminal) return yy.parse_state;

      function parse_action
         (state : yy.parse_state;
          t     : yy_tokens.Token) return Integer;

      pragma Inline (goto_state, parse_action);

      function goto_state (state : yy.parse_state;
                           sym   : Nonterminal) return yy.parse_state is
         index : goto_row;
      begin
         index := Goto_Offset (state);
         while Goto_Matrix (index).Nonterm /= sym loop
            index := index + 1;
         end loop;
         return Integer (Goto_Matrix (index).Newstate);
      end goto_state;


      function parse_action (state : yy.parse_state;
                             t     : yy_tokens.Token) return Integer is
         index   : reduce_row;
         tok_pos : Integer;
         default : constant Integer := -1;
      begin
         tok_pos := yy_tokens.Token'Pos (t);
         index   := Shift_Reduce_Offset (state);
         while Integer (Shift_Reduce_Matrix (index).T) /= tok_pos
           and then Integer (Shift_Reduce_Matrix (index).T) /= default
         loop
            index := index + 1;
         end loop;
         return Integer (Shift_Reduce_Matrix (index).Act);
      end parse_action;

      --  error recovery stuff

      procedure handle_error is
         temp_action : Integer;
      begin

         if yy.error_flag = 3 then --  no shift yet, clobber input.
            if yy.debug then
               Text_IO.Put_Line ("  -- Ayacc.YYParse: Error Recovery Clobbers "
                                 & yy_tokens.Token'Image (yy.input_symbol));
            end if;
            if yy.input_symbol = yy_tokens.END_OF_INPUT then  -- don't discard,
               if yy.debug then
                  Text_IO.Put_Line ("  -- Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
               end if;
               raise yy_tokens.Syntax_Error;
            end if;

            yy.look_ahead := True;   --  get next token
            return;                  --  and try again...
         end if;

         if yy.error_flag = 0 then --  brand new error
            yyerror ("Syntax Error");
         end if;

         yy.error_flag := 3;

         --  find state on stack where error is a valid shift --

         if yy.debug then
            Text_IO.Put_Line ("  -- Ayacc.YYParse: Looking for state with error as valid shift");
         end if;

         loop
            if yy.debug then
               Text_IO.Put_Line ("  -- Ayacc.YYParse: Examining State "
                                 & yy.parse_state'Image (yy.state_stack (yy.tos)));
            end if;
            temp_action := parse_action (yy.state_stack (yy.tos), ERROR);

            if temp_action >= yy.first_shift_entry then
               if yy.tos = yy.stack_size then
                  Text_IO.Put_Line ("  -- Ayacc.YYParse: Stack size exceeded on state_stack");
                  raise yy_tokens.Syntax_Error;
               end if;
               yy.tos                  := yy.tos + 1;
               yy.state_stack (yy.tos) := temp_action;
               exit;
            end if;

            if yy.tos /= 0 then
               yy.tos := yy.tos - 1;
            end if;

            if yy.tos = 0 then
               if yy.debug then
                  Text_IO.Put_Line
                     ("  -- Ayacc.YYParse: Error recovery popped entire stack, aborting...");
               end if;
               raise yy_tokens.Syntax_Error;
            end if;
         end loop;

         if yy.debug then
            Text_IO.Put_Line ("  -- Ayacc.YYParse: Shifted error token in state "
                              & yy.parse_state'Image (yy.state_stack (yy.tos)));
         end if;

      end handle_error;

      --  print debugging information for a shift operation
      procedure shift_debug (state_id : yy.parse_state; lexeme : yy_tokens.Token) is
      begin
         Text_IO.Put_Line ("  -- Ayacc.YYParse: Shift "
                           & yy.parse_state'Image (state_id) & " on input symbol "
                           & yy_tokens.Token'Image (lexeme));
      end shift_debug;

      --  print debugging information for a reduce operation
      procedure reduce_debug (rule_id : Rule; state_id : yy.parse_state) is
      begin
         Text_IO.Put_Line ("  -- Ayacc.YYParse: Reduce by rule "
                           & Rule'Image (rule_id) & " goto state "
                           & yy.parse_state'Image (state_id));
      end reduce_debug;

   begin
      --  initialize by pushing state 0 and getting the first input symbol
      yy.state_stack (yy.tos) := 0;

      loop
         yy.index := Shift_Reduce_Offset (yy.state_stack (yy.tos));
         if Integer (Shift_Reduce_Matrix (yy.index).T) = yy.default then
            yy.action := Integer (Shift_Reduce_Matrix (yy.index).Act);
         else
            if yy.look_ahead then
               yy.look_ahead := False;
               yy.input_symbol := YYLex;
            end if;
            yy.action := parse_action (yy.state_stack (yy.tos), yy.input_symbol);
         end if;


         if yy.action >= yy.first_shift_entry then  --  SHIFT

            if yy.debug then
               shift_debug (yy.action, yy.input_symbol);
            end if;

            --  Enter new state
            if yy.tos = yy.stack_size then
               Text_IO.Put_Line (" Stack size exceeded on state_stack");
               raise yy_tokens.Syntax_Error;
            end if;
            yy.tos                  := yy.tos + 1;
            yy.state_stack (yy.tos) := yy.action;
            yy.value_stack (yy.tos) := YYLVal;

            if yy.error_flag > 0 then  --  indicate a valid shift
               yy.error_flag := yy.error_flag - 1;
            end if;

            --  Advance lookahead
            yy.look_ahead := True;

         elsif yy.action = yy.error_code then       -- ERROR
            handle_error;

         elsif yy.action = yy.accept_code then
            if yy.debug then
               Text_IO.Put_Line ("  --  Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

         else --  Reduce Action

            --  Convert action into a rule
            yy.rule_id := Rule (-1 * yy.action);

            --  Execute User Action
            --  user_action(yy.rule_id);
            case yy.rule_id is
               pragma Style_Checks (Off);

when 1 => -- #line 36

              Expr := yy.value_stack (yy.tos).Expr;


when 2 => -- #line 43

              YYVal := yy.value_stack (yy.tos-1);


when 3 => -- #line 48

              YYVal.expr := MAT.Expressions.Create_Not (yy.value_stack (yy.tos).expr);


when 4 => -- #line 53

              YYVal.expr := MAT.Expressions.Create_Or (yy.value_stack (yy.tos-2).expr, yy.value_stack (yy.tos).expr);


when 5 => -- #line 58

              YYVal.expr := MAT.Expressions.Create_And (yy.value_stack (yy.tos-2).expr, yy.value_stack (yy.tos).expr);


when 6 => -- #line 63

               if yy.value_stack (yy.tos-1).bval then
                  YYVal.expr := MAT.Expressions.Create_Inside (yy.value_stack (yy.tos).name, MAT.Expressions.INSIDE_DIRECT_REGION);
               else
                  YYVal.expr := MAT.Expressions.Create_Inside (yy.value_stack (yy.tos).name, MAT.Expressions.INSIDE_REGION);
               end if;


when 7 => -- #line 72

               if yy.value_stack (yy.tos-1).bval then
                  YYVal.expr := MAT.Expressions.Create_Inside (yy.value_stack (yy.tos).name, MAT.Expressions.INSIDE_DIRECT_FUNCTION);
               else
                  YYVal.expr := MAT.Expressions.Create_Inside (yy.value_stack (yy.tos).name, MAT.Expressions.INSIDE_FUNCTION);
               end if;


when 8 => -- #line 81

               if yy.value_stack (yy.tos-1).bval then
                  YYVal.expr := MAT.Expressions.Create_Inside (yy.value_stack (yy.tos).low, MAT.Expressions.INSIDE_DIRECT_FUNCTION);
               else
                  YYVal.expr := MAT.Expressions.Create_Inside (yy.value_stack (yy.tos).low, MAT.Expressions.INSIDE_FUNCTION);
               end if;


when 9 => -- #line 90

              YYVal.expr := MAT.Expressions.Create_Time (MAT.Types.Target_Tick_Ref (yy.value_stack (yy.tos-2).low),
                                                      MAT.Types.Target_Tick_Ref (yy.value_stack (yy.tos).low));


when 10 => -- #line 96

              YYVal.expr := MAT.Expressions.Create_Time (MAT.Types.Target_Tick_Ref (yy.value_stack (yy.tos).low),
                                                      MAT.Types.Target_Tick_Ref'Last);


when 11 => -- #line 102

              YYVal.expr := MAT.Expressions.Create_Time (MAT.Types.Target_Tick_Ref'First,
                                                      MAT.Types.Target_Tick_Ref (yy.value_stack (yy.tos).low));


when 12 => -- #line 108

              YYVal := yy.value_stack (yy.tos); -- new Condition( C_STIME, $2 );


when 13 => -- #line 113

              YYVal.expr := MAT.Expressions.Create_Size (MAT.Types.Target_Size (yy.value_stack (yy.tos).low),
                                                      MAT.Types.Target_Size (yy.value_stack (yy.tos).high));


when 14 => -- #line 119

              YYVal.expr := MAT.Expressions.Create_Thread (MAT.Types.Target_Thread_Ref (yy.value_stack (yy.tos).low),
                                                          MAT.Types.Target_Thread_Ref (yy.value_stack (yy.tos).high));


when 15 => -- #line 125

              YYVal.expr := MAT.Expressions.Create_Addr (MAT.Types.Target_Addr (yy.value_stack (yy.tos).low),
                                                      MAT.Types.Target_Addr (yy.value_stack (yy.tos).low));


when 16 => -- #line 131

              YYVal.expr := MAT.Expressions.Create_Addr (MAT.Types.Target_Addr (yy.value_stack (yy.tos).low),
                                                      MAT.Types.Target_Addr (yy.value_stack (yy.tos).high));


when 17 => -- #line 137

              YYVal.expr := MAT.Expressions.Create_Event (To_Event_Id_Type (yy.value_stack (yy.tos).low),
                                                       To_Event_Id_Type (yy.value_stack (yy.tos).high));


when 18 => -- #line 143

              YYVal.expr := MAT.Expressions.Create_Event (To_Event_Id_Type (yy.value_stack (yy.tos-2).low),
                                                       To_Event_Id_Type (yy.value_stack (yy.tos).low));


when 19 => -- #line 149

              YYVal.expr := MAT.Expressions.Create_Time (MAT.Types.Target_Tick_Ref (yy.value_stack (yy.tos).low),
                                                      MAT.Types.Target_Tick_Ref (yy.value_stack (yy.tos).high));


when 20 => -- #line 155

              YYVal.expr := MAT.Expressions.Create_Event_Type (MAT.Events.MSG_MALLOC);


when 21 => -- #line 160

              YYVal.expr := MAT.Expressions.Create_Event_Type (MAT.Events.MSG_FREE);


when 22 => -- #line 165

              YYVal.expr := MAT.Expressions.Create_Event_Type (MAT.Events.MSG_REALLOC);


when 23 => -- #line 170

              YYVal.expr := MAT.Expressions.Create_Event_Type (MAT.Events.MSG_SECONDARY_STACK_MARK);


when 24 => -- #line 175

              YYVal.expr := MAT.Expressions.Create_Event_Type (MAT.Events.MSG_SECONDARY_STACK_ALLOC);


when 25 => -- #line 180

              YYVal.expr := MAT.Expressions.Create_Event_Type (MAT.Events.MSG_SECONDARY_STACK_RELEASE);


when 26 => -- #line 185

              YYVal.expr := MAT.Expressions.Create_No_Free;


when 27 => -- #line 190

              YYVal.expr := MAT.Expressions.Create_No_Free;


when 28 => -- #line 195

              YYVal.low := 0;


when 29 => -- #line 202

              YYVal.low  := 0;
              YYVal.high := yy.value_stack (yy.tos).low - 1;


when 30 => -- #line 208

              YYVal.low  := 0;
              YYVal.high := yy.value_stack (yy.tos).low;


when 31 => -- #line 214

              YYVal.low  := yy.value_stack (yy.tos).low + 1;
              YYVal.high := MAT.Types.Uint64'Last;


when 32 => -- #line 220

              YYVal.low  := yy.value_stack (yy.tos).low;
              YYVal.high := MAT.Types.Uint64'Last;


when 33 => -- #line 226

              YYVal.low  := yy.value_stack (yy.tos-2).low;
              YYVal.high := yy.value_stack (yy.tos).low;


when 34 => -- #line 232

              YYVal.low  := yy.value_stack (yy.tos).low;
              YYVal.high := yy.value_stack (yy.tos).low;


when 35 => -- #line 238

              YYVal.low := yy.value_stack (yy.tos).low;
              YYVal.high := yy.value_stack (yy.tos).low;


when 36 => -- #line 246
 YYVal.name := Ada.Strings.Unbounded.To_Unbounded_String (MAT.Expressions.Lexer_DFA.YYText);

when 37 => -- #line 249
 YYVal.name := Ada.Strings.Unbounded.To_Unbounded_String (MAT.Expressions.Lexer_DFA.YYText);

when 38 => -- #line 254
 YYVal.low := 1;

when 39 => -- #line 257
 YYVal := MAT.Expressions.Parser_Tokens.YYLVal;

when 43 => -- #line 270
 YYVal := MAT.Expressions.Parser_Tokens.YYLVal;

when 44 => -- #line 275
 YYVal := MAT.Expressions.Parser_Tokens.YYLVal;

when 45 => -- #line 278
 YYVal := MAT.Expressions.Parser_Tokens.YYLVal; YYVal.low := YYVal.low * 1_000_000;

when 46 => -- #line 283
 YYVal.bval := False;

when 47 => -- #line 286
 YYVal.bval := True;
               pragma Style_Checks (On);

               when others => null;
            end case;

            --  Pop RHS states and goto next state
            yy.tos := yy.tos - Rule_Length (yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
               Text_IO.Put_Line (" Stack size exceeded on state_stack");
               raise yy_tokens.Syntax_Error;
            end if;
            yy.state_stack (yy.tos) := goto_state (yy.state_stack (yy.tos - 1),
                                                   Get_LHS_Rule (yy.rule_id));

            yy.value_stack (yy.tos) := YYVal;
            if yy.debug then
               reduce_debug (yy.rule_id,
                  goto_state (yy.state_stack (yy.tos - 1),
                              Get_LHS_Rule (yy.rule_id)));
            end if;

         end if;
      end loop;

   end YYParse;

end MAT.Expressions.Parser;
