
pragma Style_Checks (Off);
with Interfaces;
with MAT.Expressions.Parser_Goto;
with MAT.Expressions.Parser_Tokens; 
with MAT.Expressions.Parser_Shift_Reduce;
with MAT.Expressions.Parser_IO;
with MAT.Expressions.Lexer;
with MAT.Expressions.Lexer_Dfa;
with Ada.Text_IO;
package body MAT.Expressions.Parser is

   use Ada;
   use MAT.Expressions.Lexer;
   use type Ada.Text_IO.Count;
   use type MAT.Types.Target_Tick_Ref;
   use type MAT.Events.Targets.Event_Id_Type;
   use type Interfaces.Unsigned_64;

   procedure yyparse;

   procedure yyerror (Message : in String := "syntax error");

   function To_Event_Id_Type (Value : in MAT.Types.Uint64) return MAT.Events.Targets.Event_Id_Type;
   function To_Thread_Ref (Value : in MAT.Types.Uint64) return MAT.Types.Target_Thread_Ref;

   Expr : MAT.Expressions.Expression_Type;

   function To_Event_Id_Type (Value : in MAT.Types.Uint64)
      return MAT.Events.Targets.Event_Id_Type is
   begin
      if Value > MAT.Types.Uint64 (MAT.Events.Targets.Event_Id_Type'Last) then
         return MAT.Events.Targets.Event_Id_Type'Last;
      else
         return MAT.Events.Targets.Event_Id_Type (Value);
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

--  Warning: This file is automatically generated by AYACC.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.


procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Mat.Expressions.Parser_Goto;
    package yy_shift_reduce_tables renames
      Mat.Expressions.Parser_Shift_Reduce;
    package yy_tokens              renames
      Mat.Expressions.Parser_Tokens;

   use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

   procedure yyerrok;
   procedure yyclearin;


   package yy is

       -- the size of the value and state stacks
       --  Affects error 'Stack size exceeded on state_stack'
       stack_size : constant Natural :=  256;

       -- subtype rule         is natural;
       subtype parse_state  is natural;
       -- subtype nonterminal  is integer;

       -- encryption constants
       default           : constant := -1;
       first_shift_entry : constant :=  0;
       accept_code       : constant := -3001;
       error_code        : constant := -3000;

       -- stack data used by the parser
       tos                : natural := 0;
       value_stack        : array(0..stack_size) of yy_tokens.yystype;
       state_stack        : array(0..stack_size) of parse_state;

       -- current input symbol and action the parser is on
       action             : integer;
       rule_id            : rule;
       input_symbol       : yy_tokens.token:= Error;


       -- error recovery flag
       error_flag : natural := 0;
          -- indicates  3 - (number of valid shifts after an error occurs)

       look_ahead : boolean := true;
       index      : integer;

       -- Is Debugging option on or off
        DEBUG : constant boolean := FALSE;

    end yy;


    function goto_state
      (state : yy.parse_state;
       sym   : nonterminal) return yy.parse_state;

    function parse_action
      (state : yy.parse_state;
       t     : yy_tokens.token) return integer;

    pragma inline(goto_state, parse_action);


    function goto_state(state : yy.parse_state;
                        sym   : nonterminal) return yy.parse_state is
        index : integer;
    begin
        index := goto_offset(state);
        while  integer(goto_matrix(index).nonterm) /= sym loop
            index := index + 1;
        end loop;
        return integer(goto_matrix(index).newstate);
    end goto_state;


    function parse_action(state : yy.parse_state;
                          t     : yy_tokens.token) return integer is
        index      : integer;
        tok_pos    : integer;
        default    : constant integer := -1;
    begin
        tok_pos := yy_tokens.token'pos(t);
        index   := shift_reduce_offset(state);
        while integer(shift_reduce_matrix(index).t) /= tok_pos and then
              integer(shift_reduce_matrix(index).t) /= default
        loop
            index := index + 1;
        end loop;
        return integer(shift_reduce_matrix(index).act);
    end parse_action;

-- error recovery stuff

    procedure handle_error is
      temp_action : integer;
    begin

      if yy.error_flag = 3 then -- no shift yet, clobber input.
      if yy.debug then
          text_io.put_line("  -- Ayacc.YYParse: Error Recovery Clobbers " &
                   yy_tokens.token'image(yy.input_symbol));
      end if;
        if yy.input_symbol = yy_tokens.end_of_input then  -- don't discard,
        if yy.debug then
            text_io.put_line("  -- Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
        end if;
        raise yy_tokens.syntax_error;
        end if;

            yy.look_ahead := true;   -- get next token
        return;                  -- and try again...
    end if;

    if yy.error_flag = 0 then -- brand new error
       yyerror("Syntax Error");
    end if;

    yy.error_flag := 3;

    -- find state on stack where error is a valid shift --

    if yy.debug then
        text_io.put_line("  -- Ayacc.YYParse: Looking for state with error as valid shift");
    end if;

    loop
        if yy.debug then
          text_io.put_line("  -- Ayacc.YYParse: Examining State " &
               yy.parse_state'image(yy.state_stack(yy.tos)));
        end if;
        temp_action := parse_action(yy.state_stack(yy.tos), error);

            if temp_action >= yy.first_shift_entry then
                if yy.tos = yy.stack_size then
                    text_io.put_line("  -- Ayacc.YYParse: Stack size exceeded on state_stack");
                    raise yy_Tokens.syntax_error;
                end if;
                yy.tos := yy.tos + 1;
                yy.state_stack(yy.tos) := temp_action;
                exit;
            end if;

        Decrement_Stack_Pointer :
        begin
          yy.tos := yy.tos - 1;
        exception
          when Constraint_Error =>
            yy.tos := 0;
        end Decrement_Stack_Pointer;

        if yy.tos = 0 then
          if yy.debug then
            text_io.put_line("  -- Ayacc.YYParse: Error recovery popped entire stack, aborting...");
          end if;
          raise yy_tokens.syntax_error;
        end if;
    end loop;

    if yy.debug then
        text_io.put_line("  -- Ayacc.YYParse: Shifted error token in state " &
              yy.parse_state'image(yy.state_stack(yy.tos)));
    end if;

    end handle_error;

   -- print debugging information for a shift operation
   procedure shift_debug(state_id: yy.parse_state; lexeme: yy_tokens.token) is
   begin
       text_io.put_line("  -- Ayacc.YYParse: Shift "& yy.parse_state'image(state_id)&" on input symbol "&
               yy_tokens.token'image(lexeme) );
   end;

   -- print debugging information for a reduce operation
   procedure reduce_debug(rule_id: rule; state_id: yy.parse_state) is
   begin
       text_io.put_line("  -- Ayacc.YYParse: Reduce by rule "&rule'image(rule_id)&" goto state "&
               yy.parse_state'image(state_id));
   end;

   -- make the parser believe that 3 valid shifts have occured.
   -- used for error recovery.
   procedure yyerrok is
   begin
       yy.error_flag := 0;
   end yyerrok;

   -- called to clear input symbol that caused an error.
   procedure yyclearin is
   begin
       -- yy.input_symbol := yylex;
       yy.look_ahead := true;
   end yyclearin;


begin
    -- initialize by pushing state 0 and getting the first input symbol
    yy.state_stack(yy.tos) := 0;


    loop

        yy.index := shift_reduce_offset(yy.state_stack(yy.tos));
        if integer(shift_reduce_matrix(yy.index).t) = yy.default then
            yy.action := integer(shift_reduce_matrix(yy.index).act);
        else
            if yy.look_ahead then
                yy.look_ahead   := false;

                yy.input_symbol := yylex;
            end if;
            yy.action :=
             parse_action(yy.state_stack(yy.tos), yy.input_symbol);
        end if;


        if yy.action >= yy.first_shift_entry then  -- SHIFT

            if yy.debug then
                shift_debug(yy.action, yy.input_symbol);
            end if;

            -- Enter new state
            if yy.tos = yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.tos := yy.tos + 1;
            yy.state_stack(yy.tos) := yy.action;
              yy.value_stack(yy.tos) := yylval;

        if yy.error_flag > 0 then  -- indicate a valid shift
            yy.error_flag := yy.error_flag - 1;
        end if;

            -- Advance lookahead
            yy.look_ahead := true;

        elsif yy.action = yy.error_code then       -- ERROR

            handle_error;

        elsif yy.action = yy.accept_code then
            if yy.debug then
                text_io.put_line("  -- Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

        else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Execute User Action
            -- user_action(yy.rule_id);


                case yy.rule_id is

when 1 => -- #line 31

              Expr := 
yy.value_stack(yy.tos).Expr;
            

when 2 => -- #line 38

              
yyval := 
yy.value_stack(yy.tos-1);
            

when 3 => -- #line 43

              
yyval.expr := MAT.Expressions.Create_Not (
yy.value_stack(yy.tos).expr);
            

when 4 => -- #line 48

              
yyval.expr := MAT.Expressions.Create_Or (
yy.value_stack(yy.tos-2).expr, 
yy.value_stack(yy.tos).expr);
            

when 5 => -- #line 53

              
yyval.expr := MAT.Expressions.Create_And (
yy.value_stack(yy.tos-2).expr, 
yy.value_stack(yy.tos).expr);
            

when 6 => -- #line 58

               if 
yy.value_stack(yy.tos-1).bval then
                  
yyval.expr := MAT.Expressions.Create_Inside (
yy.value_stack(yy.tos).name, MAT.Expressions.INSIDE_DIRECT_FILE);
               else
                  
yyval.expr := MAT.Expressions.Create_Inside (
yy.value_stack(yy.tos).name, MAT.Expressions.INSIDE_FILE);
               end if;
            

when 7 => -- #line 67

               if 
yy.value_stack(yy.tos-1).bval then
                  
yyval.expr := MAT.Expressions.Create_Inside (
yy.value_stack(yy.tos).name, MAT.Expressions.INSIDE_DIRECT_FUNCTION);
               else
                  
yyval.expr := MAT.Expressions.Create_Inside (
yy.value_stack(yy.tos).name, MAT.Expressions.INSIDE_FUNCTION);
               end if;
            

when 8 => -- #line 76

              
yyval := 
yy.value_stack(yy.tos-2);
            

when 9 => -- #line 81

              
yyval := 
yy.value_stack(yy.tos);
            

when 10 => -- #line 86

              
yyval := 
yy.value_stack(yy.tos);
            

when 11 => -- #line 91

              
yyval := 
yy.value_stack(yy.tos); -- new Condition( C_STIME, $2 );
            

when 12 => -- #line 96

              
yyval.expr := MAT.Expressions.Create_Size (MAT.Types.Target_Size (
yy.value_stack(yy.tos).low),
                                                      MAT.Types.Target_Size (
yy.value_stack(yy.tos).high));
            

when 13 => -- #line 102

              
yyval.expr := MAT.Expressions.Create_Thread (MAT.Types.Target_Thread_Ref (
yy.value_stack(yy.tos).low),
                                                      	MAT.Types.Target_Thread_Ref (
yy.value_stack(yy.tos).high));
            

when 14 => -- #line 108

              
yyval.expr := MAT.Expressions.Create_Addr (MAT.Types.Target_Addr (
yy.value_stack(yy.tos).low),
                                                      MAT.Types.Target_Addr (
yy.value_stack(yy.tos).low));
            

when 15 => -- #line 114

              
yyval.expr := MAT.Expressions.Create_Addr (MAT.Types.Target_Addr (
yy.value_stack(yy.tos).low),
                                                      MAT.Types.Target_Addr (
yy.value_stack(yy.tos).high));
            

when 16 => -- #line 120

              
yyval.expr := MAT.Expressions.Create_Event (To_Event_Id_Type (
yy.value_stack(yy.tos).low),
                                                       To_Event_Id_Type (
yy.value_stack(yy.tos).high));
            

when 17 => -- #line 126

              
yyval.expr := MAT.Expressions.Create_Event (To_Event_Id_Type (
yy.value_stack(yy.tos).low),
                                                       To_Event_Id_Type (
yy.value_stack(yy.tos).high));
            

when 18 => -- #line 132

    		  
yyval.expr := MAT.Expressions.Create_Event_Type (MAT.Events.Targets.MSG_MALLOC);
    		

when 19 => -- #line 137

    		  
yyval.expr := MAT.Expressions.Create_Event_Type (MAT.Events.Targets.MSG_FREE);
    		

when 20 => -- #line 142

    		  
yyval.expr := MAT.Expressions.Create_Event_Type (MAT.Events.Targets.MSG_REALLOC);
    		

when 21 => -- #line 147

              
yyval.low := 0;
            

when 22 => -- #line 154

              
yyval.low  := 0;
              
yyval.high := 
yy.value_stack(yy.tos).low - 1;
            

when 23 => -- #line 160

              
yyval.low  := 0;
              
yyval.high := 
yy.value_stack(yy.tos).low;
            

when 24 => -- #line 166

              
yyval.low  := 
yy.value_stack(yy.tos).low + 1;
              
yyval.high := MAT.Types.Uint64'Last;
            

when 25 => -- #line 172

              
yyval.low  := 
yy.value_stack(yy.tos).low;
              
yyval.high := MAT.Types.Uint64'Last;
            

when 26 => -- #line 178

              
yyval.low  := 
yy.value_stack(yy.tos).low;
              
yyval.high := 
yy.value_stack(yy.tos).low;
            

when 27 => -- #line 184

              
yyval.low := 
yy.value_stack(yy.tos).low;
			  
yyval.high := 
yy.value_stack(yy.tos).low;
            

when 28 => -- #line 192
 
yyval.low := 0;              

when 29 => -- #line 195
 
yyval.low := 0;              

when 30 => -- #line 200
 
yyval.low := 1;              

when 31 => -- #line 203
 
yyval := MAT.Expressions.Parser_Tokens.YYLval;   

when 32 => -- #line 208

                  
yyval.low  := 
yy.value_stack(yy.tos-3).low;
                  
yyval.high := 
yy.value_stack(yy.tos-1).low;
                

when 33 => -- #line 214

                  
yyval.low  := 
yy.value_stack(yy.tos-3).low;
                  
yyval.high := 
yy.value_stack(yy.tos-1).low;
                

when 34 => -- #line 220

                  
yyval.low  := 0;
                  
yyval.high := 0;
                  -- error( "Wrong  range specification" );
                

when 35 => -- #line 229
 
yyval := MAT.Expressions.Parser_Tokens.YYLval; 

when 36 => -- #line 234
 
yyval.name := Ada.Strings.Unbounded.To_Unbounded_String (MAT.Expressions.Lexer_Dfa.YYText);  

when 37 => -- #line 239
 
yyval.bval := True;          

when 38 => -- #line 242
 
yyval.bval := False;         

                    when others => null;
                end case;


            -- Pop RHS states and goto next state
            yy.tos      := yy.tos - rule_length(yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.state_stack(yy.tos) := goto_state(yy.state_stack(yy.tos-1) ,
                                 get_lhs_rule(yy.rule_id));

              yy.value_stack(yy.tos) := yyval;

            if yy.debug then
                reduce_debug(yy.rule_id,
                    goto_state(yy.state_stack(yy.tos - 1),
                               get_lhs_rule(yy.rule_id)));
            end if;

        end if;


    end loop;


end yyparse;

end MAT.Expressions.Parser;
