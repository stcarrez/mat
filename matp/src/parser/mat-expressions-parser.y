
%token      T_NAME T_INT T_STRING
%token      T_SELECT T_WITH T_AT T_BY T_IN T_NOT T_OR T_AND
%token      T_SIZE T_ADDR T_FROM T_BEGIN T_END T_TO T_REALLOCATION
%token      T_ALL T_UNOT
%token      T_WITHIN T_USE T_AFTER T_BEFORE
%token      T_DIRECT T_IS
%token T_MALLOC
%token T_REALLOC
%token T_FREE
%token T_THREAD
%token T_RANGE
%token T_EVENT
%token T_TIME
%token T_LT T_LE T_GT T_GE T_NE T_EQ
%token T_HAS
%token '[' ']' '(' ')' ','

%left T_OR
%left T_AND
%right T_UNOT

{
   subtype yystype is MAT.Expressions.yystype;
}

%%

selection :
        condition
            {
              Expr := $1.Expr;
            }
        ;

condition :
        '(' condition ')'
            {
              $$ := $2;
            }
    |
        T_NOT condition %prec T_UNOT
            {
              $$.expr := MAT.Expressions.Create_Not ($2.expr);
            }
    |
        condition T_OR condition
            {
              $$.expr := MAT.Expressions.Create_Or ($1.expr, $3.expr);
            }
    |
        condition T_AND condition
            {
              $$.expr := MAT.Expressions.Create_And ($1.expr, $3.expr);
            }
    |
        T_IN direct name
            {
               if $2.bval then
                  $$.expr := MAT.Expressions.Create_Inside ($3.name, MAT.Expressions.INSIDE_DIRECT_FILE);
               else
                  $$.expr := MAT.Expressions.Create_Inside ($3.name, MAT.Expressions.INSIDE_FILE);
               end if;
            }
    |
        T_BY direct name
            {
               if $2.bval then
                  $$.expr := MAT.Expressions.Create_Inside ($3.name, MAT.Expressions.INSIDE_DIRECT_FUNCTION);
               else
                  $$.expr := MAT.Expressions.Create_Inside ($3.name, MAT.Expressions.INSIDE_FUNCTION);
               end if;
            }
    |
        T_FROM time T_TO time
            {
              $$ := $2;
            }
    |
        T_AFTER time
            {
              $$ := $2;
            }
    |
        T_BEFORE time
            {
              $$ := $2;
            }
    |
        T_WITHIN name
            {
              $$ := $2; -- new Condition( C_STIME, $2 );
            }
    |
        T_SIZE compare
            {
              $$.expr := MAT.Expressions.Create_Size (MAT.Types.Target_Size ($2.low),
                                                      MAT.Types.Target_Size ($2.high));
            }
    |
        T_THREAD compare
            {
              $$.expr := MAT.Expressions.Create_Thread (MAT.Types.Target_Thread_Ref ($2.low),
                                                      	MAT.Types.Target_Thread_Ref ($2.high));
            }
    |
        T_HAS integer
            {
              $$.expr := MAT.Expressions.Create_Addr (MAT.Types.Target_Addr ($2.low),
                                                      MAT.Types.Target_Addr ($2.low));
            }
    |
        T_ADDR compare
            {
              $$.expr := MAT.Expressions.Create_Addr (MAT.Types.Target_Addr ($2.low),
                                                      MAT.Types.Target_Addr ($2.high));
            }
    |
        T_EVENT compare
            {
              $$.expr := MAT.Expressions.Create_Event (To_Event_Id_Type ($2.low),
                                                       To_Event_Id_Type ($2.high));
            }
    |
        T_TIME compare
            {
              $$.expr := MAT.Expressions.Create_Event (To_Event_Id_Type ($2.low),
                                                       To_Event_Id_Type ($2.high));
            }
    |
    	T_MALLOC
    		{
    		  $$.expr := MAT.Expressions.Create_Event_Type (MAT.Events.Targets.MSG_MALLOC);
    		}
    |
    	T_FREE
    		{
    		  $$.expr := MAT.Expressions.Create_Event_Type (MAT.Events.Targets.MSG_FREE);
    		}
    |
    	T_REALLOC
    		{
    		  $$.expr := MAT.Expressions.Create_Event_Type (MAT.Events.Targets.MSG_REALLOC);
    		}
    |
        name
            {
              $$.low := 0;
            }
    ;

compare:
        T_LT integer
            {
              $$.low  := 0;
              $$.high := $2.low - 1;
            }
    |
        T_LE integer
            {
              $$.low  := 0;
              $$.high := $2.low;
            }
    |
        T_GT integer
            {
              $$.low  := $2.low + 1;
              $$.high := MAT.Types.Uint64'Last;
            }
    |
        T_GE integer
            {
              $$.low  := $2.low;
              $$.high := MAT.Types.Uint64'Last;
            }
    |
        T_EQ integer
            {
              $$.low  := $2.low;
              $$.high := $2.low;
            }
    |
        T_NE integer
            {
              $$.low := $2.low;
			  $$.high := $2.low;
            }
    ;

name:
        T_STRING
                { $$.low := 0;              }
    |
        T_NAME
                { $$.low := 0;              }
    ;

count:
        --  /* Empty */
                { $$.low := 1;              }
    |
        T_INT
                { $$ := MAT.Expressions.Parser_Tokens.YYLval;   }
    ;

integer:
        T_INT
                { $$ := MAT.Expressions.Parser_Tokens.YYLval; }
    ;

time:
        name
                { $$.name := Ada.Strings.Unbounded.To_Unbounded_String (MAT.Expressions.Lexer_Dfa.YYText);  }
    ;

direct:
        --  /* Empty */
                { $$.bval := True;          }
    |
        T_DIRECT
                { $$.bval := False;         }
    ;
%%
package MAT.Expressions.Parser is

   pragma Elaborary_Body;

   error_count : Natural := 0;

   function Parse (Content : in String) return MAT.Expressions.Expression_Type;

end MAT.Expressions.Parser;

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

##%procedure_parse

end MAT.Expressions.Parser;
