%%

[ \t]+			{ null; }

with		    return T_With;
or		        return T_OR;
and		        return T_AND;
not		        return T_NOT;
after		    return T_AFTER;
before		    return T_BEFORE;
within		    return T_WITHIN;
reallocation	return T_REALLOCATION;
by		        return T_BY;
at		        return T_AT;
in		        return T_IN;
size		    return T_SIZE;
addr		    return T_ADDR;
all		        return T_ALL;
is		        return T_IS;
from		    return T_FROM;
direct		    return T_DIRECT;
\\\n		{   Line_Number := Line_Number + 1;  }
\(              return '(';
\)              return ')';
\[              return '[';
\]              return ']';
\,              return ',';
\.\.            return T_RANGE;

0x[0-9a-fA-F]+	{  yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_Int;
		        }
\\[0-7]+	{
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_Int;
}
[0-9]+		{
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_Int;
		}
\"		{
		  return T_String;
		}
[A-Za-z_\.\$][A-Za-z0-9_\.\$]*	{
		  return T_Name;
		}
'\n'    { Line_Number := Line_Number + 1;  }
.		{  null;  } 
%%
with MAT.Types;
with MAT.Expressions;
with MAT.Expressions.Parser_Tokens;
package MAT.Expressions.Lexer is

   use MAT.Expressions.Parser_Tokens;

   function YYLex return Token;

end MAT.Expressions.Lexer;

pragma Style_Checks (Off);
package body MAT.Expressions.Lexer is

   lval : MAT.Types.Uint64;
   Line_Number : Natural := 0;
 
##

end MAT.Expressions.Lexer;


