%unit MAT.Expressions.Lexer
%%

[ \t]+			{ null; }

with		    return T_WITH;
or		        return T_OR;
and		        return T_AND;
not		        return T_NOT;
after		    return T_AFTER;
before		    return T_BEFORE;
within		    return T_WITHIN;
reallocation	return T_REALLOCATION;
thread          return T_THREAD;
malloc          return T_MALLOC;
free            return T_FREE;
realloc         return T_REALLOC;
by		        return T_BY;
at		        return T_AT;
in		        return T_IN;
size		    return T_SIZE;
addr		    return T_ADDR;
event           return T_EVENT;
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
\<               return T_LT;
\<\=              return T_LE;
\>               return T_GT;
\>\=              return T_GE;
\=               return T_EQ;

0x[0-9a-fA-F]+	{  yylval.low := MAT.Types.Hex_Value (YYText (YYText'First + 2 .. YYText'Last));
		           return T_INT;
		        }
\\[0-7]+	{
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_INT;
}
[0-9]+		{
		           yylval.low := MAT.Types.Uint64'Value (YYText);
		           return T_INT;
		}
\"		{
		  return T_STRING;
		}
[A-Za-z_\.\$][A-Za-z0-9_\.\$]*	{
		  return T_NAME;
		}
'\n'    { Line_Number := Line_Number + 1;  }
.		{  null;  } 
%%
with MAT.Expressions.Parser_Tokens;
package MAT.Expressions.Lexer is

   use MAT.Expressions.Parser_Tokens;

   function YYLex return Token;

end MAT.Expressions.Lexer;

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
##
   pragma Style_Checks (On);

end MAT.Expressions.Lexer;


