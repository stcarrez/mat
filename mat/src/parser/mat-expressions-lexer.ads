--  Warning: This lexical scanner is automatically generated by AFLEX.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.
--  Template: templates/body-lex.adb
--# line 1 "mat-expressions-lexer.l"
--# line 2 "mat-expressions-lexer.l"

with MAT.Expressions.Parser_Tokens;
package MAT.Expressions.Lexer is

   use MAT.Expressions.Parser_Tokens;

   function YYLex return Token;

end MAT.Expressions.Lexer;
