package MAT.Expressions.Parser_Tokens is


   subtype YYSType is MAT.Expressions.yystype;

   YYLVal, YYVal : YYSType;
   type Token is
        (END_OF_INPUT, ERROR, T_NAME, T_INT,
         T_STRING, T_SELECT, T_WITH,
         T_AT, T_BY, T_IN,
         T_NOT, T_OR, T_AND,
         T_SIZE, T_ADDR, T_FROM,
         T_BEGIN, T_END, T_TO,
         T_REALLOCATION, T_ALL, T_UNOT,
         T_WITHIN, T_USE, T_AFTER,
         T_BEFORE, T_DIRECT, T_IS,
         T_MALLOC, T_REALLOC, T_FREE,
         T_SMARK, T_SALLOC, T_SRELEASE,
         T_LEAK, T_NO_FREE, T_THREAD,
         T_RANGE, T_EVENT, T_TIME,
         T_LT, T_LE, T_GT,
         T_GE, T_NE, T_EQ,
         T_HAS, '[', ']',
         '(', ')', ',',
         '+', '-');

   Syntax_Error : exception;

end MAT.Expressions.Parser_Tokens;
