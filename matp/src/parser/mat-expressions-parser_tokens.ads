pragma Style_Checks (Off);
package Mat.Expressions.Parser_Tokens is


   subtype yystype is MAT.Expressions.yystype;

    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, T_Name, T_Int,
         T_String, T_Select, T_With,
         T_At, T_By, T_In,
         T_Not, T_Or, T_And,
         T_Size, T_Addr, T_From,
         T_Begin, T_End, T_To,
         T_Reallocation, T_All, T_Unot,
         T_Within, T_Use, T_After,
         T_Before, T_Direct, T_Is,
         T_Malloc, T_Realloc, T_Free,
         T_Thread, T_Range, T_Event,
         T_Time, T_Lt, T_Le,
         T_Gt, T_Ge, T_Ne,
         T_Eq, '[', ']',
         '(', ')', ',' );

    Syntax_Error : exception;

end Mat.Expressions.Parser_Tokens;
