pragma Style_Checks (Off);
package Mat.Expressions.Parser_Tokens is


   type yystype is record
      low   : MAT.Types.Uint64 := 0;
      high  : MAT.Types.Uint64 := 0;
      bval  : Boolean := False;
      name  : Ada.Strings.Unbounded.Unbounded_String;
      expr  : MAT.Expressions.Expression_Type;
   end record;


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
         T_Thread, T_Range, T_Event,
         T_Time, T_Lt, T_Le,
         T_Gt, T_Ge, T_Ne,
         T_Eq, '[', ']',
         '(', ')', ',' );

    Syntax_Error : exception;

end Mat.Expressions.Parser_Tokens;
