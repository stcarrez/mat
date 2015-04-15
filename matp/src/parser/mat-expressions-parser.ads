package MAT.Expressions.Parser is

   pragma Elaborary_Body;

   error_count : Natural := 0;

   function Parse (Content : in String) return MAT.Expressions.Expression_Type;

end MAT.Expressions.Parser;
