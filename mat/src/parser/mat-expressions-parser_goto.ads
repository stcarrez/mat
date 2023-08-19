private package MAT.Expressions.Parser_Goto is


   type Rule        is new Natural;
   type Nonterminal is new Integer;

   type Small_Integer is range -32_000 .. 32_000;
   subtype Small_Nonterminal is Nonterminal range -32_000 .. 32_000;

   type Goto_Entry is record
      Nonterm  : Small_Nonterminal;
      Newstate : Small_Integer;
   end record;

   --  pragma suppress(index_check);

   type Row is new Integer range -1 .. Integer'Last;

   type Goto_Parse_Table is array (Row range <>) of Goto_Entry;

   Goto_Matrix : constant Goto_Parse_Table :=
      ((-1, -1)  -- Dummy Entry.
      --  State  0
      , (-6, 15), (-5, 25), (-3, 1), (-2, 29)
      --  State  2
      , (-6, 15), (-5, 25), (-3, 32)
      --  State  3
      , (-6, 15), (-5, 25), (-3, 33)
      --  State  4
      , (-4, 35)
      --  State  5
      , (-4, 36)
      --  State  6
      , (-7, 39)
      --  State  7
      , (-7, 40)
      --  State  8
      , (-7, 41)
      --  State  9
      , (-5, 42)
      --  State  10
      , (-8, 49)
      --  State  11
      , (-8, 50)
      --  State  12
      , (-6, 51)
      --  State  13
      , (-8, 52)
      --  State  14
      , (-8, 53)
      --  State  16
      , (-8, 55)
      --  State  30
      , (-6, 15), (-5, 25), (-3, 57)
      --  State  31
      , (-6, 15), (-5, 25), (-3, 58)
      --  State  35
      , (-5, 60)
      --  State  36
      , (-6, 62), (-5, 61)
      --  State  43
      , (-6, 64)
      --  State  44
      , (-6, 65)
      --  State  45
      , (-6, 66)
      --  State  46
      , (-6, 67)
      --  State  47
      , (-6, 68)
      --  State  48
      , (-6, 69)
      --  State  54
      , (-6, 70)
      --  State  63
      , (-7, 71)
      --  State  72
      , (-6, 73)
      );

   --  The offset vector
   Goto_Offset : constant array (0 .. 73) of Row :=
      (0,
      4, 4, 7, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 21, 22, 22, 22, 22,
      22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
      25, 28, 28, 28, 28, 29, 31, 31, 31, 31,
      31, 31, 31, 32, 33, 34, 35, 36, 37, 37,
      37, 37, 37, 37, 38, 38, 38, 38, 38, 38,
      38, 38, 38, 39, 39, 39, 39, 39, 39, 39,
      39, 39, 40);

   Rule_Length : constant array (Rule range 0 .. 47) of Natural := (2,
      1, 3, 2, 3, 3, 3, 3, 3, 4, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 2, 2, 1, 1, 0, 1, 3, 3, 1, 1, 1, 1, 0,
      1);

   Get_LHS_Rule : constant array (Rule range 0 .. 47) of Nonterminal := (-1,
       -2, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3,
       -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3,
       -8, -8, -8, -8, -8, -8, -8, -5, -5, -9, -9, -10, -10, -10,
       -6, -7, -7, -4, -4);

end MAT.Expressions.Parser_Goto;
