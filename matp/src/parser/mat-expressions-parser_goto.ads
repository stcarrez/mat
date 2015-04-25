pragma Style_Checks (Off);
package Mat.Expressions.Parser_Goto is

    type Small_Integer is range -32_000 .. 32_000;

    type Goto_Entry is record
        Nonterm  : Small_Integer;
        Newstate : Small_Integer;
    end record;

  --pragma suppress(index_check);

    subtype Row is Integer range -1 .. Integer'Last;

    type Goto_Parse_Table is array (Row range <>) of Goto_Entry;

    Goto_Matrix : constant Goto_Parse_Table :=
       ((-1,-1)  -- Dummy Entry.
-- State  0
,(-5,19),(-3,1),(-2,22)
-- State  2
,(-5,19),(-3,25)
-- State  3
,(-5,19),(-3,26)
-- State  4
,(-4,28)
-- State  5
,(-4,29)
-- State  6
,(-6,31),(-5,30)
-- State  7
,(-6,32),(-5,30)
-- State  8
,(-6,33),(-5,30)
-- State  9
,(-5,34)
-- State  10
,(-7,41)
-- State  11
,(-7,42)
-- State  12
,(-8,44)
-- State  13
,(-7,45)
-- State  14
,(-7,46)
-- State  15
,(-7,47)
-- State  23
,(-5,19),(-3,49)
-- State  24
,(-5,19),(-3,50)
-- State  28
,(-5,52)
-- State  29
,(-5,53)
-- State  35
,(-8,55)
-- State  36
,(-8,56)
-- State  37
,(-8,57)
-- State  38
,(-8,58)
-- State  39
,(-8,59)
-- State  40
,(-8,60)
-- State  54
,(-6,61),(-5,30)
-- State  62
,(-8,63)
);
--  The offset vector
GOTO_OFFSET : array (0.. 63) of Integer :=
(0,
3,3,5,7,8,9,11,13,15,16,17,18,19,20,21,22,22,
22,22,22,22,22,22,24,26,26,26,26,27,28,28,28,28,28,
28,29,30,31,32,33,34,34,34,34,34,34,34,34,34,34,34,
34,34,34,36,36,36,36,36,36,36,36, 37);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  36) of Natural := (2,
1,3,2,3,3,3,3,4,2,2,2,2,2,2,2,2,2,1,1,1,1,2,2,2,2,4,2,2,1,1,0,1,1,1,0,1);
   Get_LHS_Rule: array (Rule range  0 ..  36) of Nonterminal := (-1,
-2,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
-3,-3,-3,-3,-3,-3,-3,-7,-7,-7,-7,-7,-7,-7,
-5,-5,-9,-9,-8,-6,-4,-4);
end Mat.Expressions.Parser_Goto;
