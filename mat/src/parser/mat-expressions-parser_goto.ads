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
,(-5,12),(-3,1),(-2,15)
-- State  2
,(-5,12),(-3,18)
-- State  3
,(-5,12),(-3,19)
-- State  4
,(-4,21)
-- State  5
,(-4,22)
-- State  6
,(-6,24),(-5,23)
-- State  7
,(-6,25),(-5,23)
-- State  8
,(-6,26),(-5,23)
-- State  9
,(-5,27)
-- State  16
,(-5,12),(-3,32)
-- State  17
,(-5,12),(-3,33)
-- State  21
,(-5,35)
-- State  22
,(-5,36)
-- State  37
,(-6,41),(-5,23)
-- State  38
,(-7,43)
-- State  39
,(-7,44)
-- State  40
,(-7,45)
-- State  42
,(-9,47)
-- State  49
,(-9,52)
-- State  50
,(-9,53)
);
--  The offset vector
GOTO_OFFSET : array (0.. 55) of Integer :=
(0,
3,3,5,7,8,9,11,13,15,16,16,16,16,16,16,16,18,
20,20,20,20,21,22,22,22,22,22,22,22,22,22,22,22,22,
22,22,22,24,25,26,27,27,28,28,28,28,28,28,28,29,30,
30,30,30, 30);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  27) of Natural := (2,
1,3,2,3,3,3,3,4,2,2,2,4,4,4,1,1,1,1,0,1,5,5,3,1,1,0,1);
   Get_LHS_Rule: array (Rule range  0 ..  27) of Nonterminal := (-1,
-2,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
-3,-3,-5,-5,-8,-8,-7,-7,-7,-9,-6,-4,-4);
end Mat.Expressions.Parser_Goto;
