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
,(-5,15),(-3,1),(-2,18)
-- State  2
,(-5,15),(-3,21)
-- State  3
,(-5,15),(-3,22)
-- State  4
,(-4,24)
-- State  5
,(-4,25)
-- State  6
,(-6,27),(-5,26)
-- State  7
,(-6,28),(-5,26)
-- State  8
,(-6,29),(-5,26)
-- State  9
,(-5,30)
-- State  10
,(-7,37)
-- State  11
,(-7,38)
-- State  12
,(-7,39)
-- State  13
,(-7,40)
-- State  14
,(-7,41)
-- State  19
,(-5,15),(-3,43)
-- State  20
,(-5,15),(-3,44)
-- State  24
,(-5,46)
-- State  25
,(-5,47)
-- State  31
,(-8,50)
-- State  32
,(-8,51)
-- State  33
,(-8,52)
-- State  34
,(-8,53)
-- State  35
,(-8,54)
-- State  36
,(-8,55)
-- State  48
,(-6,56),(-5,26)
);
--  The offset vector
GOTO_OFFSET : array (0.. 56) of Integer :=
(0,
3,3,5,7,8,9,11,13,15,16,17,18,19,20,21,21,21,
21,21,23,25,25,25,25,26,27,27,27,27,27,27,28,29,30,
31,32,33,33,33,33,33,33,33,33,33,33,33,33,35,35,35,
35,35,35,35, 35);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  34) of Natural := (2,
1,3,2,3,3,3,3,4,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,1,1,0,1,5,5,3,1,1,0,1);
   Get_LHS_Rule: array (Rule range  0 ..  34) of Nonterminal := (-1,
-2,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
-3,-3,-3,-7,-7,-7,-7,-7,-7,-5,-5,-9,-9,-10,
-10,-10,-8,-6,-4,-4);
end Mat.Expressions.Parser_Goto;
