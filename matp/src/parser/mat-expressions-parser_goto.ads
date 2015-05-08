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
,(-8,15),(-5,22),(-3,1),(-2,26)
-- State  2
,(-8,15),(-5,22),(-3,29)
-- State  3
,(-8,15),(-5,22),(-3,30)
-- State  4
,(-4,32)
-- State  5
,(-4,33)
-- State  6
,(-6,35),(-5,34)
-- State  7
,(-6,36),(-5,34)
-- State  8
,(-6,37),(-5,34)
-- State  9
,(-5,38)
-- State  10
,(-7,45)
-- State  11
,(-7,46)
-- State  12
,(-8,47)
-- State  13
,(-7,48)
-- State  14
,(-7,49)
-- State  16
,(-7,51)
-- State  27
,(-8,15),(-5,22),(-3,53)
-- State  28
,(-8,15),(-5,22),(-3,54)
-- State  32
,(-5,56)
-- State  33
,(-5,57)
-- State  39
,(-8,59)
-- State  40
,(-8,60)
-- State  41
,(-8,61)
-- State  42
,(-8,62)
-- State  43
,(-8,63)
-- State  44
,(-8,64)
-- State  50
,(-8,65)
-- State  58
,(-6,66),(-5,34)
-- State  67
,(-8,68)
);
--  The offset vector
GOTO_OFFSET : array (0.. 68) of Integer :=
(0,
4,4,7,10,11,12,14,16,18,19,20,21,22,23,24,24,25,
25,25,25,25,25,25,25,25,25,25,28,31,31,31,31,32,33,
33,33,33,33,33,34,35,36,37,38,39,39,39,39,39,39,40,
40,40,40,40,40,40,40,42,42,42,42,42,42,42,42,42, 43);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  39) of Natural := (2,
1,3,2,3,3,3,3,4,2,2,2,2,2,2,2,2,3,2,1,1,1,1,1,1,2,2,2,2,4,2,2,1,1,0,1,1,1,
0,1);
   Get_LHS_Rule: array (Rule range  0 ..  39) of Nonterminal := (-1,
-2,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-7,-7,-7,-7,
-7,-7,-7,-5,-5,-9,-9,-8,-6,-4,-4);
end Mat.Expressions.Parser_Goto;
