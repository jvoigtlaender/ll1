module Examples
    ( cfg_8_1
    , cfg_8_4
    , cfg_exercise
    , cfg_8_7
    , cfg_8_9
    , cfg_exam
    , cfg_left_associative_arith
    , cfg_left_associative_arith_norec
    , cfg_arith
    , cfg_arith_mod
    , cfg_arith_simpl
    , cfg_arith_simpl_mod
    , cfg_arith_simpl_mod_norec
    ) where

import LL exposing (CFG(..), Symbol(..))


cfg_8_1 =
    CFG
        "S"
        [ "A", "B" ]
        [ "a", "b" ]
        [ ( "S", [ T "a", NT "B" ] )
        , ( "S", [ T "b", NT "A" ] )
        , ( "A", [ T "a" ] )
        , ( "A", [ T "a", NT "S" ] )
        , ( "A", [ T "b", NT "A", NT "A" ] )
        , ( "B", [ T "b" ] )
        , ( "B", [ T "b", NT "S" ] )
        , ( "B", [ T "a", NT "B", NT "B" ] )
        ]


cfg_8_4 =
    CFG
        "S"
        [ "B" ]
        [ "a", "b" ]
        [ ( "S", [ T "a", NT "B" ] )
        , ( "B", [ T "b" ] )
        , ( "B", [ T "a", NT "B", T "b" ] )
        ]


cfg_exercise =
    CFG
        "S"
        [ "A", "B", "C", "D" ]
        [ "a", "b", "c" ]
        [ ( "S", [ NT "A", NT "B" ] )
        , ( "A", [ NT "C", T "a" ] )
        , ( "A", [] )
        , ( "B", [ T "c", NT "D" ] )
        , ( "C", [ T "b" ] )
        , ( "C", [] )
        , ( "D", [ T "a", NT "A", NT "C", NT "D" ] )
        , ( "D", [] )
        ]


cfg_8_7 =
    CFG
        "Session"
        [ "Fact", "Question" ]
        [ "!", "?", "(", ")", "STRING" ]
        [ ( "Session", [ NT "Fact", NT "Session" ] )
        , ( "Session", [ NT "Question" ] )
        , ( "Session", [ T "(", NT "Session", T ")", NT "Session" ] )
        , ( "Fact", [ T "!", T "STRING" ] )
        , ( "Question", [ T "?", T "STRING" ] )
        ]


cfg_8_9 =
    CFG
        "Session"
        [ "Facts", "Fact", "Question" ]
        [ "(", ")", "!", "?", "STRING" ]
        [ ( "Session", [ NT "Facts", NT "Question" ] )
        , ( "Session", [ T "(", NT "Session", T ")", NT "Session" ] )
        , ( "Facts", [ NT "Fact", NT "Facts" ] )
        , ( "Facts", [] )
        , ( "Fact", [ T "!", T "STRING" ] )
        , ( "Question", [ T "?", T "STRING" ] )
        ]


cfg_exam =
    CFG
        "S"
        [ "A", "B", "B'" ]
        [ "a", "b" ]
        [ ( "S", [ NT "A", NT "B" ] )
        , ( "A", [ T "a", NT "A" ] )
        , ( "A", [] )
        , ( "B", [ T "b", NT "B'" ] )
        , ( "B'", [] )
        , ( "B'", [ T "b", NT "B'" ] )
        ]


cfg_left_associative_arith =
    CFG
        "Exp"
        [ "Term", "Factor" ]
        [ "-", "*", "(", ")", "int" ]
        [ ( "Exp", [ NT "Exp", T "-", NT "Term" ] )
        , ( "Exp", [ NT "Term" ] )
        , ( "Term", [ NT "Term", T "*", NT "Factor" ] )
        , ( "Term", [ NT "Factor" ] )
        , ( "Factor", [ T "int" ] )
        , ( "Factor", [ T "(", NT "Exp", T ")" ] )
        ]


cfg_left_associative_arith_norec =
    CFG
        "Exp"
        [ "Exp'", "Term", "Term'", "Factor" ]
        [ "-", "*", "(", ")", "int" ]
        [ ( "Exp", [ NT "Term", NT "Exp'" ] )
        , ( "Exp'", [ T "-", NT "Term", NT "Exp'" ] )
        , ( "Exp'", [ ] )
        , ( "Term", [ NT "Factor", NT "Term'" ] )
        , ( "Term'", [ T "*", NT "Factor", NT "Term'" ] )
        , ( "Term'", [ ] )
        , ( "Factor", [ T "int" ] )
        , ( "Factor", [ T "(", NT "Exp", T ")" ] )
        ]


cfg_arith =
    CFG
        "E"
        [ "T", "F" ]
        [ "+", "*", "(", ")", "int" ]
        [ ( "E", [ NT "T", T "+", NT "E" ] )
        , ( "E", [ NT "T" ] )
        , ( "T", [ NT "F", T "*", NT "T" ] )
        , ( "T", [ NT "F" ] )
        , ( "F", [ T "int" ] )
        , ( "F", [ T "(", NT "E", T ")" ] )
        ]


cfg_arith_mod =
    CFG
        "E"
        [ "E'", "T", "T'", "F" ]
        [ "+", "*", "(", ")", "int" ]
        [ ( "E", [ NT "T", NT "E'" ] )
        , ( "E'", [ T "+", NT "E" ] )
        , ( "E'", [] )
        , ( "T", [ NT "F", NT "T'" ] )
        , ( "T'", [ T "*", NT "T" ] )
        , ( "T'", [] )
        , ( "F", [ T "int" ] )
        , ( "F", [ T "(", NT "E", T ")" ] )
        ]


cfg_arith_simpl =
    CFG
        "E"
        []
        [ "+", "*", "(", ")", "int" ]
        [ ( "E", [ NT "E", T "+", NT "E" ] )
        , ( "E", [ NT "E", T "*", NT "E" ] )
        , ( "E", [ T "int" ] )
        , ( "E", [ T "(", NT "E", T ")" ] )
        ]


cfg_arith_simpl_mod =
    CFG
        "E"
        [ "E'" ]
        [ "+", "*", "(", ")", "int" ]
        [ ( "E", [ NT "E", NT "E'" ] )
        , ( "E'", [ T "+", NT "E" ] )
        , ( "E'", [ T "*", NT "E" ] )
        , ( "E", [ T "int" ] )
        , ( "E", [ T "(", NT "E", T ")" ] )
        ]


cfg_arith_simpl_mod_norec =
    CFG
        "E"
        [ "E'", "E''" ]
        [ "+", "*", "(", ")", "int" ]
        [ ( "E", [ T "int", NT "E''" ] )
        , ( "E", [ T "(", NT "E", T ")", NT "E''" ] )
        , ( "E''", [ NT "E'", NT "E''" ] )
        , ( "E''", [] )
        , ( "E'", [ T "+", NT "E" ] )
        , ( "E'", [ T "*", NT "E" ] )
        ]
