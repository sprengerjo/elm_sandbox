module SudukoSpec exposing (..)

import Suduko exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


validCol0 =
    validCol 0


aValidSolution =
    [ [ 5, 3, 4, 6, 7, 8, 9, 1, 2 ]
    , [ 6, 7, 2, 1, 9, 5, 3, 4, 8 ]
    , [ 1, 9, 8, 3, 4, 2, 5, 6, 7 ]
    , [ 8, 5, 9, 7, 6, 1, 4, 2, 3 ]
    , [ 4, 2, 6, 8, 5, 3, 7, 9, 1 ]
    , [ 7, 1, 3, 9, 2, 4, 8, 5, 6 ]
    , [ 9, 6, 1, 5, 3, 7, 2, 8, 4 ]
    , [ 2, 8, 7, 4, 1, 9, 6, 3, 5 ]
    , [ 3, 4, 5, 2, 8, 6, 1, 7, 9 ]
    ]


suite : Test
suite =
    describe "suduko"
        [ describe "solution validator"
            [ test "empty solution is not valid" <|
                \_ -> Expect.equal True (validSolution [ [] ])
            , test "1-2-3 valid solution" <|
                \_ -> Expect.equal True (validSolution [ [ 1, 2, 3 ], [ 2, 3, 1 ], [ 3, 1, 2 ] ])
            , test "a invalid solution should resolve in False" <|
                \_ -> Expect.equal False (validSolution [ [ 1, 2, 1 ], [ 2, 3, 1 ], [ 3, 1, 2 ] ])
            , test "aValidSolution should be valid" <|
                \_ -> Expect.equal True (validSolution aValidSolution)
            ]
        , describe "helper functions - row validation"
            [ test "empty row is valid" <|
                \_ -> Expect.equal True (validRow [])
            , test "single int row is valid" <|
                \_ -> Expect.equal True (validRow [ 1 ])
            , test "same number row is invalid" <|
                \_ -> Expect.equal False (validRow [ 1, 1 ])
            ]
        , describe "helper functions - column validation"
            [ test "empty row is valid" <|
                \_ -> Expect.equal True (validCol0 [])
            , test "single int row is valid" <|
                \_ -> Expect.equal True (validCol0 [ [ 1 ] ])
            , test "same number row is invalid" <|
                \_ -> Expect.equal False (validCol0 [ [ 1 ], [ 1 ] ])
            , test "different number row is valid" <|
                \_ -> Expect.equal True (validCol0 [ [ 1, 2 ], [ 2, 1 ] ])
            ]
        ]
