module SudukoSpec exposing (..)

import Suduko exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)
import List exposing (..)


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


aInvalidSolution =
    [ [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
    , [ 2, 3, 1, 5, 6, 4, 8, 9, 7 ]
    , [ 3, 1, 2, 6, 4, 5, 9, 7, 8 ]
    , [ 4, 5, 6, 7, 8, 9, 1, 2, 3 ]
    , [ 5, 6, 4, 8, 9, 7, 2, 3, 1 ]
    , [ 6, 4, 5, 9, 7, 8, 3, 1, 2 ]
    , [ 7, 8, 9, 1, 2, 3, 4, 5, 6 ]
    , [ 8, 9, 7, 2, 3, 1, 5, 6, 4 ]
    , [ 9, 7, 8, 3, 1, 2, 6, 4, 5 ]
    ]


valid4x4 =
    [ [ 1, 2, 3, 4 ]
    , [ 4, 5, 6, 7 ]
    , [ 2, 3, 4, 5 ]
    , [ 6, 7, 8, 9 ]
    ]


invalid4x4 =
    [ [ 1, 2, 3, 4 ]
    , [ 4, 5, 6, 7 ]
    , [ 2, 3, 4, 5 ]
    , [ 6, 7, 8, 4 ]
    ]


suite : Test
suite =
    describe "suduko"
        [ describe "solution validator"
            [ test "empty solution is not valid" <|
                \_ -> Expect.equal True (validateSolution [ [] ])
            , test "4x4 valid solution" <|
                \_ ->
                    Expect.equal True (validateSolution valid4x4)
            , test "4x4 invalid solution" <|
                \_ -> Expect.equal False (validateSolution invalid4x4)
            , test "aValidSolution should be valid" <|
                \_ -> Expect.equal True (validateSolution aValidSolution)
            , test "aInValidSolution should be invalid" <|
                \_ -> Expect.equal False (validateSolution aInvalidSolution)
            ]
        , describe "helper functions - row validation"
            [ test "interleave3x3 valid solution should resolve in False" <|
                \_ ->
                    Expect.equal [ [ 1, 2, 4, 5 ], [ 2, 3, 6, 7 ], [ 3, 4, 6, 7 ], [ 4, 5, 8, 9 ] ]
                        (interleave4x4 valid4x4)
            , test "a invalid solution should resolve in False" <|
                \_ -> Expect.equal False (validateBlocks aInvalidSolution)
            , test "empty row is valid" <|
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
