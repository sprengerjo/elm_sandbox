module GameOfLifeSpec exposing (..)

import GameOfLife exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "game of lifee"
        [ describe "step over a generation of the blinker pattern"
            [ test "1 step" <|
                \_ ->
                    let
                        blinker =
                            [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]

                        blinker1 =
                            [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
                    in
                        Expect.equal blinker1 (next blinker)
            , test "2 steps" <|
                \_ ->
                    let
                        blinker =
                            [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
                    in
                        Expect.equal blinker (next (next blinker))
            ]
        , describe "survival rules"
            [ test "0 neighbours - living cell" <|
                \_ -> Expect.false "cell is dead" (alive 0 True)
            , test "1 neighbours - living cell" <|
                \_ -> Expect.false "cell is dead" (alive 1 True)
            , test "2 neighbours - dead cell" <|
                \_ -> Expect.false "cell is dead" (alive 2 False)
            , test "2 neighbours - living cell" <|
                \_ -> Expect.true "cell is alive" (alive 2 True)
            , test "3 neighbours - living cell" <|
                \_ -> Expect.true "cell is alive" (alive 3 True)
            , test "3 neighbours - dead cell" <|
                \_ -> Expect.true "cell is alive" (alive 3 False)
            , test "4 neighbours - cell is dead" <|
                \_ -> Expect.false "cell is dead" (alive 4 True)
            ]
        , describe "calculate and count neighbours"
            [ test "calculate neighbours of a given cell" <|
                \_ ->
                    let
                        expected =
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 1, 0 )
                            , ( 1, 2 )
                            , ( 2, 0 )
                            , ( 2, 1 )
                            , ( 2, 2 )
                            ]
                    in
                        Expect.equalLists expected (neighbours ( 1, 1 ))
            ]
        ]
