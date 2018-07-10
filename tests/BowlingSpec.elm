module BowlingSpec exposing (..)

import Bowling exposing (score)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "bowling game"
        [ describe "score calculator"
            [ test "0 pin games should score 0" <|
                \_ -> Expect.equal 0 (score (List.repeat 20 0))
            , test "1 pin games should score 20" <|
                \_ -> Expect.equal 20 (score (List.repeat 20 1))
            , test "1 spare bonus should be added" <|
                let
                    rolls =
                        (List.append [ 4, 6 ] (List.repeat 18 4))
                in
                    \_ -> Expect.equal (4 + 6 + 4 + 18 * 4) (score rolls)
            , test "1 strike bonus should be added" <|
                let
                    rolls =
                        10 :: (List.repeat 18 4)
                in
                    \_ -> Expect.equal (10 + 4 + 4 + 18 * 4) (score rolls)
            , test "perfect game should score 300" <|
                \_ -> Expect.equal 300 (score (List.repeat 12 10))
            ]
        ]
