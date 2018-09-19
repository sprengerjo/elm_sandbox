module Bowling exposing (..)


bonus : List number -> number
bonus rolls =
    case rolls of
        [] ->
            0

        _ :: [] ->
            0

        _ :: _ :: _ :: [] ->
            0

        10 :: tail ->
            (List.sum (List.take 2 tail)) + (bonus tail)

        a :: b :: tail ->
            if a + b == 10 then
                (Maybe.withDefault 0 (List.head tail)) + (bonus tail)
            else
                bonus tail


score : List number -> number
score rolls =
    List.sum rolls + bonus rolls
