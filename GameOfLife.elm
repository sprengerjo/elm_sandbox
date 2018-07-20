module GameOfLife exposing (..)

import Tuple exposing (..)
import List.Extra exposing (unique)
import List.Extra exposing (count)


alive : Int -> Bool -> Bool
alive n isAlive =
    n == 2 && isAlive || n == 3


neighbours : ( Int, Int ) -> List ( Int, Int )
neighbours cell =
    let
        d =
            [ 1, 0, -1 ]
    in
        List.filter (\c -> c /= cell) <|
            List.concatMap (\x -> (List.map (\y -> ( first (cell) - x, (second cell) - y ))) d) <|
                d


flatten2D : List (List ( Int, Int )) -> List ( Int, Int )
flatten2D list =
    List.foldr (++) [] list


countNeighbours : ( Int, Int ) -> List ( Int, Int ) -> Int
countNeighbours cell cells =
    (count (\y -> y == cell) cells)


filterLiving : List ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
filterLiving livingCells =
    (\xs -> List.filter (\x -> alive (countNeighbours x xs) (List.member x livingCells)) xs)


next : List ( Int, Int ) -> List ( Int, Int )
next cells =
    unique <| filterLiving cells <| flatten2D <| List.map (\x -> neighbours x) <| cells
