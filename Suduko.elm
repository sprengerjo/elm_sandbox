module Suduko exposing (..)

import Maybe exposing (withDefault)
import List exposing (..)
import List.Extra exposing (..)


flatten2D : List (List a) -> List a
flatten2D list =
    foldr (++) [] list


split : Int -> List a -> List (List a)
split i list =
    case take i list of
        [] ->
            []

        xs ->
            xs :: split i (drop i list)


splitBy9 list =
    case list of
        [] ->
            []

        xs ->
            (zip3 (take 9 list) (take 9 (drop 9 list)) (drop 18 list)) :: splitBy9 (drop 27 list)


unzip3 list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                ( a, b, c ) =
                    x
            in
                a :: b :: c :: unzip3 xs


interleave list =
    foldl (\curr acc -> acc && (validRow curr)) True <|
        map unzip3 <|
            split 3 <|
                flatten2D <|
                    splitBy9 <|
                        flatten2D <|
                            transpose list


extractCol : Int -> List number -> number
extractCol index row =
    withDefault 0 (getAt index row)


validCol : Int -> List (List Int) -> Bool
validCol index solution =
    validRow <| map (extractCol index) <| solution


validRow : List Int -> Bool
validRow row =
    allDifferent row


validateColsAndRows : List (List Int) -> Int -> List Int -> Bool -> Bool
validateColsAndRows solution i curr acc =
    acc && (validCol i solution) && (validRow curr)


validateSolution : List (List Int) -> Bool
validateSolution solution =
    interleave solution
        && indexedFoldl (validateColsAndRows solution) True solution
