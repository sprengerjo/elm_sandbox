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
    let
        d =
            9
    in
        case list of
            [] ->
                []

            xs ->
                (zip3 (take d list) (take d (drop d list)) (drop (d * 2) list)) :: splitBy9 (drop (d * 3) list)


splitBy4 list =
    let
        d =
            4
    in
        case list of
            [] ->
                []

            xs ->
                (zip (take d list) (take d (drop d list))) :: splitBy4 (drop (d * 2) list)


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


unzip2 list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                ( a, b ) =
                    x
            in
                a :: b :: unzip2 xs


validateBlock9x9 list =
    foldl (\curr acc -> acc && (validRow curr)) True <|
        map unzip3 <|
            split 3 <|
                flatten2D <|
                    splitBy9 <|
                        flatten2D <|
                            transpose list


interleave4x4 list =
    map unzip2 <|
        split 2 <|
            flatten2D <|
                splitBy4 <|
                    flatten2D <|
                        transpose list


validateBlock3x3 list =
    foldl (\curr acc -> acc && (validRow curr)) True <| interleave4x4 list


validateBlocks list =
    if (length list) == 9 then
        validateBlock9x9 list
    else
        validateBlock3x3 list


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
    validateBlocks solution
        && indexedFoldl (validateColsAndRows solution) True solution
