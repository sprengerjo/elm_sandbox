module Suduko exposing (..)

import Maybe exposing (withDefault)
import List exposing (..)
import List.Extra exposing (..)


extractCol : Int -> List number -> number
extractCol index row =
    withDefault 0 (getAt index row)


validCol : Int -> List (List Int) -> Bool
validCol index solution =
    validRow <| map (extractCol index) <| solution


validRow : List Int -> Bool
validRow row =
    allDifferent row


validSolution : List (List Int) -> Bool
validSolution solution =
    indexedFoldl (\i curr acc -> acc && (validCol i solution) && (validRow curr)) True solution
