module SudokuUi exposing (..)

import Suduko exposing (validateSolution)
import List exposing (range, repeat)
import List.Extra exposing (updateAt, indexedFoldl, elemIndices)
import Html exposing (input, Html, Attribute, div, img, text, button, br, fieldset, label)
import Html.Attributes exposing (style, class, type_, checked, src, height, width)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


type alias Grid =
    List (List Int)


type alias Model =
    { grid : Grid
    , valid : Bool
    }


type Msg
    = Initialize Model
    | Toggle ( Int, Int ) Int
    | Tick Time


stepDelay =
    200


cellSize =
    100


cells =
    4


( columns, rows ) =
    ( cells, cells )


successPaths valid =
    case valid of
        True ->
            "images/correct_flower.gif"

        False ->
            "images/wrong.jpeg"


images index =
    case index of
        1 ->
            "images/fluttershy.png"

        2 ->
            "images/rainbow.jpg"

        3 ->
            "images/spike.jpg"

        4 ->
            "images/celestia.png"

        5 ->
            "images/rarity.jpg"

        6 ->
            "images/applejack.jpg"

        7 ->
            "images/pinkie.jpg"

        8 ->
            "images/moon.png"

        x ->
            "images/twilight.jpg"


imgSrc n =
    let
        size =
            if n > 0 then
                cellSize
            else
                0
    in
        [ src (images n), width size, height size ]


initGrid : Grid
initGrid =
    (repeat rows <| repeat cells 0)


init : ( Model, Cmd Msg )
init =
    ( { grid = initGrid, valid = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every stepDelay Tick


toggleCells : Grid -> List ( Int, Int ) -> Grid
toggleCells grid cells =
    List.foldr (\cell curr -> (toggle cell curr)) grid cells


toggle : ( Int, Int ) -> List (List Int) -> List (List Int)
toggle pos grid =
    updateAt (Tuple.first pos)
        (updateAt (Tuple.second pos)
            (\n ->
                if n >= 9 then
                    1
                else
                    n + 1
            )
        )
        grid


row : Int -> List Int -> Html Msg
row x row =
    div
        [ style [ ( "clear", "both" ) ]
        ]
        (List.indexedMap (cell x) row)


cell : Int -> Int -> Int -> Html Msg
cell x y on =
    div
        [ onClick (Toggle ( x, y ) on)
        , cellStyle on
        ]
        [ img (imgSrc on) [] ]


gameStyle : Attribute msg
gameStyle =
    style
        [ ( "padding", "50px" )
        , ( "width", "100%" )
        , ( "text-align", "center" )
        ]


innerStyle : Attribute msg
innerStyle =
    style
        [ ( "display", "table" )
        , ( "margin", "auto" )
        ]


cellStyle : Int -> Attribute msg
cellStyle on =
    style
        [ ( "background", "white" )
        , ( "width", toString cellSize ++ "px" )
        , ( "height", toString cellSize ++ "px" )
        , ( "float", "left" )
        , ( "border", "solid" )
        , ( "border-width", "0.5px" )
        ]


view : Model -> Html Msg
view { grid, valid } =
    div [ gameStyle ]
        [ div [ innerStyle ] (List.indexedMap row grid)
        , br [] []
        , img [ src (successPaths valid), width cellSize, height cellSize ] []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle pos state ->
            let
                next =
                    toggle pos model.grid
            in
                ( { model | grid = next, valid = validateSolution next }, Cmd.none )

        Initialize initial ->
            ( initial, Cmd.none )

        Tick _ ->
            ( model, Cmd.none )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
