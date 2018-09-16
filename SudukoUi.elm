module SudokuUi exposing (..)

import List exposing (range)
import List.Extra exposing (updateAt, indexedFoldl, elemIndices)
import Html exposing (input, Html, Attribute, div, img, text, button, br, fieldset, label)
import Html.Attributes exposing (style, class, type_, checked, src, height, width)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


type alias Model =
    { grid : Grid
    }


type alias Grid =
    List (List Int)


type Msg
    = Initialize Model
    | Toggle ( Int, Int ) Int
    | Tick Time


stepDelay =
    200


cellSize =
    60


( columns, rows ) =
    ( 9, 9 )

images index =
    case index of
        1 -> "images/fluttershy.png"
        2 -> "images/rainbow.jpg"
        3 -> "images/spike.jpg"
        4 -> "images/celestia.png"
        5 -> "images/rarity.jpg"
        6 -> "images/applejack.jpg"
        7 -> "images/pinkie.jpg"
        8 -> "images/moon.png"
        x -> "images/twilight.jpg"

initGrid : Grid
initGrid =
    (List.repeat rows <| range 1 9)


init : ( Model, Cmd Msg )
init =
    ( { grid = initGrid }, Cmd.none )


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
        [ img [ src (images on), width cellSize, height cellSize ] [] ]


groupInto : Int -> List a -> List (List a)
groupInto n lst =
    if List.length lst == 0 then
        []
    else
        (List.take n lst) :: (groupInto n (List.drop n lst))


cellStyle : Int -> Attribute msg
cellStyle on =
    style
        [ ( "background", "white" )
        , ( "width", toString cellSize ++ "px" )
        , ( "height", toString cellSize ++ "px" )
        , ( "float", "left" )
        , ( "border", "solid" )
        , ( "border-width", "0.5px" )
        , ( "text-align", "center" )
        ]


view : Model -> Html Msg
view { grid } =
    div []
        [ div [] (List.indexedMap row grid)
        , br [] []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle pos state ->
            ( { model | grid = toggle pos model.grid }, Cmd.none )

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
