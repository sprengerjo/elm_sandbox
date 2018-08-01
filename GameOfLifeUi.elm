module GameOfLifeUi exposing (..)

import GameOfLife exposing (next, flatten2D)
import List
import List.Extra exposing (updateAt, indexedFoldl, elemIndices)
import Html exposing (input, Html, Attribute, div, text, button, br, fieldset, label)
import Html.Attributes exposing (style, class, type_, checked)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


type alias Model =
    { grid : Grid
    , step : Bool
    , stepCount : Int
    }


type alias Grid =
    List (List Bool)


type Msg
    = Initialize Model
    | Toggle ( Int, Int ) Bool
    | Next
    | Tick Time


stepDelay =
    200


cellSize =
    10


( columns, rows ) =
    ( 50, 50 )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initGrid : Grid
initGrid =
    (List.repeat rows <| List.repeat columns False)


init : ( Model, Cmd Msg )
init =
    ( { grid = initGrid, step = False, stepCount = 0 }, Cmd.none )


view : Model -> Html Msg
view { grid, step, stepCount } =
    div []
        [ div [] (List.indexedMap row grid)
        , br [] []
        , fieldset [] [ checkbox Next "stepper" step, text ("generation: " ++ (toString stepCount)) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle pos state ->
            ( { model | grid = toggle pos model.grid }, Cmd.none )

        Next ->
            ( { model | step = not model.step }, Cmd.none )

        Tick _ ->
            ( let
                cells =
                    livingCells model.grid

                anythingAlive =
                    List.length cells > 0
              in
                if model.step then
                    { model | stepCount = model.stepCount + 1, step = anythingAlive, grid = toggleCells initGrid <| next <| cells }
                else
                    model
            , Cmd.none
            )

        Initialize initial ->
            ( initial, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every stepDelay Tick


toggleCells : Grid -> List ( Int, Int ) -> Grid
toggleCells grid cells =
    List.foldr (\cell curr -> (toggle cell curr)) grid cells


toggle : ( Int, Int ) -> List (List Bool) -> List (List Bool)
toggle pos grid =
    updateAt (Tuple.first pos)
        (updateAt (Tuple.second pos) not)
        grid


livingCells : Grid -> List ( Int, Int )
livingCells grid =
    indexedFoldl
        (\y cells curr ->
            List.append curr <| List.map (\x -> ( y, x )) <| (elemIndices True cells)
        )
        []
        grid


row : Int -> List Bool -> Html Msg
row x row =
    div
        [ style [ ( "clear", "both" ) ]
        ]
        (List.indexedMap (cell x) row)


cell : Int -> Int -> Bool -> Html Msg
cell x y on =
    div
        [ onClick (Toggle ( x, y ) on)
        , cellStyle on
        ]
        [ text " " ]


groupInto : Int -> List a -> List (List a)
groupInto n lst =
    if List.length lst == 0 then
        []
    else
        (List.take n lst) :: (groupInto n (List.drop n lst))


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name step =
    label
        [ style [ ( "padding", "20px" ) ]
        ]
        [ input [ type_ "checkbox", onClick msg, checked step ] []
        , text name
        ]


cellStyle : Bool -> Attribute msg
cellStyle on =
    style
        [ ( "background"
          , if on then
                "black"
            else
                "white"
          )
        , ( "width", toString cellSize ++ "px" )
        , ( "height", toString cellSize ++ "px" )
        , ( "float", "left" )
        , ( "border", "solid" )
        , ( "border-width", "0.5px" )
        ]
