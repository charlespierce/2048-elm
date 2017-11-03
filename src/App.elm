module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Keyboard exposing (..)
import Board exposing (..)
import Utils exposing (..)
import Random
import Random.List


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    Board


init : ( Model, Cmd Msg )
init =
    let
        board =
            Empty |> List.repeat 4 |> List.repeat 4
    in
        board => pickEmptyElement board



-- UPDATE


type Msg
    = NoOp
    | KeyPressed Direction
    | NewTile (Maybe ( Int, Int ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model => Cmd.none

        KeyPressed direction ->
            let
                newBoard =
                    shift direction model
            in
                newBoard => pickEmptyElement model

        NewTile location ->
            case location of
                Nothing ->
                    model => Cmd.none

                Just tuple ->
                    replace (Tuple.first tuple) (Tuple.second tuple) (Tile 2) model => Cmd.none


pickEmptyElement : Model -> Cmd Msg
pickEmptyElement model =
    let
        emptySpaces =
            getEmptyIndices model

        pickIndex tuple =
            Tuple.first tuple

        generator =
            Random.List.choose emptySpaces |> Random.map pickIndex
    in
        Random.generate NewTile generator



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewWinLose isWinningBoard "You Win!" model
        , viewWinLose isLosingBoard "You Lose!" model
        , viewBoard model
        ]


viewWinLose : (Model -> Bool) -> String -> Model -> Html Msg
viewWinLose predicate string model =
    div [] <|
        if predicate model then
            [ text string ]
        else
            []


viewBoard : Board -> Html Msg
viewBoard board =
    table [] (List.map viewRow board)


viewRow : List Tile -> Html Msg
viewRow list =
    tr [] (List.map viewTile list)


viewTile : Tile -> Html Msg
viewTile tile =
    case tile of
        Empty ->
            td [ style [ ( "width", "20px" ), ( "height", "20px" ) ] ] []

        Tile n ->
            td [ style [ ( "width", "20px" ), ( "height", "20px" ) ] ] [ text (toString n) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    presses onKeyPress


onKeyPress : KeyCode -> Msg
onKeyPress keyCode =
    case keyCode of
        97 ->
            KeyPressed Left

        119 ->
            KeyPressed Up

        100 ->
            KeyPressed Right

        115 ->
            KeyPressed Down

        _ ->
            NoOp
