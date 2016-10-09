port module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Array exposing (..)

main : Program (Maybe Model)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

-- INIT

init data = 
    case data of
        Just data -> (data, Cmd.none)
        Nothing -> (model, Cmd.none)


-- MODEL

type alias Model =
    { gratefulness : Array String
    , daily_plans: Array String
    , affirmations: String
    , daily_reflections: Array String
    , evaluation: String
    }


model : Model
model =
    Model
    (Array.fromList ["", "", ""])
    (Array.fromList ["", "", ""])
    ""
    (Array.fromList ["", "", ""])
    ""


-- UPDATE

type Msg
    = UpdateGratefulness Int String
    | UpdateDailyPlans Int String
    | UpdateAffirmations String
    | UpdateDailyReflections Int String
    | UpdateEvaluation String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        data =
            case msg of
                UpdateGratefulness idx val ->
                    { model | gratefulness = (Array.set idx val model.gratefulness) }
                UpdateDailyPlans idx val ->
                    { model | daily_plans = (Array.set idx val model.daily_plans) }
                UpdateAffirmations val ->
                    { model | affirmations = val }
                UpdateDailyReflections idx val ->
                    { model | daily_reflections = (Array.set idx val model.daily_reflections) }
                UpdateEvaluation val ->
                    { model | evaluation = val }
    in
       (data, save data)

getFromArray : Int -> Array String -> String
getFromArray idx arr =
    Maybe.withDefault "" (Array.get idx arr)

-- PORTS

port save : Model -> Cmd msg

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "Journal" ]
        [ div [ class "Day" ]
            [ div [ class "Container" ]
                [ h1 [ class "Title" ] [ text "I am grateful for..." ]
                , viewList UpdateGratefulness model.gratefulness
                , h1 [ class "Title" ] [ text "What would make today great?" ]
                , viewList UpdateDailyPlans model.daily_plans
                , h1 [ class "Title" ] [ text "Daily affirmations. I am..." ]
                , viewInput model.affirmations UpdateAffirmations
                ]
            ]
        , div [ class "Night" ]
            [ div [ class "Container" ]
                [ h1 [ class "Title" ] [ text "3 amazing things that happened today..." ]
                , viewList UpdateDailyReflections model.daily_reflections
                , h1 [ class "Title" ] [ text "How could I have made today even better?" ]
                , viewInput model.evaluation UpdateEvaluation
                ]
            ]
        ]

viewList : (Int -> String -> Msg) -> Array String -> Html Msg
viewList updateFn array =
    ol [ class "List" ]
        (Array.toList (Array.indexedMap (viewItem updateFn) array))

viewItem : (Int -> String -> Msg) -> Int -> String -> Html Msg
viewItem updateFn idx str =
    li [ class "Item" ]
        [ viewInput str (updateFn idx)
        ]

viewInput : String -> (String -> Msg) -> Html Msg
viewInput str onUpdate =
    input [ class "Input", type' "text", value str, onInput onUpdate ] []
