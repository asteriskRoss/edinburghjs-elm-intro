module Example exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { textToReverse : String
    , counter : Int
    }


init : Model
init =
    { textToReverse = ""
    , counter = 0
    }



-- UPDATE


type Msg
    = ChangeText String
    | IncrementCounter


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeText newText ->
            { model | textToReverse = newText }

        IncrementCounter ->
            { model | counter = model.counter + 1 }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick IncrementCounter ] [ text "Click me" ]
        , div [] [ text (String.fromInt model.counter) ]
        , input [ placeholder "Text to reverse", value model.textToReverse, onInput ChangeText ] []
        , div [] [ text (String.reverse model.textToReverse) ]
        ]
