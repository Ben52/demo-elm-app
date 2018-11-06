module Page.Home exposing (Model, Msg, initModel, update, view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    String


type Msg
    = Reverse
    | Repeat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reverse ->
            ( String.reverse model, Cmd.none )

        Repeat ->
            ( String.repeat 2 model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text model ]
        , button [ onClick Reverse ] [ text "Reverse!" ]
        , button [ onClick Repeat ] [ text "Repeat!" ]
        ]


initModel : String
initModel =
    "Welcome Home!!"
