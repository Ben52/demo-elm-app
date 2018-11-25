module Page.Home exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Model =
    String


type Msg
    = Reverse
    | Repeat


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reverse ->
            ( String.reverse model, Cmd.none )

        Repeat ->
            ( String.repeat 2 model, Cmd.none )


buttonClasses : String
buttonClasses =
    "bg-blue hover:bg-blue-dark text-white font-bold py-2 px-4 rounded "


view : Model -> Html Msg
view model =
    div [ class "mt-6" ]
        [ div [] [ text model ]
        , div [ class "flex justify-center mt-6" ]
            [ button [ onClick Reverse, class (buttonClasses ++ "mr-10") ] [ text "Reverse!" ]
            , button [ onClick Repeat, class buttonClasses ] [ text "Repeat!" ]
            ]
        ]


initModel : String
initModel =
    "Welcome Home!!"
