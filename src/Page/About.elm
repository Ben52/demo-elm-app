module Page.About exposing (Model, Msg, view, update)

import Html exposing (Html, div, text)


type Msg
    = Noop


type alias Model =
    { foo : String }


update : { toParentMsg : Msg -> parentMsg, toParentModel: Model -> parentModel } -> Msg -> Model -> ( Model, Cmd parentMsg )
update { toParentMsg, toParentModel } msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "about page" ]
