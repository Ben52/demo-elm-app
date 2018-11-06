module Main exposing (Model, Msg(..), Page(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Page.Home as Home
import Url



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = Home Home.Model
    | Author
    | Search


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( initHomeModel, _ ) =
            Home.init
    in
    ( { key = key, page = Home initHomeModel }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        HomeMsg msg ->
            case model.page of
                Home homeModel ->
                    homeUpdate model (Home.update msg homeModel)

                _ ->
                    ( model, Cmd.none )


homeUpdate : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
homeUpdate model ( homeModel, homeCmds ) =
    ( { model | page = Home homeModel }, Cmd.map HomeMsg homeCmds )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "My Elm App"
    , body =
        [ case model.page of
            Home homeModel ->
                Home.view homeModel |> Html.map HomeMsg

            _ ->
                div []
                    [ img [ src "/logo.svg" ] []
                    , h1 [] [ text "Your Elm App is working!" ]
                    ]
        ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
