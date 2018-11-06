module Main exposing (Model, Msg(..), Page(..), init, main, update, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Page.Home as Home
import Url
import Url.Builder
import Url.Parser exposing (Parser, map, oneOf, s, top)



-- ROUTER


type Page
    = Landing
    | Home Home.Model
    | Author
    | Search
    | NotFound


parser : Parser (Page -> a) a
parser =
    oneOf
        [ map (Home Home.initModel) (s "home")
        , map Author (s "author")
        , map Search (s "search")
        , map Landing top
        ]


fromUrl : Url.Url -> Page
fromUrl url =
    Maybe.withDefault NotFound (Url.Parser.parse parser url)



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , page : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, page = fromUrl url }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | NavigateTo String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        NavigateTo url ->
            ( model, Nav.pushUrl model.key url )

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
            ( { model | page = fromUrl url }, Cmd.none )

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


headerView : Html Msg
headerView =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "width" "100%"
        , style "height" "50px"
        , style "padding-left" "20px"
        , style "background-color" "lightgrey"
        ]
        [ div
            [ style "flex" "1"
            , style "text-align" "left"
            , style "cursor" "pointer"
            , onClick <| NavigateTo "/home"
            ]
            [ text "Home" ]
        , div [] []
        ]


pageView : Page -> Html Msg
pageView page =
    case page of
        Home homeModel ->
            Home.view homeModel |> Html.map HomeMsg

        NotFound ->
            h1 [] [ text "OH NO, your look lost!" ]

        _ ->
            div []
                [ img [ src "/logo.svg" ] []
                , h1 [] [ text "Your Elm App is working!" ]
                ]


view : Model -> Browser.Document Msg
view model =
    { title = "My Elm App"
    , body =
        [ headerView
        , pageView model.page
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
