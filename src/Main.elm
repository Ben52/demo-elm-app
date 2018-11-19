module Main exposing (Model, Msg(..), Route(..), init, main, update, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Page.Home as Home
import Page.Post as Post
import Url
import Url.Builder
import Url.Parser exposing (Parser, map, oneOf, s, top)



-- ROUTER


type Route
    = Home Home.Model
    | Post Post.Model
    | Author
    | Search
    | NotFound


routeParser : Parser (Route -> Route) Route
routeParser =
    oneOf
        [ map (Home Home.initModel) (s "home")
        , map (Home Home.initModel) top
        , map Author (s "author")
        , map Search (s "search")
        , map (Post Post.initModel) (s "post")
        ]


fromUrl : Url.Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (Url.Parser.parse routeParser url)


routeInitialCmd : Route -> Cmd Msg
routeInitialCmd route =
    case route of
        Post _ ->
            Cmd.map PostMsg Post.initCmd

        _ ->
            Cmd.none



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , page : Route
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            fromUrl url
    in
    ( { key = key, page = route }, routeInitialCmd route )



---- UPDATE ----


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | PostMsg Post.Msg
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
            let
                route =
                    fromUrl url
            in
            ( { model | page = route }, routeInitialCmd route )

        HomeMsg msg ->
            case model.page of
                Home homeModel ->
                    homeUpdate model (Home.update msg homeModel)

                _ ->
                    ( model, Cmd.none )

        PostMsg msg ->
            case model.page of
                Post postModel ->
                    postUpdate model (Post.update msg postModel)

                _ ->
                    ( model, Cmd.none )


homeUpdate : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
homeUpdate model ( homeModel, homeCmds ) =
    ( { model | page = Home homeModel }, Cmd.map HomeMsg homeCmds )


postUpdate : Model -> ( Post.Model, Cmd Post.Msg ) -> ( Model, Cmd Msg )
postUpdate model ( postModel, postCmds ) =
    ( { model | page = Post postModel }, Cmd.map PostMsg postCmds )



---- VIEW ----


headerView : Html Msg
headerView =
    div
        [ class "flex items-center h-16 bg-grey pl-8" ]
        [ div
            [ class "cursor-pointer mr-8"
            , onClick <| NavigateTo "/home"
            ]
            [ text "Home" ]
        , div
            [ class "cursor-pointer"
            , onClick <| NavigateTo "/post"
            ]
            [ text "Posts" ]
        ]


pageView : Route -> Html Msg
pageView page =
    case page of
        Home homeModel ->
            Home.view homeModel |> Html.map HomeMsg

        Post postModel ->
            Post.view postModel |> Html.map PostMsg

        _ ->
            div [ class "text-center" ]
                [ h1 [] [ text "OH NO, your look lost!" ]
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
