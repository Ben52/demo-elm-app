module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Page.Home as Home
import Page.Post as Post
import Route exposing (Route)
import Url



-- ROUTER


type Page
    = Home Home.Model
    | Post Post.Model
    | NotFound
    | Blank



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , page : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url) { key = key, page = Blank }



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | PostMsg Post.Msg
    | NavigateTo String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.page ) of
        ( NavigateTo url, _ ) ->
            ( model, Nav.pushUrl model.key url )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( HomeMsg subMsg, Home homeModel ) ->
            Home.update subMsg homeModel
                |> updateWith Home HomeMsg model

        ( PostMsg subMsg, Post subModel ) ->
            Post.update subMsg subModel
                |> updateWith Post PostMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toPage toMsg model ( subModel, subCmd ) =
    ( { model | page = toPage subModel }, Cmd.map toMsg subCmd )


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Route.Post ->
            Post.init
                |> updateWith Post PostMsg model

        Route.Home ->
            Home.init
                |> updateWith Home HomeMsg model

        Route.NotFound ->
            ( model, Cmd.none )



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


pageView : Page -> Html Msg
pageView page =
    case page of
        Home homeModel ->
            Home.view homeModel |> Html.map HomeMsg

        Post postModel ->
            Post.view postModel |> Html.map PostMsg

        _ ->
            div [ class "text-center" ]
                [ h1 [] [ text "404 NOT FOUND!" ]
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
