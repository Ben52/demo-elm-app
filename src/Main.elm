module Main exposing (Model, Msg(..), Page(..), init, main, update, view)

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


type Page
    = Landing
    | Home Home.Model
    | Post ( Post.Model, Cmd Post.Msg )
    | Author
    | Search
    | NotFound


parser : Parser (Page -> a) a
parser =
    oneOf
        [ map (Home Home.initModel) (s "home")
        , map Author (s "author")
        , map Search (s "search")
        , map (Post Post.init) (s "post")
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
    initNewPage { key = key, page = fromUrl url } url



---- UPDATE ----


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | PostMsg Post.Msg
    | NavigateTo String


initNewPage : Model -> Url.Url -> ( Model, Cmd Msg )
initNewPage model url =
    case fromUrl url of
        Post ( postModel, postCmd ) ->
            postUpdate model ( postModel, postCmd )

        _ ->
            ( { model | page = fromUrl url }, Cmd.none )


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
            initNewPage model url

        HomeMsg msg ->
            case model.page of
                Home homeModel ->
                    homeUpdate model (Home.update msg homeModel)

                _ ->
                    ( model, Cmd.none )

        PostMsg msg ->
            case model.page of
                Post ( postModel, postCmd ) ->
                    postUpdate model (Post.update msg postModel)

                _ ->
                    ( model, Cmd.none )


homeUpdate : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
homeUpdate model ( homeModel, homeCmds ) =
    ( { model | page = Home homeModel }, Cmd.map HomeMsg homeCmds )


postUpdate : Model -> ( Post.Model, Cmd Post.Msg ) -> ( Model, Cmd Msg )
postUpdate model ( postModel, postCmds ) =
    ( { model | page = Post ( postModel, postCmds ) }, Cmd.map PostMsg postCmds )



---- VIEW ----


headerView : Html Msg
headerView =
    div
        [ class "flex items-center h-16 bg-grey pl-8" ]
        [ div
            [ class "cursor-pointer"
            , onClick <| NavigateTo "/home"
            ]
            [ text "Home" ]
        , div
            [ class "cursor-pointer"
            , onClick <| NavigateTo "/post"
            ]
            [ text "Post" ]
        ]


pageView : Page -> Html Msg
pageView page =
    case page of
        Home homeModel ->
            Home.view homeModel |> Html.map HomeMsg

        Post ( postModel, postCmd ) ->
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
