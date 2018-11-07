module Page.Post exposing (Model, Msg, init, update, view)

--import Http

import Api.Object
import Api.Object.Post
import Api.Query
import Api.Scalar
import Graphql.Field
import Graphql.Http exposing (Error, send)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet(..), with)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Url exposing (Protocol(..))


type alias Model =
    { posts : List Post
    }


type Msg
    = GotResponse (Result (Error Response) Response)
    | Fetch


init : ( Model, Cmd Msg )
init =
    ( Model [], send GotResponse queryRequest )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( model, send GotResponse queryRequest )

        GotResponse res ->
            case res of
                Ok { publishedPosts } ->
                    ( { model | posts = publishedPosts }, Cmd.none )

                Err err ->
                    let
                        oops =
                            Debug.log "oops" err
                    in
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] <|
        button [ onClick Fetch ] [ text "Click me!" ]
            :: List.map (\post -> div [] [ text post.title ]) model.posts


queryRequest : Graphql.Http.Request Response
queryRequest =
    Graphql.Http.queryRequest "http://localhost:4000" query


type alias Post =
    { id : String, title : String, published : Bool }


type alias Response =
    { publishedPosts : List Post }


query : SelectionSet Response RootQuery
query =
    Api.Query.selection Response
        |> with (Api.Query.publishedPosts postSelection)


postSelection : SelectionSet Post Api.Object.Post
postSelection =
    Api.Object.Post.selection Post
        |> with idField
        |> with (titleField |> Graphql.Field.map String.toUpper)
        |> with publishedField


idField =
    Api.Object.Post.id |> Graphql.Field.map (\(Api.Scalar.Id id) -> id)


titleField =
    Api.Object.Post.title


publishedField =
    Api.Object.Post.published
