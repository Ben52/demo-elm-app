module Page.Post exposing (Model, Msg, init, update, view)

--import Http

import Api.Object
import Api.Object.Post
import Api.Query
import Api.Scalar
import Graphql.Field exposing (Field(..))
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


type Id
    = Id String


type Title
    = Title String


type Published
    = Published Bool


type alias Post =
    { id : Id, title : Title, published : Published }


type alias Response =
    { publishedPosts : List Post }


titleString : Post -> String
titleString post =
    let
        (Title title) =
            post.title
    in
    title


init : ( Model, Cmd Msg )
init =
    ( Model [], send GotResponse postsRequest )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( model, send GotResponse postsRequest )

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
            :: List.map (\post -> div [] [ text <| titleString post ]) model.posts


graphqlEndpoint : String
graphqlEndpoint =
    "http://localhost:4000"


graphqlRequest : SelectionSet decodesTo RootQuery -> Graphql.Http.Request decodesTo
graphqlRequest query =
    Graphql.Http.queryRequest graphqlEndpoint query


postsRequest : Graphql.Http.Request Response
postsRequest =
    graphqlRequest postsQuery


postsQuery : SelectionSet Response RootQuery
postsQuery =
    Api.Query.selection Response
        |> with (Api.Query.publishedPosts postSelection)


postSelection : SelectionSet Post Api.Object.Post
postSelection =
    Api.Object.Post.selection Post
        |> with idField
        |> with titleField
        |> with publishedField


idField : Field Id Api.Object.Post
idField =
    Api.Object.Post.id |> Graphql.Field.map (\(Api.Scalar.Id id) -> Id id)


titleField : Field Title Api.Object.Post
titleField =
    Api.Object.Post.title |> Graphql.Field.map (Title << String.toUpper)


publishedField : Field Published Api.Object.Post
publishedField =
    Api.Object.Post.published |> Graphql.Field.map Published
