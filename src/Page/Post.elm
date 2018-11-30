module Page.Post exposing (Model, Msg, init, update, view)

import Api.Mutation
import Api.Object
import Api.Object.Post
import Api.Query
import Api.Scalar
import Graphql.Field exposing (Field(..))
import Graphql.Http exposing (Error, send, withCredentials)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet(..), with)
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (class, classList, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode
import Keyboard
import Keyboard.Events
import Octicons
import RemoteData exposing (RemoteData(..), WebData)
import Utils
import Validate exposing (Valid, Validator)


type alias Model =
    { posts : RemoteData (Graphql.Http.Error PostsResponse) (List Post)
    , newPost : RemoteData (Graphql.Http.Error PostResponse) Post
    , deletedPost : RemoteData (Graphql.Http.Error PostResponse) Post
    , newPostTitle : String
    }


type Msg
    = GotPostsResponse (RemoteData (Graphql.Http.Error PostsResponse) PostsResponse)
    | GotNewPostRes (RemoteData (Graphql.Http.Error PostResponse) PostResponse)
    | GotPostDeletedRes (RemoteData (Graphql.Http.Error PostResponse) PostResponse)
    | Fetch
    | NewPostTitleUpdated String
    | NewPost
    | DeletePost Id
    | Noop


type Id
    = Id String


type Title
    = Title String


type Published
    = Published Bool


type alias Post =
    { id : Id, title : Title, published : Published }


type alias PostsResponse =
    { posts : List Post }


type alias PostResponse =
    { post : Post }


titleString : Title -> String
titleString (Title title) =
    title


titleValidator : Validator String Title
titleValidator =
    Validate.ifBlank titleString "Please enter a title"


titleIsValid : Title -> Bool
titleIsValid title =
    case Validate.validate titleValidator title of
        Ok _ ->
            True

        Err _ ->
            False


initModel : Model
initModel =
    Model NotAsked NotAsked NotAsked ""


initCmd : Cmd Msg
initCmd =
    sendQuery GotPostsResponse postsQuery


init : ( Model, Cmd Msg )
init =
    fetchPosts initModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        NewPostTitleUpdated title ->
            ( { model | newPostTitle = title }, Cmd.none )

        NewPost ->
            case Validate.validate titleValidator <| Title model.newPostTitle of
                Ok validTitle ->
                    ( { model | newPost = Loading }, sendMutation GotNewPostRes (newPostMutation validTitle) )

                Err errors ->
                    ( model, Cmd.none )

        DeletePost id ->
            ( { model | deletedPost = Loading }, sendMutation GotPostDeletedRes (deletePostMutation id) )

        Fetch ->
            fetchPosts model

        GotPostsResponse remoteData ->
            ( { model | posts = RemoteData.map .posts remoteData }, Cmd.none )

        GotNewPostRes remoteData ->
            let
                posts =
                    case remoteData of
                        Success { post } ->
                            RemoteData.map (List.append [ post ]) model.posts

                        _ ->
                            model.posts
            in
            ( { model
                | newPostTitle = ""
                , newPost = NotAsked
                , posts = posts
              }
            , Cmd.none
            )

        GotPostDeletedRes remoteData ->
            let
                posts =
                    case remoteData of
                        Success { post } ->
                            RemoteData.map (List.filter ((/=) post.id << .id)) model.posts

                        _ ->
                            model.posts
            in
            ( { model | posts = posts, deletedPost = NotAsked }, Cmd.none )


buttonClasses : String
buttonClasses =
    "bg-blue hover:bg-blue-dark text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline "


disabledButtonClasses : String
disabledButtonClasses =
    "opacity-50 cursor-not-allowed "


inputClasses : String
inputClasses =
    "shadow appearance-none border rounded w-full py-2 px-3 text-grey-darker leading-tight focus:outline-none focus:shadow-outline"


viewTrashCanIcon : Html Msg
viewTrashCanIcon =
    Html.i [] [ Octicons.trashcan (Octicons.defaultOptions |> Octicons.color "red" |> Octicons.class "cursor-pointer") ]


viewPost : Post -> Html Msg
viewPost post =
    div [ class "flex justify-center" ]
        [ div [ class "mr-4" ] [ text <| titleString post.title ]
        , span [ onClick (DeletePost post.id) ] [ viewTrashCanIcon ]
        ]


buttonText : Model -> Html Msg
buttonText model =
    case ( model.deletedPost, model.newPost ) of
        ( Loading, _ ) ->
            text "Loading..."

        ( _, Loading ) ->
            text "Loading..."

        ( _, _ ) ->
            text "Submit"


view : Model -> Html Msg
view model =
    div [] <|
        case model.posts of
            Success posts ->
                [ button [ onClick Fetch ] [ text "Click me!" ] ]
                    ++ List.map viewPost posts
                    ++ [ div [ class "flex justify-center" ]
                            [ div []
                                [ input
                                    [ onInput NewPostTitleUpdated
                                    , Keyboard.Events.onKeyDown [ ( Keyboard.Enter, Utils.ternary Noop NewPost (RemoteData.isLoading model.newPost) ) ]
                                    , value model.newPostTitle
                                    , class inputClasses
                                    ]
                                    []
                                , button
                                    [ onClick (Utils.ternary Noop NewPost (RemoteData.isLoading model.newPost))
                                    , class (buttonClasses ++ "mt-6")
                                    , classList [ ( disabledButtonClasses, Title model.newPostTitle |> titleIsValid |> not ) ]
                                    ]
                                    [ buttonText model ]
                                ]
                            ]
                       ]

            NotAsked ->
                [ text "Not asked..." ]

            Loading ->
                [ text "Loading..." ]

            Failure _ ->
                [ text "Error..." ]



-- GRAPHQL API


graphqlEndpoint : String
graphqlEndpoint =
    "https://hello-prisma.now.sh"


fetchPosts : Model -> ( Model, Cmd Msg )
fetchPosts model =
    ( { model | posts = Loading }, sendQuery GotPostsResponse postsQuery )


sendQuery : (RemoteData (Error a) a -> Msg) -> SelectionSet a RootQuery -> Cmd Msg
sendQuery toMsg query =
    query
        |> Graphql.Http.queryRequest graphqlEndpoint
        |> withCredentials
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


sendMutation : (RemoteData (Error a) a -> Msg) -> SelectionSet a RootMutation -> Cmd Msg
sendMutation toMsg mutation =
    mutation
        |> Graphql.Http.mutationRequest graphqlEndpoint
        |> withCredentials
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


newPostMutation : Valid Title -> SelectionSet PostResponse RootMutation
newPostMutation title =
    Api.Mutation.selection PostResponse
        |> with (Api.Mutation.createDraft (newPostArgs title) postSelection)


newPostArgs : Valid Title -> Api.Mutation.CreateDraftRequiredArguments
newPostArgs title =
    { title = titleString <| Validate.fromValid title, userId = Api.Scalar.Id "cjo7vfgvj4pdr0a017iv8k1sy" }


deletePostMutation : Id -> SelectionSet PostResponse RootMutation
deletePostMutation (Id id) =
    Api.Mutation.selection PostResponse
        |> with (Api.Mutation.deletePost { id = Api.Scalar.Id id } postSelection)


postsQuery : SelectionSet PostsResponse RootQuery
postsQuery =
    Api.Query.selection PostsResponse
        |> with (Api.Query.posts postSelection)


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
