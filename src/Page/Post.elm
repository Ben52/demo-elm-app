module Page.Post exposing (Model, Msg, init, update, view)

import Api.Mutation
import Api.Object
import Api.Object.Post
import Api.Query
import Api.Scalar
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (class, classList, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Keyboard
import Keyboard.Events
import Octicons
import RemoteData exposing (RemoteData(..))
import Utils
import Validate exposing (Valid, Validator)


type alias Model =
    { posts : RemoteData (Graphql.Http.Error ()) (List Post)
    , newPost : RemoteData (Graphql.Http.Error ()) Post
    , deletedPost : RemoteData (Graphql.Http.Error ()) Post
    , newPostTitle : String
    }


type Msg
    = GotPostsResponse (RemoteData (Graphql.Http.Error ()) (List Post))
    | GotNewPostRes (RemoteData (Graphql.Http.Error ()) Post)
    | GotPostDeletedRes (RemoteData (Graphql.Http.Error ()) Post)
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
            ( { model | posts = remoteData }, Cmd.none )

        GotNewPostRes remoteData ->
            ( { model
                | newPostTitle = ""
                , newPost = NotAsked
                , posts = RemoteData.map2 List.append model.posts (RemoteData.map List.singleton remoteData)
              }
            , Cmd.none
            )

        GotPostDeletedRes remoteData ->
            ( { model
                | deletedPost = NotAsked
                , posts = RemoteData.map2 List.filter (RemoteData.map (/=) remoteData) model.posts
              }
            , Cmd.none
            )


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

            Failure err ->
                [ text "Error..." ]



-- GRAPHQL API


graphqlEndpoint : String
graphqlEndpoint =
    "https://hello-prisma.now.sh"


fetchPosts : Model -> ( Model, Cmd Msg )
fetchPosts model =
    ( { model | posts = Loading }, sendQuery GotPostsResponse postsQuery )


sendQuery : (RemoteData (Graphql.Http.Error ()) a -> Msg) -> SelectionSet a RootQuery -> Cmd Msg
sendQuery toMsg query =
    query
        |> Graphql.Http.queryRequest graphqlEndpoint
        |> Graphql.Http.send (Graphql.Http.discardParsedErrorData >> RemoteData.fromResult >> toMsg)


sendMutation : (RemoteData (Graphql.Http.Error ()) a -> Msg) -> SelectionSet a RootMutation -> Cmd Msg
sendMutation toMsg mutation =
    mutation
        |> Graphql.Http.mutationRequest graphqlEndpoint
        |> Graphql.Http.send (Graphql.Http.discardParsedErrorData >> RemoteData.fromResult >> toMsg)


newPostMutation : Valid Title -> SelectionSet Post RootMutation
newPostMutation title =
    Api.Mutation.createDraft (newPostArgs title) postSelection


newPostArgs : Valid Title -> Api.Mutation.CreateDraftRequiredArguments
newPostArgs title =
    { title = titleString <| Validate.fromValid title, userId = Api.Scalar.Id "cjo7vfgvj4pdr0a017iv8k1sy" }


deletePostMutation : Id -> SelectionSet Post RootMutation
deletePostMutation (Id id) =
    Api.Mutation.deletePost { id = Api.Scalar.Id id } postSelection


postsQuery : SelectionSet (List Post) RootQuery
postsQuery =
    Api.Query.posts postSelection


postSelection : SelectionSet Post Api.Object.Post
postSelection =
    SelectionSet.map3 Post
        (Api.Object.Post.id |> SelectionSet.map (\(Api.Scalar.Id id) -> Id id))
        (Api.Object.Post.title |> SelectionSet.map (Title << String.toUpper))
        (Api.Object.Post.published |> SelectionSet.map Published)
