module Page.Post exposing (Model, Msg, initCmd, initModel, update, view)

--import Http

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
import Json.Decode
import Octicons
import Url exposing (Protocol(..))
import Utils


type alias Model =
    { posts : List Post
    , newPostTitle : String
    , submitting : Bool
    }


type Msg
    = GotResponse (Result (Error Response) Response)
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


type Response
    = Posts (List Post)
    | PostRes Post
    | NewPostRes Post
    | PostDeleted Post


titleString : Title -> String
titleString (Title title) =
    title


initModel : Model
initModel =
    Model [] "" False


initCmd : Cmd Msg
initCmd =
    send GotResponse <| withCredentials postsRequest


init : ( Model, Cmd Msg )
init =
    ( initModel, initCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        NewPostTitleUpdated title ->
            ( { model | newPostTitle = title }, Cmd.none )

        NewPost ->
            ( { model | submitting = True }
            , send GotResponse <| withCredentials (mutationRequest (newPostMutation (Title model.newPostTitle)))
            )

        DeletePost id ->
            ( model
            , send GotResponse <| withCredentials (mutationRequest (deletePostMutation id))
            )

        Fetch ->
            ( model, send GotResponse (withCredentials postsRequest) )

        GotResponse res ->
            case res of
                Ok (Posts posts) ->
                    ( { model | posts = posts }, Cmd.none )

                Ok (PostRes post) ->
                    ( { model | posts = post :: model.posts }, Cmd.none )

                Ok (NewPostRes post) ->
                    ( { model | submitting = False, posts = model.posts ++ [ post ], newPostTitle = "" }, Cmd.none )

                Ok (PostDeleted { id }) ->
                    ( { model | posts = List.filter ((/=) id << .id) model.posts }, Cmd.none )

                Err err ->
                    ( { model | submitting = False }, Cmd.none )


buttonClasses : String
buttonClasses =
    "bg-blue hover:bg-blue-dark text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline "


disabledButtonClasses : String
disabledButtonClasses =
    "opacity-50 cursor-not-allowed "


inputClasses : String
inputClasses =
    "shadow appearance-none border rounded w-full py-2 px-3 text-grey-darker leading-tight focus:outline-none focus:shadow-outline"


onEnter : msg -> Attribute msg
onEnter onEnterAction =
    on "keyup" <|
        Json.Decode.andThen
            (\keyCode ->
                case keyCode of
                    13 ->
                        Json.Decode.succeed onEnterAction

                    _ ->
                        Json.Decode.fail (String.fromInt keyCode)
            )
            keyCode


viewTrashCanIcon : Html Msg
viewTrashCanIcon =
    Html.i [] [ Octicons.trashcan (Octicons.defaultOptions |> Octicons.color "red" |> Octicons.class "cursor-pointer") ]


viewPost : Post -> Html Msg
viewPost post =
    div [ class "flex justify-center" ]
        [ div [ class "mr-4" ] [ text <| titleString post.title ]
        , span [ onClick (DeletePost post.id) ] [ viewTrashCanIcon ]
        ]


view : Model -> Html Msg
view model =
    div [] <|
        [ button [ onClick Fetch ] [ text "Click me!" ] ]
            ++ List.map viewPost model.posts
            ++ [ div [ class "flex justify-center" ]
                    [ div []
                        [ input
                            [ onInput NewPostTitleUpdated
                            , onEnter (Utils.ternary Noop NewPost model.submitting)
                            , value model.newPostTitle
                            , class inputClasses
                            ]
                            []
                        , button
                            [ onClick (Utils.ternary Noop NewPost model.submitting)
                            , class (buttonClasses ++ "mt-6")
                            , classList [ ( disabledButtonClasses, String.isEmpty model.newPostTitle ) ]
                            ]
                            [ text "Submit" ]
                        ]
                    ]
               ]


graphqlEndpoint : String
graphqlEndpoint =
    "https://hello-prisma.now.sh "


postsRequest : Graphql.Http.Request Response
postsRequest =
    queryRequest postsQuery



-- GENERIC QUERY REQUEST


queryRequest : SelectionSet decodesTo RootQuery -> Graphql.Http.Request decodesTo
queryRequest =
    Graphql.Http.queryRequest graphqlEndpoint



-- GENERIC MUTATION REQUEST


mutationRequest : SelectionSet decodesTo RootMutation -> Graphql.Http.Request decodesTo
mutationRequest mutation =
    Graphql.Http.mutationRequest graphqlEndpoint mutation


newPostArgs : Title -> Api.Mutation.CreateDraftRequiredArguments
newPostArgs title =
    { title = titleString title, userId = Api.Scalar.Id "cjo7vfgvj4pdr0a017iv8k1sy" }


newPostMutation : Title -> SelectionSet Response RootMutation
newPostMutation title =
    Api.Mutation.selection NewPostRes
        |> with (Api.Mutation.createDraft (newPostArgs title) postSelection)


deletePostMutation : Id -> SelectionSet Response RootMutation
deletePostMutation (Id id) =
    Api.Mutation.selection PostDeleted
        |> with (Api.Mutation.deletePost { id = Api.Scalar.Id id } postSelection)


postsQuery : SelectionSet Response RootQuery
postsQuery =
    Api.Query.selection Posts
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
