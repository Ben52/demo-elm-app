-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Query exposing (PostRequiredArguments, PostsByUserRequiredArguments, post, posts, postsByUser, publishedPosts, selection, users)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.Union
import Graphql.Field as Field exposing (Field)
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)


{-| Select fields to build up a top-level query. The request can be sent with
functions from `Graphql.Http`.
-}
selection : (a -> constructor) -> SelectionSet (a -> constructor) RootQuery
selection constructor =
    Object.selection constructor


{-| -}
posts : SelectionSet decodesTo Api.Object.Post -> Field (List decodesTo) RootQuery
posts object_ =
    Object.selectionField "posts" [] object_ (identity >> Decode.list)


{-| -}
users : SelectionSet decodesTo Api.Object.User -> Field (List decodesTo) RootQuery
users object_ =
    Object.selectionField "users" [] object_ (identity >> Decode.list)


{-| -}
publishedPosts : SelectionSet decodesTo Api.Object.Post -> Field (List decodesTo) RootQuery
publishedPosts object_ =
    Object.selectionField "publishedPosts" [] object_ (identity >> Decode.list)


type alias PostRequiredArguments =
    { postId : Api.Scalar.Id }


{-|

  - postId -

-}
post : PostRequiredArguments -> SelectionSet decodesTo Api.Object.Post -> Field (Maybe decodesTo) RootQuery
post requiredArgs object_ =
    Object.selectionField "post" [ Argument.required "postId" requiredArgs.postId (\(Api.Scalar.Id raw) -> Encode.string raw) ] object_ (identity >> Decode.nullable)


type alias PostsByUserRequiredArguments =
    { userId : Api.Scalar.Id }


{-|

  - userId -

-}
postsByUser : PostsByUserRequiredArguments -> SelectionSet decodesTo Api.Object.Post -> Field (List decodesTo) RootQuery
postsByUser requiredArgs object_ =
    Object.selectionField "postsByUser" [ Argument.required "userId" requiredArgs.userId (\(Api.Scalar.Id raw) -> Encode.string raw) ] object_ (identity >> Decode.list)