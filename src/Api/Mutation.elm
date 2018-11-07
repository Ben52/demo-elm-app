-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Mutation exposing (CreateDraftRequiredArguments, CreateUserRequiredArguments, PublishRequiredArguments, createDraft, createUser, publish, selection)

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


{-| Select fields to build up a top-level mutation. The request can be sent with
functions from `Graphql.Http`.
-}
selection : (a -> constructor) -> SelectionSet (a -> constructor) RootMutation
selection constructor =
    Object.selection constructor


type alias CreateUserRequiredArguments =
    { name : String }


{-|

  - name -

-}
createUser : CreateUserRequiredArguments -> SelectionSet decodesTo Api.Object.User -> Field (Maybe decodesTo) RootMutation
createUser requiredArgs object_ =
    Object.selectionField "createUser" [ Argument.required "name" requiredArgs.name Encode.string ] object_ (identity >> Decode.nullable)


type alias CreateDraftRequiredArguments =
    { title : String, userId : Api.Scalar.Id }


{-|

  - title -
  - userId -

-}
createDraft : CreateDraftRequiredArguments -> SelectionSet decodesTo Api.Object.Post -> Field (Maybe decodesTo) RootMutation
createDraft requiredArgs object_ =
    Object.selectionField "createDraft" [ Argument.required "title" requiredArgs.title Encode.string, Argument.required "userId" requiredArgs.userId (\(Api.Scalar.Id raw) -> Encode.string raw) ] object_ (identity >> Decode.nullable)


type alias PublishRequiredArguments =
    { postId : Api.Scalar.Id }


{-|

  - postId -

-}
publish : PublishRequiredArguments -> SelectionSet decodesTo Api.Object.Post -> Field (Maybe decodesTo) RootMutation
publish requiredArgs object_ =
    Object.selectionField "publish" [ Argument.required "postId" requiredArgs.postId (\(Api.Scalar.Id raw) -> Encode.string raw) ] object_ (identity >> Decode.nullable)
