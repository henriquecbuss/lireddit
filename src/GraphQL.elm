module GraphQL exposing
    ( GraphQLResult
    , UserResult(..)
    , getSession
    , mutation
    , postSelection
    , postWithSnippetSelection
    , postsSelection
    , postsWithSnippetSelection
    , query
    , userResultSelection
    , userSelection
    )

import Api.Object exposing (UserResponse)
import Api.Object.FieldError as FieldError
import Api.Object.PaginatedPosts as ObjPagPosts
import Api.Object.Post as ObjPost
import Api.Object.User as User
import Api.Object.UserResponse as UserResponse
import Api.Query as Query
import Browser.Navigation as Nav
import Graphql.Http exposing (Request)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Post exposing (PaginatedPosts, Post, PostWithUser)
import User exposing (User)



-- ENDPOINT


endpoint : String
endpoint =
    "http://localhost:4000/graphql"



-- TYPES


type alias GraphQLResult decodesTo =
    Result (Graphql.Http.Error decodesTo) decodesTo



-- MUTATION


mutation :
    SelectionSet decodesTo RootMutation
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
mutation selectionSet toMsg =
    selectionSet
        |> Graphql.Http.mutationRequest endpoint
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send toMsg



-- QUERY


query :
    SelectionSet decodesTo RootQuery
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
query selectionSet toMsg =
    selectionSet
        |> Graphql.Http.queryRequest endpoint
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send toMsg


getSession : Nav.Key -> (GraphQLResult (Maybe User) -> msg) -> Cmd msg
getSession key toMsg =
    query (Query.me userSelection) toMsg



-- SELECTION SETS


type alias UserError =
    { field : String, message : String }


type alias UserResultIntermediary =
    { errors : Maybe (List UserError), user : Maybe User }


type UserResult
    = WithError (List UserError)
    | WithUser User


errorsSelection : SelectionSet UserError Api.Object.FieldError
errorsSelection =
    SelectionSet.map2 UserError FieldError.field FieldError.message


userSelection : SelectionSet User Api.Object.User
userSelection =
    SelectionSet.map3 User User.id User.username User.email


userResultSelection : SelectionSet UserResult Api.Object.UserResponse
userResultSelection =
    SelectionSet.map2 UserResultIntermediary
        (UserResponse.errors errorsSelection)
        (UserResponse.user userSelection)
        |> SelectionSet.map
            (\{ errors, user } ->
                case ( errors, user ) of
                    ( Nothing, Nothing ) ->
                        WithError []

                    ( Just errs, _ ) ->
                        WithError errs

                    ( Nothing, Just u ) ->
                        WithUser u
            )


postSelection : SelectionSet Post Api.Object.Post
postSelection =
    SelectionSet.map5 Post
        ObjPost.id
        ObjPost.title
        ObjPost.text
        ObjPost.points
        ObjPost.createdAt


postWithSnippetSelection : SelectionSet Post Api.Object.Post
postWithSnippetSelection =
    SelectionSet.map5 Post
        ObjPost.id
        ObjPost.title
        ObjPost.textSnippet
        ObjPost.points
        ObjPost.createdAt


postWithUserSelection : SelectionSet PostWithUser Api.Object.Post
postWithUserSelection =
    SelectionSet.map6 PostWithUser
        ObjPost.id
        ObjPost.title
        ObjPost.text
        ObjPost.points
        (ObjPost.creator userSelection)
        ObjPost.createdAt


postWithUserWithSnippetSelection : SelectionSet PostWithUser Api.Object.Post
postWithUserWithSnippetSelection =
    SelectionSet.map6 PostWithUser
        ObjPost.id
        ObjPost.title
        ObjPost.textSnippet
        ObjPost.points
        (ObjPost.creator userSelection)
        ObjPost.createdAt


postsSelection : SelectionSet PaginatedPosts Api.Object.PaginatedPosts
postsSelection =
    SelectionSet.map2 PaginatedPosts
        (ObjPagPosts.posts postWithUserSelection)
        ObjPagPosts.hasMore


postsWithSnippetSelection : SelectionSet PaginatedPosts Api.Object.PaginatedPosts
postsWithSnippetSelection =
    SelectionSet.map2 PaginatedPosts
        (ObjPagPosts.posts postWithUserWithSnippetSelection)
        ObjPagPosts.hasMore
