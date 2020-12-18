module GraphQL exposing
    ( GraphQLResult
    , UserResult(..)
    , getPost
    , getSession
    , mutation
    , postSelection
    , postWithSnippetSelection
    , postWithUserSelection
    , postWithUserWithSnippetSelection
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
import Post.PostId as PostId exposing (PostId(..))
import User exposing (User)



-- TYPES


type alias GraphQLResult decodesTo =
    Result (Graphql.Http.Error decodesTo) decodesTo



-- MUTATION


mutation :
    String
    -> SelectionSet decodesTo RootMutation
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
mutation apiUrl selectionSet toMsg =
    selectionSet
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send toMsg



-- QUERY


query :
    String
    -> SelectionSet decodesTo RootQuery
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
query apiUrl selectionSet toMsg =
    selectionSet
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send toMsg


getSession : String -> (GraphQLResult (Maybe User) -> msg) -> Cmd msg
getSession apiUrl toMsg =
    query apiUrl (Query.me userSelection) toMsg


getPost : String -> PostId -> (GraphQLResult (Maybe PostWithUser) -> msg) -> Cmd msg
getPost apiUrl postId msg =
    query apiUrl (Query.post { id = PostId.getId postId } postWithUserSelection) msg



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
    SelectionSet.map6 Post
        (SelectionSet.map PostId ObjPost.id)
        ObjPost.title
        ObjPost.text
        ObjPost.points
        ObjPost.voteStatus
        ObjPost.createdAt


postWithSnippetSelection : SelectionSet Post Api.Object.Post
postWithSnippetSelection =
    SelectionSet.map6 Post
        (SelectionSet.map PostId ObjPost.id)
        ObjPost.title
        ObjPost.textSnippet
        ObjPost.points
        ObjPost.voteStatus
        ObjPost.createdAt


postWithUserSelection : SelectionSet PostWithUser Api.Object.Post
postWithUserSelection =
    SelectionSet.map7 PostWithUser
        (SelectionSet.map PostId ObjPost.id)
        ObjPost.title
        ObjPost.text
        ObjPost.points
        ObjPost.voteStatus
        (ObjPost.creator userSelection)
        ObjPost.createdAt


postWithUserWithSnippetSelection : SelectionSet PostWithUser Api.Object.Post
postWithUserWithSnippetSelection =
    SelectionSet.map7 PostWithUser
        (SelectionSet.map PostId ObjPost.id)
        ObjPost.title
        ObjPost.textSnippet
        ObjPost.points
        ObjPost.voteStatus
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
