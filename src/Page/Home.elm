module Page.Home exposing (..)

import Api.Mutation as Mutation
import Api.Object
import Api.Object.Post as Post
import Api.Query as Query
import Browser
import Components.Navbar exposing (navbar)
import Element exposing (..)
import GraphQL exposing (GraphQLResult, postSelection, query)
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html)
import Post exposing (Post)
import Route
import Session exposing (Session)
import User exposing (User)



-- MODEL


type alias Model =
    { session : Session
    , posts : List Post
    , isLoggingOut : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, posts = [], isLoggingOut = False }
    , fetchPosts
    )



-- MESSAGE


type Msg
    = GotPosts (GraphQLResult (List Post))
    | RequestedLogOut
    | LoggedOut (GraphQLResult Bool)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Home"
    , body =
        [ layout []
            (column [ spacing 40, width fill ]
                [ navbar (toSession model)
                    RequestedLogOut
                    { isLoggingOut = model.isLoggingOut }
                , column [] <| List.map viewPost model.posts
                ]
            )
        ]
    }


viewPost : Post -> Element Msg
viewPost post =
    row [ spacing 20 ] [ text <| String.fromFloat post.id, text post.title ]



-- UPDATE


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        GotPosts (Ok posts) ->
            ( { model | posts = posts }, Cmd.none )

        GotPosts _ ->
            ( model, Cmd.none )

        RequestedLogOut ->
            ( model, GraphQL.mutation Mutation.logout LoggedOut )

        LoggedOut result ->
            case result of
                Ok True ->
                    ( updateSession model Nothing, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- EXPORT


toSession : Model -> Session
toSession =
    .session


updateSession : Model -> Maybe User -> Model
updateSession model maybeUser =
    { model | session = Session.updateSession model.session maybeUser }



-- GRAPHQL


fetchPosts : Cmd Msg
fetchPosts =
    query (Query.posts postSelection) GotPosts
