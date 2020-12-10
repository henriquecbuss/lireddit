module Page.Home exposing (..)

import Api.Mutation as Mutation
import Api.Object
import Api.Object.Post as Post
import Api.Query as Query
import Browser
import Element exposing (..)
import GraphQL exposing (query)
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , post : Maybe Post
    , posts : List Post
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, post = Nothing, posts = [] }
    , query postsQuery GotPosts
    )



-- MESSAGE


type Msg
    = GotPost (Result (Graphql.Http.Error (Maybe Post)) (Maybe Post))
    | GotPosts (Result (Graphql.Http.Error (List Post)) (List Post))



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Home"
    , body =
        [ layout []
            (column [ spacing 40 ]
                [ text "Home"
                , Maybe.map viewPost model.post
                    |> Maybe.withDefault (text "Single post")
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
        GotPost (Ok maybePost) ->
            ( { model | post = maybePost }, Cmd.none )

        GotPosts (Ok posts) ->
            ( { model | posts = posts }, Cmd.none )

        GotPosts _ ->
            ( model, Cmd.none )

        GotPost _ ->
            ( model, Cmd.none )



-- EXPORT


toSession : Model -> Session
toSession =
    .session



-- GRAPHQL


type alias Post =
    { id : Float
    , title : String
    }


postQuery : SelectionSet (Maybe Post) RootQuery
postQuery =
    Query.post { id = 1 } postInfoSelection


postsQuery : SelectionSet (List Post) RootQuery
postsQuery =
    Query.posts postInfoSelection


postInfoSelection : SelectionSet Post Api.Object.Post
postInfoSelection =
    SelectionSet.map2 Post
        Post.id
        Post.title
