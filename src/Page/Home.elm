module Page.Home exposing (..)

import Api.Mutation as Mutation
import Api.Object
import Api.Object.Post as Post
import Api.Query as Query
import Browser
import Components.Button as Button exposing (button)
import Components.Card exposing (card)
import Components.LinkButton exposing (linkButton)
import Components.Navbar exposing (navbar)
import Components.Variant as Variant
import Element exposing (..)
import Element.Font as Font
import Element.Region as Region
import GraphQL exposing (GraphQLResult, postsWithSnippetSelection, query)
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html)
import Post exposing (PaginatedPosts, Post)
import Route
import Session exposing (Session)
import User exposing (User)



-- MODEL


type Model
    = WithData { session : Session, posts : List Post, isLoggingOut : Bool }
    | Loading { session : Session, posts : List Post, isLoggingOut : Bool }
    | NoMoreData { session : Session, posts : List Post, isLoggingOut : Bool }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Loading { session = session, posts = [], isLoggingOut = False }
    , fetchPosts { limit = 10, cursor = Nothing }
    )



-- MESSAGE


type Msg
    = GotPosts (GraphQLResult PaginatedPosts)
    | RequestedLogOut
    | LoggedOut (GraphQLResult Bool)
    | RequestedPosts



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        isLoggingOut =
            case model of
                WithData wd ->
                    wd.isLoggingOut

                Loading l ->
                    l.isLoggingOut

                NoMoreData n ->
                    n.isLoggingOut

        isLoggedIn =
            case toSession model of
                Session.LoggedIn _ _ ->
                    True

                Session.Guest _ ->
                    False

        posts =
            case model of
                WithData wd ->
                    wd.posts

                Loading l ->
                    l.posts

                NoMoreData n ->
                    n.posts

        isInitialLoad =
            case model of
                Loading l ->
                    List.isEmpty l.posts

                _ ->
                    False
    in
    { title = "Home"
    , body =
        [ layout []
            (column [ spacing 40, width fill ]
                [ navbar (toSession model)
                    RequestedLogOut
                    { isLoggingOut = isLoggingOut }
                , row
                    [ width <| maximum 1000 fill, centerX, spaceEvenly ]
                    [ el
                        [ Region.heading 1
                        , Font.bold
                        , Font.color <| rgb255 0 128 128
                        , Font.size 48
                        ]
                        (text "LiReddit")
                    , if isLoggedIn then
                        linkButton
                            { route = Route.CreatePost
                            , variant = Variant.Teal
                            , label = text "Create Post"
                            }

                      else
                        none
                    ]
                , if isInitialLoad then
                    text "Loading"

                  else
                    column
                        [ spacing 60
                        , paddingEach { top = 0, left = 0, right = 0, bottom = 50 }
                        , width <| maximum 1000 fill
                        , centerX
                        ]
                    <|
                        List.map viewPost posts
                            ++ [ el
                                    [ centerX
                                    , paddingEach
                                        { top = 0
                                        , left = 0
                                        , right = 0
                                        , bottom = 100
                                        }
                                    ]
                                 <|
                                    case model of
                                        NoMoreData _ ->
                                            el [ Font.color <| rgb 0.7 0.7 0.7 ] <|
                                                text "You've reached the end"

                                        _ ->
                                            button
                                                { onClick = RequestedPosts
                                                , variant = Variant.Teal
                                                , state =
                                                    case model of
                                                        Loading _ ->
                                                            Button.Loading

                                                        WithData _ ->
                                                            Button.Enabled "More posts"

                                                        NoMoreData _ ->
                                                            Button.Enabled "No more data"
                                                }
                               ]
                ]
            )
        ]
    }


viewPost : Post -> Element Msg
viewPost post =
    card [] ( text post.title, paragraph [] [ text post.text ] )



-- UPDATE


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( GotPosts (Ok paginatedPosts), Loading l ) ->
            if paginatedPosts.hasMore then
                ( WithData { l | posts = l.posts ++ paginatedPosts.posts }, Cmd.none )

            else
                ( NoMoreData { l | posts = l.posts ++ paginatedPosts.posts }, Cmd.none )

        ( GotPosts _, _ ) ->
            ( model, Cmd.none )

        ( RequestedLogOut, _ ) ->
            ( model, GraphQL.mutation Mutation.logout LoggedOut )

        ( LoggedOut result, _ ) ->
            case result of
                Ok True ->
                    updateSession model Nothing

                _ ->
                    ( model, Cmd.none )

        ( RequestedPosts, WithData wd ) ->
            let
                maybeLastPost =
                    List.drop (List.length wd.posts - 2) wd.posts
                        |> List.head
            in
            case maybeLastPost of
                Nothing ->
                    ( model, Cmd.none )

                Just lastPost ->
                    ( Loading wd, fetchPosts { limit = 10, cursor = Just lastPost.createdAt } )

        -- Invalid messages
        ( RequestedPosts, _ ) ->
            ( model, Cmd.none )



-- EXPORT


toSession : Model -> Session
toSession model =
    case model of
        WithData wd ->
            wd.session

        Loading l ->
            l.session

        NoMoreData n ->
            n.session


updateSession : Model -> Maybe User -> ( Model, Cmd Msg )
updateSession model maybeUser =
    case model of
        WithData wd ->
            ( WithData { wd | session = Session.updateSession wd.session maybeUser }
            , Cmd.none
            )

        Loading l ->
            ( Loading { l | session = Session.updateSession l.session maybeUser }
            , Cmd.none
            )

        NoMoreData n ->
            ( NoMoreData { n | session = Session.updateSession n.session maybeUser }
            , Cmd.none
            )



-- GRAPHQL


fetchPosts : { limit : Int, cursor : Maybe String } -> Cmd Msg
fetchPosts { limit, cursor } =
    query
        (Query.posts (\args -> { args | cursor = OptionalArgument.fromMaybe cursor })
            { limit = limit }
            postsWithSnippetSelection
        )
        GotPosts
