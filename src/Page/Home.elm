module Page.Home exposing
    ( Model(..)
    , Msg
    , init
    , toSession
    , update
    , updateSession
    , view
    )

import Api.Mutation as Mutation
import Api.Object
import Api.Object.Post as Post
import Api.Query as Query
import Browser
import Components.Button as Button
import Components.LinkButton exposing (linkButton)
import Components.Navbar exposing (navbar)
import Components.Variant as Variant
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import GraphQL exposing (GraphQLResult, mutation, postSelection, postsWithSnippetSelection, query)
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html)
import Post exposing (PaginatedPosts, Post, PostWithUser)
import Route
import Session exposing (Session)
import User exposing (User)



-- MODEL


type Model
    = WithData { session : Session, posts : List PostWithUser, isLoggingOut : Bool }
    | Loading { session : Session, posts : List PostWithUser, isLoggingOut : Bool }
    | NoMoreData { session : Session, posts : List PostWithUser, isLoggingOut : Bool }
    | Voting
        { session : Session
        , posts : List PostWithUser
        , votingOn : PostWithUser
        , isLoggingOut : Bool
        , hadData : Bool
        }


paginationLimit : Int
paginationLimit =
    5


init : Session -> ( Model, Cmd Msg )
init session =
    ( Loading { session = session, posts = [], isLoggingOut = False }
    , fetchPosts { limit = paginationLimit, cursor = Nothing }
    )



-- MESSAGE


type Msg
    = GotPosts (GraphQLResult PaginatedPosts)
    | RequestedLogOut
    | LoggedOut (GraphQLResult Bool)
    | RequestedPosts
    | RequestedVote PostWithUser Bool
    | Voted (GraphQLResult Post)



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

                Voting v ->
                    v.isLoggingOut

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

                Voting v ->
                    v.posts

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
                    (Just RequestedLogOut)
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
                        List.map (viewPost model) posts
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
                                            Button.button
                                                { onClick = Just RequestedPosts
                                                , variant = Variant.Teal
                                                , state =
                                                    case model of
                                                        Loading _ ->
                                                            Button.Loading

                                                        Voting _ ->
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


viewPost : Model -> PostWithUser -> Element Msg
viewPost model post =
    let
        loggedIn =
            case toSession model of
                Session.LoggedIn _ _ ->
                    True

                _ ->
                    False

        voting =
            case model of
                Voting { votingOn } ->
                    votingOn == post

                _ ->
                    False

        votedPositive =
            case post.voteStatus of
                Just True ->
                    True

                _ ->
                    False

        votedNegative =
            case post.voteStatus of
                Just False ->
                    True

                _ ->
                    False
    in
    row
        [ Border.rounded 4
        , Border.glow (rgba 0.2 0.2 0.2 0.12) 2
        , spacing 30
        , paddingXY 20 50
        , width fill
        ]
        [ column
            [ centerY, spacing 15 ]
            [ if loggedIn then
                Button.button
                    { onClick =
                        if votedPositive then
                            Nothing

                        else
                            Just <| RequestedVote post True
                    , variant =
                        if votedPositive then
                            Variant.Green

                        else
                            Variant.Gray
                    , state =
                        if voting then
                            Button.Loading

                        else
                            Button.Enabled "/\\"
                    }

              else
                none
            , el [ centerX, Font.size 16 ] <| text <| String.fromFloat post.points
            , if loggedIn then
                Button.button
                    { onClick =
                        if votedNegative then
                            Nothing

                        else
                            Just <| RequestedVote post False
                    , variant =
                        if votedNegative then
                            Variant.Red

                        else
                            Variant.Gray
                    , state =
                        if voting then
                            Button.Loading

                        else
                            Button.Enabled "\\/"
                    }

              else
                none
            ]
        , column [ width fill, spacing 20 ]
            [ column [ spacing 10, centerY, width fill ]
                [ el [ Font.bold ] (text post.title)
                , paragraph [ Font.color <| rgb 0.7 0.7 0.7, Font.size 18 ]
                    [ text "posted by "
                    , el [ Font.semiBold ] <| text post.creator.username
                    ]
                ]
            , paragraph [] [ text post.text ]
            ]
        ]



-- UPDATE


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( GotPosts (Ok paginatedPosts), Loading l ) ->
            if paginatedPosts.hasMore then
                ( WithData
                    { l
                        | posts =
                            (l.posts |> Debug.log "PREV POSTS")
                                ++ (paginatedPosts.posts |> Debug.log "NEWPOSTS")
                    }
                , Cmd.none
                )

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
                    List.drop (List.length wd.posts - 1) wd.posts
                        |> List.head
            in
            case maybeLastPost of
                Nothing ->
                    ( model, Cmd.none )

                Just lastPost ->
                    ( Loading wd
                    , fetchPosts
                        { limit = paginationLimit, cursor = Just lastPost.createdAt }
                    )

        ( RequestedVote post isPositive, WithData wd ) ->
            ( Voting
                { session = wd.session
                , posts = wd.posts
                , votingOn = post
                , isLoggingOut = wd.isLoggingOut
                , hadData = True
                }
              -- TODO Change post.id to be an int
            , vote { isPositive = isPositive, postId = round post.id }
            )

        ( RequestedVote post isPositive, NoMoreData nmd ) ->
            ( Voting
                { session = nmd.session
                , posts = nmd.posts
                , votingOn = post
                , isLoggingOut = nmd.isLoggingOut
                , hadData = False
                }
            , vote { isPositive = isPositive, postId = round post.id }
            )

        ( Voted (Ok post), Voting v ) ->
            let
                -- TODO - Update vote count on post in front end?
                modelObj =
                    { session = v.session
                    , posts = renewPostVotes post v.posts
                    , isLoggingOut = v.isLoggingOut
                    }
            in
            if v.hadData then
                ( WithData modelObj, Cmd.none )

            else
                ( NoMoreData modelObj, Cmd.none )

        -- Invalid messages
        ( RequestedPosts, _ ) ->
            ( model, Cmd.none )

        ( RequestedVote _ _, _ ) ->
            ( model, Cmd.none )

        ( Voted _, _ ) ->
            ( model, Cmd.none )


renewPostVotes : Post -> List PostWithUser -> List PostWithUser
renewPostVotes newPost oldPosts =
    case List.head oldPosts of
        Just op ->
            if op.id == newPost.id then
                { op | points = newPost.points, voteStatus = newPost.voteStatus }
                    :: List.drop 1 oldPosts

            else
                op :: renewPostVotes newPost (List.drop 1 oldPosts)

        Nothing ->
            oldPosts



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

        Voting v ->
            v.session


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

        Voting v ->
            ( Voting { v | session = Session.updateSession v.session maybeUser }
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


vote : { isPositive : Bool, postId : Int } -> Cmd Msg
vote args =
    mutation (Mutation.vote args postSelection) Voted
