module Page.Post exposing (..)

import Api.Mutation as Mutation
import Api.Query as Query
import Browser
import Components.Navbar exposing (navbar)
import Element exposing (..)
import Element.Font as Font
import GraphQL exposing (GraphQLResult, postWithUserSelection, query)
import Graphql.Http
import Post exposing (PostWithUser)
import Post.PostId as PostId exposing (PostId)
import Session exposing (Session)
import User exposing (User)



-- MODEL


type Model
    = Loading
        { session : Session
        , postId : PostId
        , isLoggingOut : Bool
        }
    | WithPost
        { session : Session
        , post : PostWithUser
        , isLoggingOut : Bool
        }
    | Errored
        { session : Session
        , message : String
        , isLoggingOut : Bool
        }


init : Session -> PostId -> ( Model, Cmd Msg )
init session postId =
    ( Loading { session = session, postId = postId, isLoggingOut = False }
    , fetchPost postId
    )



-- MESSAGE


type Msg
    = FetchedPost (GraphQLResult (Maybe PostWithUser))
    | RequestedLogOut
    | LoggedOut (GraphQLResult Bool)


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    let
        setLoggingOut m val =
            case m of
                Loading l ->
                    Loading { l | isLoggingOut = val }

                WithPost wp ->
                    WithPost { wp | isLoggingOut = val }

                Errored e ->
                    Errored { e | isLoggingOut = val }
    in
    case ( msg, model ) of
        ( FetchedPost (Ok maybePost), Loading l ) ->
            case maybePost of
                Just post ->
                    ( WithPost
                        { session = l.session
                        , post = post
                        , isLoggingOut = l.isLoggingOut
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( Errored
                        { session = l.session
                        , message = "Could not find post"
                        , isLoggingOut = l.isLoggingOut
                        }
                    , Cmd.none
                    )

        ( FetchedPost (Err e), Loading l ) ->
            ( Errored
                { session = l.session
                , message = "Unknown error"
                , isLoggingOut = l.isLoggingOut
                }
            , Cmd.none
            )

        ( RequestedLogOut, _ ) ->
            ( setLoggingOut model True, GraphQL.mutation Mutation.logout LoggedOut )

        ( LoggedOut result, _ ) ->
            case result of
                Ok True ->
                    updateSession (setLoggingOut model False) Nothing

                _ ->
                    ( model, Cmd.none )

        -- Invalid messages
        ( FetchedPost _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        isLoggingOut =
            case model of
                Loading l ->
                    l.isLoggingOut

                WithPost wd ->
                    wd.isLoggingOut

                Errored e ->
                    e.isLoggingOut

        nav =
            navbar (toSession model) (Just RequestedLogOut) { isLoggingOut = isLoggingOut }

        baseCol =
            column [ width <| maximum 800 fill, centerX, spacing 50 ]

        postView children =
            [ layout [] <|
                column [ width fill, spacing 100 ] <|
                    [ nav, baseCol children ]
            ]
    in
    case model of
        Loading _ ->
            { title = "Loading post"
            , body = postView [ text "Loading" ]
            }

        WithPost { post } ->
            { title = post.title
            , body =
                postView
                    [ column [ spacing 10 ]
                        [ el [ Font.bold ] <| text post.title
                        , paragraph [ Font.color <| rgb 0.7 0.7 0.7, Font.size 18 ]
                            [ text "written by "
                            , el [ Font.bold ] <| text post.creator.username
                            ]
                        ]
                    , paragraph [] [ text post.text ]
                    ]
            }

        Errored e ->
            { title = "Error"
            , body = postView [ text e.message ]
            }



-- EXPORTS


toSession : Model -> Session
toSession model =
    case model of
        Loading l ->
            l.session

        WithPost wp ->
            wp.session

        Errored wp ->
            wp.session


updateSession : Model -> Maybe User -> ( Model, Cmd Msg )
updateSession model maybeUser =
    case model of
        Loading l ->
            ( Loading { l | session = Session.updateSession l.session maybeUser }
            , Cmd.none
            )

        WithPost wp ->
            ( WithPost { wp | session = Session.updateSession wp.session maybeUser }
            , Cmd.none
            )

        Errored wp ->
            ( Errored { wp | session = Session.updateSession wp.session maybeUser }
            , Cmd.none
            )



-- GRAPHQL


fetchPost : PostId -> Cmd Msg
fetchPost postId =
    query (Query.post { id = PostId.getId postId } postWithUserSelection) FetchedPost
