module Page.Post exposing (..)

import Api.Mutation as Mutation
import Api.Query as Query
import Browser
import Element exposing (..)
import Element.Font as Font
import GraphQL exposing (GraphQLResult, postWithUserSelection, query)
import Graphql.Http
import Html
import Post exposing (PostWithUser)
import Post.PostId as PostId exposing (PostId)
import Session exposing (Session)
import User exposing (User)



-- MODEL


type Model
    = Loading
        { session : Session
        , postId : PostId
        }
    | WithPost
        { session : Session
        , post : PostWithUser
        }
    | Errored
        { session : Session
        , message : String
        }


init : Session -> PostId -> ( Model, Cmd Msg )
init session postId =
    ( Loading
        { session = session
        , postId = postId
        }
    , GraphQL.getPost postId GotPost
    )



-- MESSAGE


type Msg
    = GotPost (GraphQLResult (Maybe PostWithUser))


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( GotPost (Ok maybePost), Loading l ) ->
            case maybePost of
                Just post ->
                    ( WithPost
                        { session = l.session
                        , post = post
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( Errored
                        { session = l.session
                        , message = "Could not find post"
                        }
                    , Cmd.none
                    )

        ( GotPost (Err e), Loading l ) ->
            ( Errored
                { session = l.session
                , message = "Unknown error"
                }
            , Cmd.none
            )

        -- Invalid messages
        ( GotPost _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        postView children =
            [ layout [] <|
                column [ width <| maximum 800 fill, centerX, spacing 50 ]
                    children
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
                    [ column [ spacing 10, centerX, centerY, width <| maximum 800 fill ]
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
