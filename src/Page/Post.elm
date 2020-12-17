module Page.Post exposing (..)

import Api.Mutation as Mutation
import Browser
import Components.Button as Button
import Components.LinkButton exposing (linkButton)
import Components.Variant as Variant
import Element exposing (..)
import Element.Font as Font
import GraphQL exposing (GraphQLResult, mutation, postWithUserSelection, query)
import Graphql.Http
import Html exposing (Html)
import Post exposing (PostWithUser)
import Post.PostId as PostId exposing (PostId)
import Route
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
    | Deleting { session : Session, post : PostWithUser }
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
    | RequestedDelete
    | DeletedPost (GraphQLResult Bool)


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

        ( RequestedDelete, WithPost wp ) ->
            ( Deleting wp, deletePost wp.post.id )

        ( DeletedPost (Ok True), _ ) ->
            ( model, Route.previousPage (Session.navKey <| toSession model) )

        ( DeletedPost (Ok False), _ ) ->
            ( Errored { session = toSession model, message = "There was an error deleting the post" }
            , Cmd.none
            )

        -- Invalid messages
        ( GotPost _, _ ) ->
            ( model, Cmd.none )

        ( RequestedDelete, _ ) ->
            ( model, Cmd.none )

        ( DeletedPost _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Loading _ ->
            { title = "Loading post"
            , body = defaultView [ text "Loading" ]
            }

        WithPost { session, post } ->
            viewPost session post False

        Deleting { session, post } ->
            viewPost session post True

        Errored e ->
            { title = "Error"
            , body = defaultView [ text e.message ]
            }


defaultView : List (Element Msg) -> List (Html Msg)
defaultView children =
    [ layoutWith { options = [ noStaticStyleSheet ] } [] <|
        column [ width <| maximum 1000 fill, centerX, spacing 50 ]
            children
    ]


viewPost : Session -> PostWithUser -> Bool -> Browser.Document Msg
viewPost session post isDeleting =
    let
        isOwner =
            case session of
                Session.LoggedIn _ user ->
                    user.id == post.creator.id

                _ ->
                    False
    in
    { title = post.title
    , body =
        defaultView
            [ row [ width fill ]
                [ column [ spacing 10, centerY, width fill ]
                    [ el [ Font.bold, Font.size 36 ] <| text post.title
                    , paragraph [ Font.color <| rgb 0.7 0.7 0.7, Font.size 18 ]
                        [ text "written by "
                        , el [ Font.bold ] <| text post.creator.username
                        ]
                    ]
                , if isOwner then
                    column [ spacing 20 ]
                        [ linkButton [ width fill ]
                            { route = Route.EditPost post.id
                            , variant = Variant.Gray
                            , label = el [ centerX ] <| text "Edit Post"
                            }
                        , Button.button [ width fill ]
                            { onClick =
                                if isDeleting then
                                    Nothing

                                else
                                    Just RequestedDelete
                            , variant = Variant.Gray
                            , state =
                                if isDeleting then
                                    Button.Loading

                                else
                                    Button.Enabled "Delete Post"
                            }
                        ]

                  else
                    none
                ]
            , paragraph [] [ text post.text ]
            ]
    }



-- EXPORTS


toSession : Model -> Session
toSession model =
    case model of
        Loading l ->
            l.session

        WithPost wp ->
            wp.session

        Deleting d ->
            d.session

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

        Deleting d ->
            ( Deleting { d | session = Session.updateSession d.session maybeUser }
            , Cmd.none
            )

        Errored wp ->
            ( Errored { wp | session = Session.updateSession wp.session maybeUser }
            , Cmd.none
            )



-- GRAPHQL


deletePost : PostId -> Cmd Msg
deletePost postId =
    mutation
        (Mutation.deletePost { id = PostId.getId postId })
        DeletedPost
