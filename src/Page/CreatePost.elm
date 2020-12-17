module Page.CreatePost exposing (..)

import Api.Mutation as Mutation
import Browser
import Components.Button as Button
import Components.LinkButton exposing (linkButton)
import Components.UserForm exposing (userForm)
import Components.Variant as Variant
import Element exposing (..)
import Element.Input as Input
import GraphQL exposing (GraphQLResult, mutation, postSelection)
import Post exposing (Post)
import Route
import Session as Session exposing (Session)
import User exposing (User)



-- MODEL


type Model
    = Posting { session : Session, title : String, text : String }
    | Loading { session : Session, title : String, text : String }
    | Posted { session : Session, post : Post }
    | Errored { session : Session }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Posting { session = session, title = "", text = "" }, Cmd.none )



-- MESSAGE


type Msg
    = ChangedTitle String
    | ChangedText String
    | Submitted
    | CreatedPost (GraphQLResult Post)


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( ChangedTitle title, Posting p ) ->
            ( Posting { p | title = title }, Cmd.none )

        ( ChangedText text, Posting p ) ->
            ( Posting { p | text = text }, Cmd.none )

        ( Submitted, Posting p ) ->
            ( Loading p, createPost { options = { title = p.title, text = p.text } } )

        ( CreatedPost res, Loading l ) ->
            case res of
                Ok post ->
                    ( Posted { session = l.session, post = post }
                    , Route.replaceUrl (Session.navKey l.session) Route.Home
                    )

                Err e ->
                    ( Errored { session = l.session }, Cmd.none )

        -- Invalid messages
        ( ChangedTitle _, _ ) ->
            ( model, Cmd.none )

        ( ChangedText _, _ ) ->
            ( model, Cmd.none )

        ( Submitted, _ ) ->
            ( model, Cmd.none )

        ( CreatedPost _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        title =
            case model of
                Posting p ->
                    p.title

                Loading l ->
                    l.title

                _ ->
                    ""

        postText =
            case model of
                Posting p ->
                    p.text

                Loading l ->
                    l.text

                _ ->
                    ""
    in
    { title = "Create Post"
    , body =
        [ layoutWith { options = [ noStaticStyleSheet ] } [] <|
            case model of
                Posted _ ->
                    el [ centerX, centerY ] <|
                        text "You created a post!"

                Errored _ ->
                    el [ centerX, centerY ] <|
                        text "Something went wrong :/"

                _ ->
                    case toSession model of
                        Session.Guest _ ->
                            column [ width fill, centerY, moveUp 100, spacing 80 ]
                                [ el [ centerX ] <| text "You need to log in to create a post"
                                , row [ width fill, spacing 80 ]
                                    [ linkButton
                                        [ centerX ]
                                        { route = Route.Register
                                        , variant = Variant.Teal
                                        , label = text "Register"
                                        }
                                    , linkButton
                                        [ centerX ]
                                        { route = Route.Login
                                        , variant = Variant.Green
                                        , label = text "Log In"
                                        }
                                    ]
                                ]

                        _ ->
                            userForm Nothing
                                [ Input.text []
                                    { onChange = ChangedTitle
                                    , text = title
                                    , placeholder =
                                        Just <|
                                            Input.placeholder []
                                                (text "title")
                                    , label = Input.labelAbove [] (text "Title")
                                    }
                                , Input.multiline [ height <| px 300 ]
                                    { onChange = ChangedText
                                    , text = postText
                                    , placeholder =
                                        Just <|
                                            Input.placeholder []
                                                (text "Lorem ipsum")
                                    , label = Input.labelAbove [] (text "Create your post")
                                    , spellcheck = True
                                    }
                                , row [ width fill, spaceEvenly ]
                                    [ linkButton
                                        []
                                        { route = Route.Home
                                        , variant = Variant.Gray
                                        , label = text "Go to Home"
                                        }
                                    , Button.button
                                        []
                                        { onClick = Just Submitted
                                        , variant = Variant.Green
                                        , state = Button.Enabled "Create Post"
                                        }
                                    ]
                                ]
        ]
    }



-- EXPORTS


toSession : Model -> Session
toSession model =
    case model of
        Posting p ->
            p.session

        Loading l ->
            l.session

        Posted p ->
            p.session

        Errored e ->
            e.session


updateSession : Model -> Maybe User -> ( Model, Cmd Msg )
updateSession model maybeUser =
    let
        updatedSession session =
            Session.updateSession session maybeUser
    in
    case model of
        Posting p ->
            ( Posting { p | session = updatedSession p.session }, Cmd.none )

        Loading l ->
            ( Loading { l | session = updatedSession l.session }, Cmd.none )

        Posted p ->
            ( Posted { p | session = updatedSession p.session }, Cmd.none )

        Errored e ->
            ( Errored { e | session = updatedSession e.session }, Cmd.none )



-- GRAPHQL


createPost : { options : { title : String, text : String } } -> Cmd Msg
createPost options =
    mutation (Mutation.createPost options postSelection) CreatedPost
