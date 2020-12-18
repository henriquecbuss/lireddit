module Page.EditPost exposing (..)

import Api.Mutation as Mutation
import Browser
import Components.Button as Button
import Components.LinkButton exposing (linkButton)
import Components.UserForm exposing (userForm)
import Components.Variant as Variant
import Element exposing (..)
import Element.Input as Input
import GraphQL exposing (GraphQLResult, mutation, postSelection)
import Post exposing (Post, PostWithUser)
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
    | Editing { session : Session, post : PostWithUser, title : String, text : String }
    | Submitting { session : Session }
    | SubmittedPost { session : Session, post : Post }
    | Errored { session : Session, message : String }


init : Session -> PostId -> ( Model, Cmd Msg )
init session postId =
    ( Loading { session = session, postId = postId }
    , GraphQL.getPost (Session.apiUrl session) postId GotPost
    )



-- MESSAGE


type Msg
    = GotPost (GraphQLResult (Maybe PostWithUser))
    | ChangedTitle String
    | ChangedText String
    | Submitted
    | UpdatedPost (GraphQLResult (Maybe Post))


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( GotPost (Ok maybePost), Loading l ) ->
            case maybePost of
                Just post ->
                    let
                        userId =
                            l.session
                                |> Session.getUser
                                |> Maybe.map .id
                    in
                    if Just post.creator.id == userId then
                        ( Editing
                            { session = l.session
                            , post = post
                            , title = post.title
                            , text = post.text
                            }
                        , Cmd.none
                        )

                    else
                        ( Errored
                            { session = l.session
                            , message = "You need to be the owner of the post to edit it"
                            }
                        , Cmd.none
                        )

                Nothing ->
                    ( Errored
                        { session = l.session
                        , message = "Could not find your post"
                        }
                    , Cmd.none
                    )

        ( GotPost _, Loading l ) ->
            ( Errored { session = l.session, message = "There was an error getting your post" }, Cmd.none )

        ( ChangedTitle newTitle, Editing e ) ->
            ( Editing { e | title = newTitle }, Cmd.none )

        ( ChangedText newText, Editing e ) ->
            ( Editing { e | text = newText }, Cmd.none )

        ( Submitted, Editing e ) ->
            ( Submitting { session = e.session }
            , updatePost (Session.apiUrl e.session)
                { id = e.post.id, title = e.title, text = e.text }
            )

        ( UpdatedPost (Ok maybePost), Submitting s ) ->
            case maybePost of
                Just post ->
                    ( SubmittedPost { session = s.session, post = post }
                    , Route.previousPage (Session.navKey s.session)
                    )

                Nothing ->
                    ( Errored { session = s.session, message = "Could not edit your post" }, Cmd.none )

        ( UpdatedPost _, Submitting s ) ->
            ( Errored { session = s.session, message = "There was an error updating your post" }, Cmd.none )

        -- Invalid messages
        ( GotPost _, _ ) ->
            ( model, Cmd.none )

        ( ChangedTitle _, _ ) ->
            ( model, Cmd.none )

        ( ChangedText _, _ ) ->
            ( model, Cmd.none )

        ( Submitted, _ ) ->
            ( model, Cmd.none )

        ( UpdatedPost _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Loading _ ->
            { title = "Loading"
            , body =
                [ layoutWith { options = [ noStaticStyleSheet ] } [] <|
                    el [ centerX, centerY ] <|
                        text "Loading"
                ]
            }

        Editing e ->
            { title = "Editing " ++ e.title
            , body =
                [ layoutWith { options = [ noStaticStyleSheet ] } [] <|
                    userForm Nothing
                        [ Input.text []
                            { onChange = ChangedTitle
                            , text = e.title
                            , placeholder = Just <| Input.placeholder [] <| text "title"
                            , label = Input.labelAbove [] <| text "Title"
                            }
                        , Input.multiline [ height <| px 300 ]
                            { onChange = ChangedText
                            , text = e.text
                            , placeholder = Just <| Input.placeholder [] <| text "Lorem ipsum"
                            , label = Input.labelAbove [] <| text "Edit your post"
                            , spellcheck = True
                            }
                        , row [ width fill, spaceEvenly ]
                            [ linkButton []
                                { route = Route.Home
                                , variant = Variant.Gray
                                , label = text "Go to Home"
                                }
                            , Button.button []
                                { onClick = Just Submitted
                                , variant = Variant.Green
                                , state = Button.Enabled "Confirm Changes"
                                }
                            ]
                        ]
                ]
            }

        Submitting s ->
            { title = "Submiting"
            , body = [ layoutWith { options = [ noStaticStyleSheet ] } [] <| text "Your post is being submitted" ]
            }

        SubmittedPost sp ->
            { title = "Success!"
            , body = [ layoutWith { options = [ noStaticStyleSheet ] } [] <| text "Your post has been updated!" ]
            }

        Errored { message } ->
            { title = "Something went wrong"
            , body =
                [ layoutWith { options = [ noStaticStyleSheet ] } [] <|
                    column [ centerX, centerY, spacing 50 ]
                        [ text message
                        , linkButton [ centerX ]
                            { route = Route.Home
                            , variant = Variant.Gray
                            , label = text "Go to Home"
                            }
                        ]
                ]
            }



-- EXPORTS


toSession : Model -> Session
toSession model =
    case model of
        Loading l ->
            l.session

        Editing e ->
            e.session

        Submitting s ->
            s.session

        SubmittedPost sp ->
            sp.session

        Errored e ->
            e.session


updateSession : Model -> Maybe User -> ( Model, Cmd Msg )
updateSession model maybeUser =
    case model of
        Loading l ->
            ( Loading { l | session = Session.updateSession l.session maybeUser }
            , Cmd.none
            )

        Editing e ->
            ( Editing { e | session = Session.updateSession e.session maybeUser }
            , Cmd.none
            )

        Submitting s ->
            ( Submitting { s | session = Session.updateSession s.session maybeUser }
            , Cmd.none
            )

        SubmittedPost sp ->
            ( SubmittedPost { sp | session = Session.updateSession sp.session maybeUser }
            , Cmd.none
            )

        Errored e ->
            ( Errored { e | session = Session.updateSession e.session maybeUser }
            , Cmd.none
            )



-- GRAPHQL


updatePost : String -> { id : PostId, title : String, text : String } -> Cmd Msg
updatePost apiUrl { id, title, text } =
    mutation apiUrl
        (Mutation.updatePost { id = PostId.getId id, title = title, text = text }
            postSelection
        )
        UpdatedPost
