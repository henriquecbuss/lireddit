module Page.Register exposing (Model(..), Msg(..), init, toSession, update, updateSession, view)

import Api.Mutation as Mutation
import Api.Object
import Api.Object.FieldError as FieldError
import Api.Object.User as User
import Api.Object.UserResponse as UserResponse
import Browser
import Components.Button as Button
import Components.LinkButton exposing (linkButton)
import Components.UserForm exposing (userForm)
import Components.Variant as Variant
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Error exposing (Error, isEmailError, isPasswordError, isUsernameError, unknown)
import GraphQL exposing (GraphQLResult, UserResult(..), mutation, userResultSelection)
import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Route
import Session exposing (Session)
import User exposing (User)



-- MODEL


type Model
    = Registering
        { session : Session
        , username : String
        , email : String
        , password : String
        , errors : List Error
        }
    | Loading
        { session : Session
        , username : String
        , email : String
        , password : String
        }
    | Registered { session : Session, user : User }


init : Session -> ( Model, Cmd Msg )
init session =
    case Session.getUser session of
        Just user ->
            ( Registered { session = session, user = user }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        Nothing ->
            ( Registering
                { session = session
                , username = ""
                , email = ""
                , password = ""
                , errors = []
                }
            , Cmd.none
            )



-- MESSAGE


type Msg
    = ChangedUsername String
    | ChangedEmail String
    | ChangedPassword String
    | Submitted
    | SentRegistration (GraphQLResult UserResult)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        username =
            case model of
                Registering r ->
                    r.username

                Loading l ->
                    l.username

                _ ->
                    ""

        email =
            case model of
                Registering r ->
                    r.email

                Loading l ->
                    l.email

                _ ->
                    ""

        password =
            case model of
                Registering r ->
                    r.password

                Loading l ->
                    l.password

                _ ->
                    ""

        errs =
            case model of
                Registering { errors } ->
                    errors

                _ ->
                    []

        disabled =
            case model of
                Loading _ ->
                    True

                Registered _ ->
                    True

                _ ->
                    False
    in
    { title = "Register"
    , body =
        case model of
            Registered { user } ->
                [ layoutWith { options = [ noStaticStyleSheet ] } [] (text <| "Registered: " ++ user.username) ]

            _ ->
                [ layoutWith { options = [ noStaticStyleSheet ] } [] <|
                    userForm (Just Submitted)
                        [ Error.viewInputWithError Input.username
                            [ Input.focusedOnLoad ]
                            { onChange = ChangedUsername
                            , text = username
                            , placeholder =
                                Just (Input.placeholder [] (text "username"))
                            , label = Input.labelAbove [] (text "Username")
                            }
                            (List.filter isUsernameError errs)
                        , Error.viewInputWithError Input.email
                            []
                            { onChange = ChangedEmail
                            , text = email
                            , placeholder = Just (Input.placeholder [] (text "email"))
                            , label = Input.labelAbove [] (text "Email")
                            }
                            (List.filter isEmailError errs)
                        , Error.viewInputWithError Input.newPassword
                            []
                            { onChange = ChangedPassword
                            , text = password
                            , placeholder = Just (Input.placeholder [] (text "password"))
                            , label = Input.labelAbove [] (text "Password")
                            , show = password == ""
                            }
                            (List.filter isPasswordError errs)
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
                                , variant = Variant.Teal
                                , state =
                                    if disabled then
                                        Button.Loading

                                    else
                                        Button.Enabled "Register"
                                }
                            ]
                        ]
                ]
    }



-- UPDATE


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( ChangedUsername username, Registering registering ) ->
            ( Registering { registering | username = username }, Cmd.none )

        ( ChangedEmail email, Registering registering ) ->
            ( Registering { registering | email = email }, Cmd.none )

        ( ChangedPassword password, Registering registering ) ->
            ( Registering { registering | password = password }, Cmd.none )

        ( Submitted, Registering { session, username, email, password } ) ->
            ( Loading
                { session = session
                , username = username
                , email = email
                , password = password
                }
            , registerUser
                (Session.apiUrl session)
                { options =
                    { username = username
                    , email = email
                    , password = password
                    }
                }
            )

        ( SentRegistration res, Loading l ) ->
            case res of
                Ok (WithUser user) ->
                    ( Registered { session = Session.logIn l.session user, user = user }
                    , Route.previousPage (Session.navKey l.session)
                    )

                Ok (WithError errors) ->
                    ( Registering
                        { session = l.session
                        , username = l.username
                        , email = l.email
                        , password = l.password
                        , errors =
                            if List.isEmpty errors then
                                [ unknown ]

                            else
                                List.map Error.fromRecord errors
                        }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        -- Disregard invalid states
        _ ->
            ( model, Cmd.none )



-- EXPORTS


toSession : Model -> Session
toSession m =
    case m of
        Registering r ->
            r.session

        Loading l ->
            l.session

        Registered r ->
            r.session


updateSession : Model -> Maybe User -> ( Model, Cmd Msg )
updateSession model maybeUser =
    let
        makeRegistered session user =
            let
                newSession =
                    Session.updateSession session (Just user)
            in
            ( Registered
                { session = Session.updateSession session (Just user)
                , user = user
                }
            , Route.previousPage (Session.navKey newSession)
            )
    in
    case ( model, maybeUser ) of
        ( Registering r, Just user ) ->
            makeRegistered r.session user

        ( Registering r, Nothing ) ->
            ( Registering
                { r | session = Session.updateSession r.session maybeUser }
            , Cmd.none
            )

        ( Loading l, Just user ) ->
            makeRegistered l.session user

        ( Loading l, Nothing ) ->
            ( Loading { l | session = Session.updateSession l.session maybeUser }
            , Cmd.none
            )

        ( Registered r, Just user ) ->
            makeRegistered r.session user

        ( Registered r, Nothing ) ->
            ( Registering
                { session = Session.updateSession r.session maybeUser
                , username = ""
                , email = ""
                , password = ""
                , errors = []
                }
            , Cmd.none
            )



-- GRAPHQL


registerUser :
    String
    -> { options : { username : String, email : String, password : String } }
    -> Cmd Msg
registerUser apiUrl options =
    mutation apiUrl (Mutation.register options userResultSelection) SentRegistration
