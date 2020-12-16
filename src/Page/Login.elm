module Page.Login exposing (Model(..), Msg(..), init, toSession, update, updateSession, view)

import Api.Mutation as Mutation
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
import Error exposing (Error, unknown)
import GraphQL exposing (GraphQLResult, UserResult(..), mutation, userResultSelection)
import Graphql.Http
import Graphql.SelectionSet as SelectionSet
import Html exposing (Html)
import Route
import Session exposing (Session)
import User exposing (User)



-- MODEL


type Model
    = Login
        { session : Session
        , usernameOrEmail : String
        , password : String
        , errors : List Error
        }
    | Loading { session : Session, usernameOrEmail : String, password : String }
    | LoggedIn { session : Session, user : User }


init : Session -> ( Model, Cmd Msg )
init session =
    case session of
        Session.LoggedIn key user ->
            ( LoggedIn { session = session, user = user }
            , Route.replaceUrl key Route.Home
            )

        Session.Guest key ->
            ( Login
                { session = session
                , usernameOrEmail = ""
                , password = ""
                , errors = []
                }
            , Cmd.none
            )



-- MESSAGE


type Msg
    = ChangedUsername String
    | ChangedPassword String
    | Submitted
    | SentLogin (GraphQLResult UserResult)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Login"
    , body =
        let
            usernameOrEmail =
                case model of
                    Login l ->
                        l.usernameOrEmail

                    Loading l ->
                        l.usernameOrEmail

                    LoggedIn _ ->
                        ""

            password =
                case model of
                    Login l ->
                        l.password

                    Loading l ->
                        l.password

                    LoggedIn _ ->
                        ""

            errors =
                case model of
                    Login l ->
                        l.errors

                    Loading l ->
                        []

                    LoggedIn _ ->
                        []

            loading =
                case model of
                    Login _ ->
                        False

                    _ ->
                        True
        in
        [ layout [] <|
            userForm (Just Submitted)
                [ Error.viewInputWithError Input.username
                    [ Input.focusedOnLoad ]
                    { onChange = ChangedUsername
                    , text = usernameOrEmail
                    , placeholder = Just (Input.placeholder [] (text "username or email"))
                    , label = Input.labelAbove [] (text "Username or Email")
                    }
                    (List.filter
                        (\e -> Error.isUsernameError e || Error.isEmailError e)
                        errors
                    )
                , Error.viewInputWithError Input.currentPassword
                    []
                    { onChange = ChangedPassword
                    , text = password
                    , placeholder = Just (Input.placeholder [] (text "password"))
                    , label = Input.labelAbove [] (text "Password")
                    , show = password == ""
                    }
                    (List.filter Error.isPasswordError errors)
                , row [ width fill, spaceEvenly ]
                    [ linkButton
                        { route = Route.Home
                        , variant = Variant.Gray
                        , label = text "Go to Home"
                        }
                    , linkButton
                        { route = Route.ForgotPassword
                        , variant = Variant.Teal
                        , label = text "Forgot Password"
                        }
                    , Button.button
                        { onClick = Just Submitted
                        , variant = Variant.Green
                        , state =
                            if loading then
                                Button.Loading

                            else
                                Button.Enabled "Log In"
                        }
                    ]
                ]
        ]
    }



-- UPDATE


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( ChangedUsername usernameOrEmail, Login login ) ->
            ( Login { login | usernameOrEmail = usernameOrEmail }, Cmd.none )

        ( ChangedPassword password, Login login ) ->
            ( Login { login | password = password }, Cmd.none )

        ( Submitted, Login { session, usernameOrEmail, password } ) ->
            ( Loading
                { session = session, usernameOrEmail = usernameOrEmail, password = password }
            , loginUser
                { usernameOrEmail = usernameOrEmail, password = password }
            )

        ( SentLogin res, Loading l ) ->
            case res of
                Ok (WithUser user) ->
                    ( LoggedIn
                        { session =
                            Session.LoggedIn (Session.navKey l.session) user
                        , user = user
                        }
                      -- , Route.replaceUrl (Session.navKey l.session) Route.Home
                    , Route.previousPage (Session.navKey l.session)
                    )

                Ok (WithError errors) ->
                    ( Login
                        { session = l.session
                        , usernameOrEmail = l.usernameOrEmail
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

        -- Invalid states
        ( ChangedUsername _, _ ) ->
            ( model, Cmd.none )

        ( ChangedPassword _, _ ) ->
            ( model, Cmd.none )

        ( Submitted, _ ) ->
            ( model, Cmd.none )

        ( SentLogin _, _ ) ->
            ( model, Cmd.none )



-- EXPORTS


toSession : Model -> Session
toSession m =
    case m of
        Login { session } ->
            session

        Loading { session } ->
            session

        LoggedIn { session } ->
            session


updateSession : Model -> Maybe User -> ( Model, Cmd Msg )
updateSession model maybeUser =
    let
        makeLoggedIn session user =
            let
                newSession =
                    Session.updateSession session (Just user)
            in
            ( LoggedIn
                { session = newSession
                , user = user
                }
            , Route.previousPage (Session.navKey newSession)
            )
    in
    case ( model, maybeUser ) of
        ( Login l, Just user ) ->
            makeLoggedIn l.session user

        ( Login l, Nothing ) ->
            ( Login
                { l | session = Session.updateSession l.session maybeUser }
            , Cmd.none
            )

        ( Loading l, Just user ) ->
            makeLoggedIn l.session user

        ( Loading l, Nothing ) ->
            ( Loading { l | session = Session.updateSession l.session maybeUser }
            , Cmd.none
            )

        ( LoggedIn l, Just user ) ->
            makeLoggedIn l.session user

        ( LoggedIn l, Nothing ) ->
            ( Login
                { session = Session.updateSession l.session maybeUser
                , usernameOrEmail = ""
                , password = ""
                , errors = []
                }
            , Cmd.none
            )



-- GRAPHQL


loginUser : { usernameOrEmail : String, password : String } -> Cmd Msg
loginUser options =
    mutation (Mutation.login options userResultSelection) SentLogin
