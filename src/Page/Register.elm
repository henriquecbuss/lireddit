module Page.Register exposing (Model(..), Msg(..), init, toSession, update, view)

import Api.Mutation as Mutation
import Api.Object
import Api.Object.FieldError as FieldError
import Api.Object.User as User
import Api.Object.UserResponse as UserResponse
import Browser
import Components.UserForm exposing (Variant(..), userForm)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Error exposing (Error, unknown)
import GraphQL exposing (UserResult(..), mutation, userInfoSelection)
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
        , password : String
        , errors : List Error
        }
    | Loading { session : Session, username : String, password : String }
    | Registered { session : Session, user : User }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Registering
        { session = session
        , username = ""
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
    | SentRegistration (Result (Graphql.Http.Error UserResult) UserResult)



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
                [ layout [] (text <| "Registered: " ++ user.username) ]

            _ ->
                [ layout [] <|
                    userForm
                        { onUsernameChange = ChangedUsername
                        , usernameText = username
                        , errors = errs
                        , newPassword = True
                        , onPasswordChange = ChangedPassword
                        , passwordText = password
                        , variant = Teal
                        , onSubmit = Submitted
                        , buttonLabel = "Sign Up"
                        , loading = disabled
                        }
                ]
    }



-- UPDATE


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( ChangedUsername username, Registering registering ) ->
            ( Registering { registering | username = username }, Cmd.none )

        ( ChangedPassword password, Registering registering ) ->
            ( Registering { registering | password = password }, Cmd.none )

        ( Submitted, Registering { session, username, password } ) ->
            ( Loading
                { session = session
                , username = username
                , password = password
                }
            , registerUser
                { options =
                    { username = username
                    , password = password
                    }
                }
            )

        ( SentRegistration res, Loading l ) ->
            case res of
                Ok (WithUser user) ->
                    ( Registered
                        { session =
                            Session.LoggedIn (Session.navKey l.session)
                                user
                        , user = user
                        }
                    , Route.replaceUrl (Session.navKey l.session) Route.Home
                    )

                Ok (WithError errors) ->
                    ( Registering
                        { session = l.session
                        , username = l.username
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



-- GRAPHQL


registerUser : { options : { username : String, password : String } } -> Cmd Msg
registerUser options =
    mutation (Mutation.register options userInfoSelection) SentRegistration
