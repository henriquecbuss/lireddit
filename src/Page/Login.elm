module Page.Login exposing (Model(..), Msg(..), init, toSession, update, view)

import Api.Mutation as Mutation
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
import Html exposing (Html)
import Route
import Session exposing (Session)
import User exposing (User)



-- MODEL


type Model
    = Login
        { session : Session
        , username : String
        , password : String
        , errors : List Error
        }
    | Loading { session : Session, username : String, password : String }
    | LoggedIn { session : Session, user : User }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Login { session = session, username = "", password = "", errors = [] }
    , Cmd.none
    )



-- MESSAGE


type Msg
    = ChangedUsername String
    | ChangedPassword String
    | Submitted
    | SentLogin (Result (Graphql.Http.Error UserResult) UserResult)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Login"
    , body =
        let
            username =
                case model of
                    Login l ->
                        l.username

                    Loading l ->
                        l.username

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
            userForm
                { onUsernameChange = ChangedUsername
                , usernameText = username
                , errors = errors
                , newPassword = False
                , onPasswordChange = ChangedPassword
                , passwordText = password
                , variant = Green
                , onSubmit = Submitted
                , buttonLabel = "Sign In"
                , loading = loading
                }
        ]
    }



-- UPDATE


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( ChangedUsername username, Login login ) ->
            ( Login { login | username = username }, Cmd.none )

        ( ChangedPassword password, Login login ) ->
            ( Login { login | password = password }, Cmd.none )

        ( Submitted, Login { session, username, password } ) ->
            ( Loading
                { session = session, username = username, password = password }
            , loginUser
                { options = { username = username, password = password } }
            )

        ( SentLogin res, Loading l ) ->
            case res of
                Ok (WithUser user) ->
                    ( LoggedIn
                        { session =
                            Session.LoggedIn (Session.navKey l.session) user
                        , user = user
                        }
                    , Route.replaceUrl (Session.navKey l.session) Route.Home
                    )

                Ok (WithError errors) ->
                    ( Login
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

        -- Invalid states
        ( ChangedUsername _, Loading _ ) ->
            ( model, Cmd.none )

        ( ChangedPassword _, Loading _ ) ->
            ( model, Cmd.none )

        ( Submitted, Loading _ ) ->
            ( model, Cmd.none )

        ( SentLogin _, Login _ ) ->
            ( model, Cmd.none )

        ( _, LoggedIn _ ) ->
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



-- GRAPHQL


loginUser : { options : { username : String, password : String } } -> Cmd Msg
loginUser options =
    mutation (Mutation.login options userInfoSelection) SentLogin
