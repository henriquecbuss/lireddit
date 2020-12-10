module Page.Register exposing (Model(..), Msg(..), init, toSession, update, view)

import Api.Mutation as Mutation
import Api.Object
import Api.Object.FieldError as FieldError
import Api.Object.User as User
import Api.Object.UserResponse as UserResponse
import Browser
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
        inputFieldWidth =
            maximum 800 fill

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

        showPassword =
            case model of
                Registering r ->
                    r.password == ""

                _ ->
                    False

        buttonContent =
            case model of
                Loading _ ->
                    text "Loading"

                _ ->
                    text "Register"
    in
    { title = "Register"
    , body =
        case model of
            Registered { user } ->
                [ layout [] (text <| "Registered: " ++ user.username) ]

            _ ->
                [ layout []
                    (column
                        [ height fill
                        , width <| maximum 800 fill
                        , paddingXY 80 80
                        , spacing 30
                        , centerX
                        ]
                        [ Error.viewInputWithError Input.username
                            [ Input.focusedOnLoad ]
                            { onChange = ChangedUsername
                            , text = username
                            , placeholder = Just (Input.placeholder [] (text "username"))
                            , label = Input.labelAbove [] (text "Username")
                            }
                            (List.filter Error.isUsernameError errs)
                        , Error.viewInputWithError Input.newPassword
                            []
                            { onChange = ChangedPassword
                            , text = password
                            , placeholder = Just (Input.placeholder [] (text "password"))
                            , label = Input.labelAbove [] (text "Password")
                            , show = showPassword
                            }
                            (List.filter Error.isPasswordError errs)
                        , Input.button
                            [ Background.color <|
                                if disabled then
                                    rgb255 0 100 100

                                else
                                    rgb255 0 128 128
                            , Font.color <|
                                if disabled then
                                    rgb 0.9 0.9 0.9

                                else
                                    rgb 1 1 1
                            , Border.rounded 4
                            , paddingXY 30 15
                            ]
                            { onPress =
                                if disabled then
                                    Nothing

                                else
                                    Just Submitted
                            , label = el [ centerX, centerY ] buttonContent
                            }
                        ]
                    )
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
