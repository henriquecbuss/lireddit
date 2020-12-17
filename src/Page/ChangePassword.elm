module Page.ChangePassword exposing (..)

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
import Error exposing (Error, isPasswordError, isTokenError, viewError)
import GraphQL exposing (GraphQLResult, UserResult(..), mutation, userResultSelection)
import Html exposing (th)
import Route
import Session exposing (Session)
import Token exposing (Token)
import User exposing (User)



-- MODEL


type Model
    = Changing
        { session : Session
        , token : Token
        , password : String
        , errors : List Error
        }
    | Loading { session : Session, token : Token, password : String }
    | Changed { session : Session, token : Token }


init : Session -> Token -> ( Model, Cmd Msg )
init session token =
    ( Changing
        { session = session
        , token = token
        , password = ""
        , errors = []
        }
    , Cmd.none
    )



-- MESSAGE


type Msg
    = PasswordChanged String
    | Submitted
    | GotResult (GraphQLResult UserResult)


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( PasswordChanged password, Changing c ) ->
            ( Changing { c | password = password }, Cmd.none )

        ( PasswordChanged _, _ ) ->
            ( model, Cmd.none )

        ( Submitted, Changing c ) ->
            ( Loading
                { session = c.session, token = c.token, password = c.password }
            , changePassword { newPassword = c.password, token = c.token }
            )

        ( Submitted, _ ) ->
            ( model, Cmd.none )

        ( GotResult res, Loading l ) ->
            case res of
                Ok (WithUser user) ->
                    ( Changed
                        { session = Session.updateSession l.session (Just user)
                        , token = l.token
                        }
                    , Route.replaceUrl (Session.navKey l.session) Route.Home
                    )

                Ok (WithError errors) ->
                    ( Changing
                        { session = l.session
                        , token = l.token
                        , password = l.password
                        , errors = List.map Error.fromRecord errors
                        }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ( GotResult _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        password =
            case model of
                Changing c ->
                    c.password

                Loading l ->
                    l.password

                Changed _ ->
                    ""

        errors =
            case model of
                Changing c ->
                    c.errors

                _ ->
                    []

        buttonState =
            case model of
                Loading l ->
                    Button.Loading

                _ ->
                    Button.Enabled "Change Password"
    in
    { title = "Password Recovery"
    , body =
        [ layoutWith { options = [ noStaticStyleSheet ] } [] <|
            case model of
                Changed c ->
                    el [ centerX, centerY ] (text "Password Changed!")

                _ ->
                    column [ width fill ]
                        [ userForm
                            (Just Submitted)
                            [ Error.viewInputWithError Input.newPassword
                                [ Input.focusedOnLoad ]
                                { onChange = PasswordChanged
                                , text = password
                                , placeholder =
                                    Just <|
                                        Input.placeholder []
                                            (text "new password")
                                , label = Input.labelAbove [] (text "New Password")
                                , show = password == ""
                                }
                                (List.filter isPasswordError errors)
                            , if List.isEmpty errors then
                                none

                              else
                                column [ spacing 10 ] <|
                                    List.filterMap
                                        (\err ->
                                            if isTokenError err then
                                                Just (viewError err)

                                            else
                                                Nothing
                                        )
                                        errors
                                        ++ [ Route.linkToRoute [ Font.color <| rgb255 0 128 128 ]
                                                { route = Route.ForgotPassword
                                                , label = text "Get a new token"
                                                }
                                           ]
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
                                    , state = buttonState
                                    }
                                ]
                            ]
                        ]
        ]
    }



-- EXPORTS


toSession : Model -> Session
toSession model =
    case model of
        Changing { session } ->
            session

        Loading { session } ->
            session

        Changed { session } ->
            session


updateSession : Model -> Maybe User -> Model
updateSession model maybeUser =
    case model of
        Changing c ->
            Changing { c | session = Session.updateSession c.session maybeUser }

        Loading l ->
            Loading { l | session = Session.updateSession l.session maybeUser }

        Changed c ->
            Changed { c | session = Session.updateSession c.session maybeUser }



-- GRAPHQL


changePassword : { newPassword : String, token : String } -> Cmd Msg
changePassword options =
    mutation (Mutation.changePassword options userResultSelection) GotResult
