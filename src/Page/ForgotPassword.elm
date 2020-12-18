module Page.ForgotPassword exposing (..)

import Api.Mutation as Mutation
import Browser
import Components.Button as Button
import Components.LinkButton exposing (linkButton)
import Components.UserForm exposing (userForm)
import Components.Variant as Variant
import Element exposing (..)
import Element.Input as Input
import GraphQL exposing (GraphQLResult, mutation)
import Route
import Session exposing (Session)
import User exposing (User)



-- MODEL


type Model
    = ReceivingEmail { session : Session, email : String }
    | Loading { session : Session, email : String }
    | GaveToken { session : Session, email : String }


init : Session -> ( Model, Cmd Msg )
init session =
    ( ReceivingEmail { session = session, email = "" }, Cmd.none )



-- MESSAGE


type Msg
    = ChangedEmail String
    | Submitted
    | SentToken (GraphQLResult Bool)


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case ( msg, model ) of
        ( ChangedEmail email, ReceivingEmail r ) ->
            ( ReceivingEmail { r | email = email }, Cmd.none )

        ( Submitted, ReceivingEmail r ) ->
            ( Loading { session = r.session, email = r.email }
            , sendTokenEmail (Session.apiUrl r.session) r.email
            )

        ( SentToken _, Loading l ) ->
            ( GaveToken { session = l.session, email = l.email }, Cmd.none )

        -- Invalid messages
        ( SentToken _, _ ) ->
            ( model, Cmd.none )

        ( ChangedEmail _, _ ) ->
            ( model, Cmd.none )

        ( Submitted, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        email =
            case model of
                ReceivingEmail r ->
                    r.email

                Loading l ->
                    l.email

                _ ->
                    ""
    in
    { title = "Password Recovery"
    , body =
        [ layoutWith { options = [ noStaticStyleSheet ] } [] <|
            case model of
                GaveToken g ->
                    el [ centerX, centerY ] <|
                        text "Change your password with the link sent to your email"

                _ ->
                    userForm (Just Submitted)
                        [ Input.email []
                            { onChange = ChangedEmail
                            , text = email
                            , placeholder = Just <| Input.placeholder [] (text "Email")
                            , label = Input.labelAbove [] (text "Email")
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
                                , variant = Variant.Teal
                                , state =
                                    case model of
                                        Loading _ ->
                                            Button.Loading

                                        _ ->
                                            Button.Enabled "Change Password"
                                }
                            ]
                        ]
        ]
    }



-- EXPORTS


toSession : Model -> Session
toSession model =
    case model of
        ReceivingEmail r ->
            r.session

        Loading l ->
            l.session

        GaveToken g ->
            g.session


updateSession : Model -> Maybe User -> Model
updateSession model maybeUser =
    case model of
        ReceivingEmail r ->
            ReceivingEmail { r | session = Session.updateSession r.session maybeUser }

        Loading l ->
            Loading { l | session = Session.updateSession l.session maybeUser }

        GaveToken g ->
            GaveToken { g | session = Session.updateSession g.session maybeUser }



-- GRAPHQL


sendTokenEmail : String -> String -> Cmd Msg
sendTokenEmail apiUrl email =
    mutation apiUrl (Mutation.forgotPassword { email = email }) SentToken
