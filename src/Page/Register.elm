module Page.Register exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, input)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , username : String
    , password : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, username = "", password = "" }
    , Cmd.none
    )



-- MESSAGE


type Msg
    = ChangedUsername String
    | ChangedPassword String
    | Submitted



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        inputFieldWidth =
            maximum 800 fill
    in
    { title = "Register"
    , body =
        [ layout []
            (column
                [ height fill
                , width <| maximum 800 fill
                , paddingXY 80 80
                , spacing 30
                , centerX
                ]
                [ Input.username [ Input.focusedOnLoad ]
                    { onChange = ChangedUsername
                    , text = model.username
                    , placeholder = Just (Input.placeholder [] (text "username"))
                    , label = Input.labelAbove [] (text "Username")
                    }
                , Input.newPassword []
                    { onChange = ChangedPassword
                    , text = model.password
                    , placeholder = Just (Input.placeholder [] (text "password"))
                    , label = Input.labelAbove [] (text "Password")
                    , show = model.password == ""
                    }
                , Input.button
                    [ Background.color <| rgb255 0 128 128
                    , Font.color <| rgb 1 1 1
                    , Border.rounded 4
                    , paddingXY 30 15
                    ]
                    { onPress = Just Submitted
                    , label = el [ centerX, centerY ] <| text "Register"
                    }
                ]
            )
        ]
    }



-- UPDATE


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        ChangedUsername username ->
            ( { model | username = username }, Cmd.none )

        ChangedPassword password ->
            ( { model | password = password }, Cmd.none )

        Submitted ->
            -- TODO
            ( model, Cmd.none )



-- EXPORTS


toSession : Model -> Session
toSession =
    .session
