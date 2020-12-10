module Components.UserForm exposing (Variant(..), userForm)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Error exposing (isPasswordError)
import Html exposing (th)


type Variant
    = Teal
    | Green


userForm :
    { onUsernameChange : String -> msg
    , usernameText : String
    , errors : List Error.Error
    , newPassword : Bool
    , onPasswordChange : String -> msg
    , passwordText : String
    , variant : Variant
    , onSubmit : msg
    , buttonLabel : String
    , loading : Bool
    }
    -> Element msg
userForm options =
    let
        passwordField =
            if options.newPassword then
                Input.newPassword

            else
                Input.currentPassword

        buttonText =
            if options.loading then
                "Loading"

            else
                options.buttonLabel

        buttonBg =
            case options.variant of
                Teal ->
                    if options.loading then
                        rgb255 0 100 100

                    else
                        rgb255 0 128 128

                Green ->
                    if options.loading then
                        rgb255 0 112 84

                    else
                        rgb255 0 170 128

        buttonFg =
            if options.loading then
                rgb 0.9 0.9 0.9

            else
                rgb 1 1 1
    in
    column
        [ height fill
        , width <| maximum 800 fill
        , padding 80
        , spacing 30
        , centerX
        ]
        [ Error.viewInputWithError Input.username
            [ Input.focusedOnLoad ]
            { onChange = options.onUsernameChange
            , text = options.usernameText
            , placeholder = Just (Input.placeholder [] (text "username"))
            , label = Input.labelAbove [] (text "Username")
            }
            (List.filter Error.isUsernameError options.errors)
        , Error.viewInputWithError passwordField
            []
            { onChange = options.onPasswordChange
            , text = options.passwordText
            , placeholder = Just (Input.placeholder [] (text "password"))
            , label = Input.labelAbove [] (text "Password")
            , show = options.passwordText == ""
            }
            (List.filter isPasswordError options.errors)
        , Input.button
            [ Background.color buttonBg
            , Font.color buttonFg
            , Border.rounded 4
            , paddingXY 30 15
            ]
            { onPress =
                if options.loading then
                    Nothing

                else
                    Just options.onSubmit
            , label = el [ centerX, centerY ] (text buttonText)
            }
        ]
