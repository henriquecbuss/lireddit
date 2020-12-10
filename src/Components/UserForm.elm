module Components.UserForm exposing (userForm)

import Components.Button as Button
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Error exposing (isPasswordError)


userForm :
    { onUsernameChange : String -> msg
    , usernameText : String
    , errors : List Error.Error
    , newPassword : Bool
    , onPasswordChange : String -> msg
    , passwordText : String
    , variant : Button.Variant
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
        , Button.button
            { onClick = options.onSubmit
            , variant = options.variant
            , state =
                if options.loading then
                    Button.Loading

                else
                    Button.Enabled options.buttonLabel
            }
        ]
