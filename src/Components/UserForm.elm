module Components.UserForm exposing (userForm)

import Components.Button as Button
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Error exposing (isPasswordError)


userForm : List (Element msg) -> Element msg
userForm =
    column
        [ height fill
        , width <| maximum 800 fill
        , padding 80
        , spacing 30
        , centerX
        ]
