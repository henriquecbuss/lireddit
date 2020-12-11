module Components.UserForm exposing (userForm)

import Components.Button as Button
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Error exposing (isPasswordError)
import Html.Events as Events
import Json.Decode as Decode


userForm : msg -> List (Element msg) -> Element msg
userForm onEnter =
    column
        [ height fill
        , width <| maximum 800 fill
        , padding 80
        , spacing 30
        , centerX
        , Element.htmlAttribute
            (Events.on "keyup"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\key ->
                            if key == "Enter" then
                                Decode.succeed onEnter

                            else
                                Decode.fail "Not the enter key"
                        )
                )
            )
        ]
