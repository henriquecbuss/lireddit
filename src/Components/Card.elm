module Components.Card exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Font as Font


card : List (Attribute msg) -> ( Element msg, Element msg ) -> Element msg
card attributes ( title, content ) =
    column
        (attributes
            ++ [ Border.rounded 4
               , Border.glow (rgba 0.2 0.2 0.2 0.12) 2
               , spacing 40
               , paddingXY 20 50
               , width fill
               ]
        )
        [ el [ Font.bold ] title, content ]
