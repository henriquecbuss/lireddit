module Components.Button exposing (State(..), Variant(..), button)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



-- TYPES


type Variant
    = Transparent
    | Green
    | Teal


type State
    = Loading
    | Enabled String



-- PROPERTIES


buttonFg : Variant -> State -> Color
buttonFg variant state =
    case state of
        Loading ->
            rgb 0.9 0.9 0.9

        Enabled _ ->
            rgb 1 1 1


buttonBg : Variant -> State -> Color
buttonBg variant state =
    case variant of
        Transparent ->
            rgba 0 0 0 0

        Green ->
            case state of
                Loading ->
                    rgb255 0 112 84

                Enabled _ ->
                    rgb255 0 170 128

        Teal ->
            case state of
                Loading ->
                    rgb255 0 100 100

                Enabled _ ->
                    rgb255 0 128 128


buttonBorder : Variant -> State -> Color
buttonBorder variant state =
    case variant of
        Transparent ->
            rgb 1 1 1

        _ ->
            rgba 0 0 0 0


buttonBorderWidth : Variant -> State -> Int
buttonBorderWidth variant state =
    case variant of
        Transparent ->
            1

        _ ->
            0


buttonLabel : State -> Element msg
buttonLabel state =
    case state of
        Loading ->
            text "Loading"

        Enabled label ->
            text label


button :
    { onClick : msg, variant : Variant, state : State }
    -> Element msg
button { onClick, variant, state } =
    Input.button
        [ Background.color <| buttonBg variant state
        , Font.color <| buttonFg variant state
        , Border.rounded 4
        , Border.color <| buttonBorder variant state
        , Border.width <| buttonBorderWidth variant state
        , paddingXY 30 15
        ]
        { onPress =
            case state of
                Loading ->
                    Nothing

                Enabled _ ->
                    Just onClick
        , label = el [ centerX, centerY ] (buttonLabel state)
        }
