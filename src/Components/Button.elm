module Components.Button exposing (State(..), button)

import Components.Variant as Variant exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



-- TYPES


type State
    = Loading
    | Enabled String



-- PROPERTIES


buttonFg : Variant -> State -> Color
buttonFg variant state =
    case state of
        Loading ->
            setAlpha 0.9 <| fg variant

        Enabled _ ->
            fg variant


buttonBg : Variant -> State -> Color
buttonBg variant state =
    case state of
        Loading ->
            setAlpha 0.75 <| bg variant

        Enabled _ ->
            bg variant


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
        , Border.color <| border variant
        , Border.width <| borderWidth variant
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
