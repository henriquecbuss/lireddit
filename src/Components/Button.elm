module Components.Button exposing (State(..), button)

import Components.Loader as Loader
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


colorString : Color -> String
colorString color =
    let
        { red, green, blue, alpha } =
            toRgb color

        componentToString component =
            component * 255 |> round |> String.fromInt

        components =
            List.map componentToString [ red, green, blue ]
                |> String.join ", "
    in
    "rgba(" ++ components ++ ", " ++ String.fromFloat alpha ++ ")"


buttonLabel : Variant -> State -> Element msg
buttonLabel variant state =
    case state of
        Loading ->
            Loader.dots [] { radius = 5, color = colorString <| buttonFg variant state }

        Enabled label ->
            text label


button :
    List (Attribute msg)
    -> { onClick : Maybe msg, variant : Variant, state : State }
    -> Element msg
button attributes { onClick, variant, state } =
    Input.button
        ([ Background.color <| buttonBg variant state
         , Font.color <| buttonFg variant state
         , Border.rounded 4
         , Border.color <| border variant
         , Border.width <| borderWidth variant
         , paddingXY 30 15
         , mouseOver <|
            case state of
                Loading ->
                    []

                Enabled _ ->
                    [ Background.color <| hoverBg variant ]
         ]
            ++ attributes
        )
        { onPress =
            case state of
                Loading ->
                    Nothing

                Enabled _ ->
                    onClick
        , label = el [ centerX, centerY ] (buttonLabel variant state)
        }
