module Components.Variant exposing (..)

import Element exposing (..)



-- TYPES


type Variant
    = Transparent
    | Green
    | Gray
    | Teal
    | Red


bg : Variant -> Color
bg variant =
    case variant of
        Transparent ->
            rgba 0 0 0 0

        Green ->
            rgb255 0 170 128

        Red ->
            rgb255 183 48 48

        Gray ->
            rgb255 130 130 130

        Teal ->
            rgb255 0 128 128


fg : Variant -> Color
fg variant =
    rgb 1 1 1


border : Variant -> Color
border variant =
    case variant of
        Transparent ->
            rgb 1 1 1

        _ ->
            rgba 0 0 0 0


borderWidth : Variant -> Int
borderWidth variant =
    case variant of
        Transparent ->
            1

        _ ->
            0


setAlpha : Float -> Color -> Color
setAlpha amt color =
    let
        original =
            toRgb color
    in
    { original | alpha = amt }
        |> fromRgb
