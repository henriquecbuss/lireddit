module Components.LinkButton exposing (..)

import Components.Variant as Variant exposing (Variant)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Route as Route exposing (Route)


linkButton : List (Attribute msg) -> { route : Route, variant : Variant, label : Element msg } -> Element msg
linkButton attributes { route, variant, label } =
    Route.linkToRoute
        ([ paddingXY 30 15
         , Background.color <| Variant.bg variant
         , Font.color <| Variant.fg variant
         , Border.rounded 4
         , Border.width <| Variant.borderWidth variant
         , Border.color <| Variant.border variant
         ]
            ++ attributes
        )
        { route = route, label = label }
