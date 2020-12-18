module Components.Loader exposing (dots)

import Element exposing (Element)
import Svg exposing (Svg)
import Svg.Attributes as Attr


dots : List (Element.Attribute msg) -> { radius : Int, color : String } -> Element msg
dots attributes { radius, color } =
    let
        dot index =
            Svg.circle
                [ Attr.cx <| String.fromInt (index * radius * 3 + radius)
                , Attr.cy <| String.fromInt radius
                , Attr.r <| String.fromInt radius
                , Attr.fill color
                , Attr.opacity "0"
                ]
                [ Svg.animate
                    [ Attr.attributeName "opacity"
                    , Attr.values "0;1;0"
                    , Attr.dur "2s"
                    , Attr.begin <| String.fromFloat (toFloat index * 0.2) ++ "s"
                    , Attr.repeatCount "indefinite"
                    ]
                    []
                ]
    in
    Svg.svg
        [ Attr.width <| String.fromInt (radius * 8)
        , Attr.height <| String.fromInt (radius * 2)
        ]
        [ dot 0, dot 1, dot 2 ]
        |> Element.html
        |> Element.el attributes
