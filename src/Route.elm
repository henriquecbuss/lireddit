module Route exposing (Route(..), fromUrl, linkToRoute, replaceUrl)

import Browser.Navigation as Nav
import Element exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)



-- ROUTING


type Route
    = Home
    | Register
    | Login


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Register (s "register")
        , Parser.map Login (s "login")
        ]



-- PUBLIC HELPERS


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toString route)


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser


linkToRoute :
    List (Attribute msg)
    -> { route : Route, label : Element msg }
    -> Element msg
linkToRoute attrs { route, label } =
    link attrs { url = toString route, label = label }



-- INTERNAL


toString : Route -> String
toString page =
    "/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            []

        Register ->
            [ "register" ]

        Login ->
            [ "login" ]
