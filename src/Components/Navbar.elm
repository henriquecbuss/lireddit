module Components.Navbar exposing (..)

import Element exposing (..)
import Route exposing (linkToRoute)


navbar : Element msg
navbar =
    row [ width fill, paddingXY 20 40, spacing 50 ]
        [ linkToRoute [ alignRight ]
            { route = Route.Login, label = text "Login" }
        , linkToRoute [] { route = Route.Register, label = text "Register" }
        ]
