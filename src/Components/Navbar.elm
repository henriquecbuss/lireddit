module Components.Navbar exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (focusedOnLoad)
import Element.Region as Region
import Route exposing (linkToRoute)
import Session exposing (Session(..))


navbar : Session -> Element msg
navbar session =
    let
        linkStyles =
            [ mouseOver [ Font.color <| rgb255 242 148 48 ] ]
    in
    (case session of
        LoggedIn _ user ->
            [ el [ alignRight ] <| text user.username
            , el [] (text "Log Out")
            ]

        Guest _ ->
            [ linkToRoute (linkStyles ++ [ alignRight ])
                { route = Route.Login, label = text "Log In" }
            , linkToRoute linkStyles { route = Route.Register, label = text "Register" }
            ]
    )
        |> row
            [ width fill
            , paddingXY 100 40
            , spacing 80
            , Background.color <| rgb255 31 107 142
            , Font.color <| rgb 1 1 1
            , Region.navigation
            ]



-- GRAPHQL
