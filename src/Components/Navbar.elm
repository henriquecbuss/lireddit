module Components.Navbar exposing (..)

import Api.Mutation as Mutation
import Components.Button as Button
import Components.LinkButton exposing (linkButton)
import Components.Variant as Variant
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import GraphQL exposing (GraphQLResult)
import Route exposing (linkToRoute)
import Session exposing (Session(..))


{-| A navbar that receives a Session, a Message to be fired when the user logged out,
and an indication to know if the user is logging out
-}
navbar : Session -> Maybe msg -> Bool -> Element msg
navbar session loggedOut isLoggingOut =
    let
        linkStyles =
            [ mouseOver [ Font.color <| rgb255 242 148 48 ] ]
    in
    row
        [ width fill
        , height <| minimum 100 <| maximum 100 fill
        , Background.color <| rgb255 31 107 142
        , Font.color <| rgb 1 1 1
        , Region.navigation
        ]
    <|
        [ row [ width <| maximum 1000 fill, centerX, spacing 80 ]
            (case Session.getUser session of
                Just user ->
                    [ linkToRoute (Font.size 32 :: centerY :: linkStyles)
                        { route = Route.Home, label = text "LiReddit" }
                    , linkButton
                        [ alignRight ]
                        { route = Route.CreatePost
                        , variant = Variant.Teal
                        , label = text "Create Post"
                        }
                    , text user.username
                    , Button.button
                        []
                        { onClick = loggedOut
                        , variant = Variant.Transparent
                        , state =
                            if isLoggingOut then
                                Button.Loading

                            else
                                Button.Enabled "Log Out"
                        }
                    ]

                Nothing ->
                    [ linkToRoute (Font.size 32 :: linkStyles)
                        { route = Route.Home, label = text "LiReddit" }
                    , linkToRoute (alignRight :: linkStyles)
                        { route = Route.Login, label = text "Log In" }
                    , linkToRoute linkStyles { route = Route.Register, label = text "Register" }
                    ]
            )
        ]
