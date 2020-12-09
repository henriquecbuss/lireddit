module Session exposing (..)

import Browser.Navigation as Nav



-- TYPES


type Session
    = LoggedIn Nav.Key
    | Guest Nav.Key



-- INFO


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key ->
            key

        Guest key ->
            key
