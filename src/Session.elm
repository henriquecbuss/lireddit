module Session exposing (..)

import Browser.Navigation as Nav
import User exposing (User)



-- TYPES


type Session
    = LoggedIn Nav.Key User
    | Guest Nav.Key



-- INFO


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key
