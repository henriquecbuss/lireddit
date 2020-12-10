module Session exposing (Session(..), navKey, updateSession)

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



-- UPDATE


updateSession : Session -> Maybe User -> Session
updateSession session maybeUser =
    case maybeUser of
        Nothing ->
            Guest (navKey session)

        Just user ->
            LoggedIn (navKey session) user
