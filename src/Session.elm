module Session exposing (Session, apiUrl, getUser, guest, logIn, navKey, updateSession)

import Browser.Navigation as Nav
import User exposing (User)



-- TYPES


type Session
    = LoggedIn Nav.Key User String
    | Guest Nav.Key String



-- INFO


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ _ ->
            key

        Guest key _ ->
            key


apiUrl : Session -> String
apiUrl session =
    case session of
        LoggedIn _ _ url ->
            url

        Guest _ url ->
            url


getUser : Session -> Maybe User
getUser session =
    case session of
        LoggedIn _ user _ ->
            Just user

        Guest _ _ ->
            Nothing


logIn : Session -> User -> Session
logIn session user =
    LoggedIn (navKey session) user (apiUrl session)


guest : Nav.Key -> String -> Session
guest =
    Guest



-- UPDATE


updateSession : Session -> Maybe User -> Session
updateSession session maybeUser =
    case maybeUser of
        Nothing ->
            Guest (navKey session) (apiUrl session)

        Just user ->
            LoggedIn (navKey session) user (apiUrl session)
