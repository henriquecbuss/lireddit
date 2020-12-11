module Error exposing
    ( Error
    , fromRecord
    , isEmailError
    , isPasswordError
    , isUsernameError
    , toMessage
    , unknown
    , viewInputWithError
    )

import Element exposing (..)
import Element.Border as Border
import Element.Font as Font


type Error
    = UsernameError String
    | PasswordError String
    | EmailError String
    | UnknownError String


isUsernameError : Error -> Bool
isUsernameError err =
    case err of
        UsernameError _ ->
            True

        _ ->
            False


isPasswordError : Error -> Bool
isPasswordError err =
    case err of
        PasswordError _ ->
            True

        _ ->
            False


isEmailError : Error -> Bool
isEmailError err =
    case err of
        EmailError _ ->
            True

        _ ->
            False


toMessage : Error -> String
toMessage err =
    case err of
        UsernameError msg ->
            msg

        PasswordError msg ->
            msg

        EmailError msg ->
            msg

        UnknownError msg ->
            msg


fromRecord : { field : String, message : String } -> Error
fromRecord { field, message } =
    if field == "username" then
        UsernameError message

    else if field == "password" then
        PasswordError message

    else if field == "email" then
        EmailError message

    else
        UnknownError message


unknown : Error
unknown =
    UnknownError "Unknown Error"


viewInputWithError :
    (List (Attribute msg) -> options -> Element msg)
    -> List (Attribute msg)
    -> options
    -> List Error
    -> Element msg
viewInputWithError inputFunction attributes options errors =
    let
        errorBorder =
            [ Border.color <| rgba 1 0 0 0.4
            , Border.glow (rgba 1 0 0 0.2) 1.5
            ]
    in
    column [ width fill, spacing 15 ]
        [ inputFunction
            ((if not (List.isEmpty errors) then
                errorBorder

              else
                []
             )
                ++ attributes
            )
            options
        , List.map viewError errors
            |> column []
        ]


viewError : Error -> Element msg
viewError =
    el [ Font.color <| rgb 1 0 0 ]
        << text
        << toMessage
