module Page.NotFound exposing (..)

import Browser
import Element exposing (..)
import Html exposing (Html)



-- VIEW


view : Browser.Document msg
view =
    { title = "Not Found"
    , body =
        [ layout [] (el [] (text "not found"))
        ]
    }
