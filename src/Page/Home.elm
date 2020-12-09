module Page.Home exposing (..)

import Browser
import Element exposing (..)
import Html exposing (Html)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )



-- MESSAGE


type Msg
    = NoOp



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Home"
    , body =
        [ layout []
            (text "Home")
        ]
    }



-- UPDATE


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- EXPORT


toSession : Model -> Session
toSession =
    .session
