module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Register as Register
import Route exposing (Route)
import Session exposing (Session(..))
import Url



-- MODEL


type Model
    = Register Register.Model
    | Home Home.Model
    | NotFound Session
    | Redirect Session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url) (Redirect (Guest key))



-- MESSAGE


type Msg
    = GotRegisterMsg Register.Msg
    | GotHomeMsg Home.Msg
    | ChangedUrl Url.Url
    | RequestedUrl Browser.UrlRequest



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = RequestedUrl
        }



-- VIEW


viewPage : Browser.Document a -> (a -> Msg) -> Browser.Document Msg
viewPage docView msg =
    { title = docView.title
    , body = List.map (Html.map msg) docView.body
    }


view : Model -> Browser.Document Msg
view model =
    case model of
        Redirect _ ->
            { title = "Loading", body = [ text "loading" ] }

        Register register ->
            viewPage (Register.view register) GotRegisterMsg

        Home home ->
            viewPage (Home.view home) GotHomeMsg

        NotFound _ ->
            NotFound.view



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl (url |> Debug.log "url") |> Debug.log "fromUrl") model

        ( RequestedUrl request, _ ) ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (Session.navKey (toSession model))
                        (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        ( GotRegisterMsg registerMsg, Register register ) ->
            Register.update register registerMsg
                |> updateWith Register GotRegisterMsg model

        ( GotHomeMsg homeMsg, Home home ) ->
            Home.update home homeMsg
                |> updateWith Home GotHomeMsg model

        -- Disregard invalid messages
        _ ->
            ( model, Cmd.none )


updateWith :
    (subModel -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
                |> Debug.log "session"
    in
    case Debug.log "maybeRoute" maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.Register ->
            Register.init session
                |> updateWith Register GotRegisterMsg model


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Register register ->
            Register.toSession register



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
