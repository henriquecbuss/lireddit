module Main exposing (..)

import Api.Query as Query
import Browser
import Browser.Navigation as Nav
import GraphQL exposing (GraphQLResult, query, userSelection)
import Html exposing (..)
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Register as Register
import Route exposing (Route)
import Session exposing (Session(..))
import Url
import User exposing (User)



-- MODEL


type Model
    = Register Register.Model
    | Home Home.Model
    | Login Login.Model
    | NotFound Session
    | Redirect Session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url) (Redirect (Guest key))



-- MESSAGE


type Msg
    = GotRegisterMsg Register.Msg
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | ChangedUrl Url.Url
    | RequestedUrl Browser.UrlRequest
    | GotSession (GraphQLResult (Maybe User))



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

        Login login ->
            viewPage (Login.view login) GotLoginMsg

        NotFound _ ->
            NotFound.view



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

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

        ( GotLoginMsg loginMsg, Login login ) ->
            Login.update login loginMsg
                |> updateWith Login GotLoginMsg model

        ( GotSession result, _ ) ->
            case result of
                Ok user ->
                    ( updateSession model user, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

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


updateWithSession :
    (subModel -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWithSession toModel toMsg model ( subModel, subCmd ) =
    let
        key =
            toSession model
                |> Session.navKey
    in
    ( toModel subModel
    , Cmd.batch
        [ Cmd.map toMsg subCmd
        , GraphQL.getSession key GotSession
        ]
    )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWithSession Home GotHomeMsg model

        Just Route.Login ->
            Login.init session
                |> updateWithSession Login GotLoginMsg model

        Just Route.Register ->
            Register.init session
                |> updateWithSession Register GotRegisterMsg model


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

        Login login ->
            Login.toSession login


updateSession : Model -> Maybe User -> Model
updateSession page maybeUser =
    case page of
        Redirect session ->
            Session.updateSession session maybeUser
                |> Redirect

        NotFound session ->
            Session.updateSession session maybeUser
                |> NotFound

        Home home ->
            Home.updateSession home maybeUser
                |> Home

        Register register ->
            Register.updateSession register maybeUser
                |> Register

        Login login ->
            Login.updateSession login maybeUser
                |> Login



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
