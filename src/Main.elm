module Main exposing (..)

import Api.Mutation as Mutation exposing (forgotPassword)
import Api.Query as Query
import Browser
import Browser.Navigation as Nav
import Components.Navbar exposing (navbar)
import Element exposing (..)
import GraphQL exposing (GraphQLResult, query, userSelection)
import Html exposing (Html)
import Page.ChangePassword as ChangePassword
import Page.CreatePost as CreatePost
import Page.EditPost as EditPost
import Page.ForgotPassword as ForgotPassword
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Post as Post
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
    | ChangePassword ChangePassword.Model
    | ForgotPassword ForgotPassword.Model
    | CreatePost CreatePost.Model
    | Post Post.Model
    | EditPost EditPost.Model
    | NotFound Session
    | Redirect Session


type alias Flags =
    String


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init apiUrl url key =
    changeRouteTo (Route.fromUrl url) (Redirect (Session.guest key apiUrl))



-- MESSAGE


type Msg
    = GotRegisterMsg Register.Msg
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotChangePasswordMsg ChangePassword.Msg
    | GotForgotPasswordMsg ForgotPassword.Msg
    | GotCreatePostMsg CreatePost.Msg
    | GotPostMsg Post.Msg
    | GotEditPostMsg EditPost.Msg
    | ChangedUrl Url.Url
    | RequestedUrl Browser.UrlRequest
    | GotSession (GraphQLResult (Maybe User))
    | RequestedLogOut
    | LoggedOut (GraphQLResult Bool)



-- MAIN


main : Program Flags Model Msg
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


viewPage : Browser.Document a -> (a -> Msg) -> Maybe Session -> Browser.Document Msg
viewPage docView msg maybeSession =
    { title = docView.title
    , body =
        [ basicView maybeSession <|
            List.map (Html.map msg >> html) docView.body
        ]
    }


basicView : Maybe Session -> List (Element Msg) -> Html Msg
basicView maybeSession children =
    layout [] <|
        column [ width fill, spacing 50 ] <|
            (case maybeSession of
                Nothing ->
                    none

                Just session ->
                    navbar session (Just RequestedLogOut) False
            )
                :: children


view : Model -> Browser.Document Msg
view model =
    case model of
        Redirect _ ->
            { title = "Loading"
            , body = [ basicView (Just <| toSession model) [ text "Loading" ] ]
            }

        Register register ->
            viewPage (Register.view register) GotRegisterMsg (Just <| toSession model)

        Home home ->
            viewPage (Home.view home) GotHomeMsg (Just <| toSession model)

        Login login ->
            viewPage (Login.view login) GotLoginMsg (Just <| toSession model)

        ChangePassword changePassword ->
            viewPage (ChangePassword.view changePassword)
                GotChangePasswordMsg
                (Just <| toSession model)

        ForgotPassword forgotPassword ->
            viewPage (ForgotPassword.view forgotPassword)
                GotForgotPasswordMsg
                (Just <| toSession model)

        CreatePost createPost ->
            viewPage (CreatePost.view createPost)
                GotCreatePostMsg
                (Just <| toSession model)

        EditPost editPost ->
            viewPage (EditPost.view editPost)
                GotEditPostMsg
                (Just <| toSession model)

        Post post ->
            viewPage (Post.view post)
                GotPostMsg
                (Just <| toSession model)

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

        ( GotChangePasswordMsg changePasswordMsg, ChangePassword changePassword ) ->
            ChangePassword.update changePassword changePasswordMsg
                |> updateWith ChangePassword GotChangePasswordMsg model

        ( GotForgotPasswordMsg forgotPasswordMsg, ForgotPassword forgotPassword ) ->
            ForgotPassword.update forgotPassword forgotPasswordMsg
                |> updateWith ForgotPassword GotForgotPasswordMsg model

        ( GotCreatePostMsg createPostMsg, CreatePost createPost ) ->
            CreatePost.update createPost createPostMsg
                |> updateWith CreatePost GotCreatePostMsg model

        ( GotPostMsg postMsg, Post post ) ->
            Post.update post postMsg
                |> updateWith Post GotPostMsg model

        ( GotEditPostMsg editPostMsg, EditPost editPost ) ->
            EditPost.update editPost editPostMsg
                |> updateWith EditPost GotEditPostMsg model

        ( GotSession result, _ ) ->
            case result of
                Ok user ->
                    updateSession model user

                Err _ ->
                    ( model, Cmd.none )

        ( RequestedLogOut, _ ) ->
            ( model
            , GraphQL.mutation (Session.apiUrl <| toSession model)
                Mutation.logout
                LoggedOut
            )

        ( LoggedOut result, _ ) ->
            case result of
                Ok True ->
                    updateSession model Nothing

                _ ->
                    ( model, Cmd.none )

        -- Disregard invalid messages
        ( GotRegisterMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotHomeMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotLoginMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotChangePasswordMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotForgotPasswordMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotCreatePostMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotEditPostMsg editPostMsg, _ ) ->
            ( model, Cmd.none )

        ( GotPostMsg _, _ ) ->
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
        apiUrl =
            toSession model |> Session.apiUrl
    in
    ( toModel subModel
    , Cmd.batch
        [ Cmd.map toMsg subCmd
        , GraphQL.getSession apiUrl GotSession
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

        Just (Route.ChangePassword token) ->
            ChangePassword.init session token
                |> updateWithSession ChangePassword GotChangePasswordMsg model

        Just Route.ForgotPassword ->
            ForgotPassword.init session
                |> updateWithSession ForgotPassword GotForgotPasswordMsg model

        Just Route.CreatePost ->
            CreatePost.init session
                |> updateWithSession CreatePost GotCreatePostMsg model

        Just (Route.Post postId) ->
            Post.init session postId
                |> updateWithSession Post GotPostMsg model

        Just (Route.EditPost postId) ->
            EditPost.init session postId
                |> updateWithSession EditPost GotEditPostMsg model


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

        ChangePassword changePassword ->
            ChangePassword.toSession changePassword

        ForgotPassword forgotPassword ->
            ForgotPassword.toSession forgotPassword

        CreatePost createPost ->
            CreatePost.toSession createPost

        Post post ->
            Post.toSession post

        EditPost editPost ->
            EditPost.toSession editPost


updateSession : Model -> Maybe User -> ( Model, Cmd Msg )
updateSession page maybeUser =
    case page of
        Redirect session ->
            ( page, Cmd.none )

        NotFound session ->
            ( page, Cmd.none )

        Home home ->
            Home.updateSession home maybeUser
                |> updateWith Home GotHomeMsg page

        Register register ->
            Register.updateSession register maybeUser
                |> updateWith Register GotRegisterMsg page

        Login login ->
            Login.updateSession login maybeUser
                |> updateWith Login GotLoginMsg page

        ChangePassword changePassword ->
            ( page, Cmd.none )

        ForgotPassword forgotPassword ->
            ( page, Cmd.none )

        CreatePost createPost ->
            CreatePost.updateSession createPost maybeUser
                |> updateWith CreatePost GotCreatePostMsg page

        Post post ->
            Post.updateSession post maybeUser
                |> updateWith Post GotPostMsg page

        EditPost editPost ->
            EditPost.updateSession editPost maybeUser
                |> updateWith EditPost GotEditPostMsg page



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
