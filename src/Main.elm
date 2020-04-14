module Main exposing (main)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Json.Encode as Encode
import Page
import Pages.Blank
import Pages.Home
import Pages.Login
import Pages.NotFound
import Pages.Signup
import Route
import Session exposing (Session)
import Url exposing (Url)
import Url.Parser as Parser
import Viewer exposing (Viewer)


main : Program Encode.Value Model Msg
main =
    Api.application Viewer.decode
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url key =
    updateUrl url
        (Redirect (Session.fromViewer key maybeViewer))


type Model
    = NotFound Session
    | Redirect Session
    | Home Session
    | Signup Pages.Signup.Model
    | Login Pages.Login.Model



-- VIEWS


view : Model -> Document Msg
view model =
    let
        viewer =
            Session.getViewer (toSession model)

        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view viewer page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        NotFound _ ->
            Page.view viewer Page.Other Pages.NotFound.view

        Redirect _ ->
            Page.view viewer Page.Other Pages.Blank.view

        Home _ ->
            Page.view viewer Page.Other Pages.Home.view

        Signup signupModel ->
            viewPage Page.Signup GotSignupMsg (Pages.Signup.view signupModel)

        Login loginModel ->
            viewPage Page.Login GotLoginMsg (Pages.Login.view loginModel)



-- MSG AND UPDATES


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotSession Session
    | GotSignupMsg Pages.Signup.Msg
    | GotLoginMsg Pages.Login.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl
                        (Session.getNavKey (toSession model))
                        (Url.toString url)
                    )

        ( ChangedUrl url, _ ) ->
            updateUrl url model

        ( GotSignupMsg signupMsg, Signup signupModel ) ->
            Pages.Signup.update signupMsg signupModel
                |> updateWith Signup GotSignupMsg

        ( GotLoginMsg loginMsg, Login loginModel ) ->
            Pages.Login.update loginMsg loginModel
                |> updateWith Login GotLoginMsg

        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.getNavKey (toSession model))

        Home _ ->
            Sub.none

        Signup _ ->
            Sub.none

        Login loginModel ->
            Sub.map GotLoginMsg (Pages.Login.subscriptions loginModel)



-- HELPERS


toSession : Model -> Session
toSession model =
    case model of
        NotFound session ->
            session

        Redirect session ->
            session

        Home session ->
            session

        Signup signupModel ->
            Pages.Signup.toSession signupModel

        Login loginModel ->
            Pages.Login.toSession loginModel


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    let
        session =
            toSession model
    in
    case Parser.parse Route.parser url of
        Just Route.Home ->
            ( Home session, Cmd.none )

        Just Route.Signup ->
            Pages.Signup.init session
                |> updateWith Signup GotSignupMsg

        Just Route.Login ->
            Pages.Login.init session
                |> updateWith Login GotLoginMsg

        Nothing ->
            ( NotFound session, Cmd.none )


updateWith :
    (subModel -> Model)
    -> (subMsg -> Msg)
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )
