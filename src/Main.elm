module Main exposing (..)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Json.Encode as Encode
import Page
import Pages.Blank
import Pages.Home
import Pages.NotFound
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
    | Home Pages.Home.Model



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

        Home homeModel ->
            viewPage Page.Home GotHomeMsg (Pages.Home.view homeModel)



-- MSG AND UPDATES


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotSession Session
    | GotHomeMsg Pages.Home.Msg


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

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            Pages.Home.update homeMsg homeModel
                |> updateWith Home GotHomeMsg

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

        Home homeModel ->
            Sub.map GotHomeMsg (Pages.Home.subscriptions homeModel)



-- HELPERS


toSession : Model -> Session
toSession model =
    case model of
        NotFound session ->
            session

        Redirect session ->
            session

        Home homeModel ->
            Pages.Home.toSession homeModel


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    let
        session =
            toSession model
    in
    case Parser.parse Route.parser url of
        Just Route.Home ->
            Pages.Home.init session
                |> updateWith Home GotHomeMsg

        Nothing ->
            ( NotFound session, Cmd.none )


updateWith :
    (subModel -> Model)
    -> (subMsg -> Msg)
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )
