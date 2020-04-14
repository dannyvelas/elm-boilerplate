module Page exposing (Page(..), view)

import Browser exposing (Document)
import Html exposing (Html, a, div, nav, text)
import Html.Attributes exposing (class, classList, style)
import Route exposing (Route)
import Viewer exposing (Viewer)


type Page
    = Home
    | Signup
    | Login
    | Other


view :
    Maybe Viewer
    -> Page
    -> { title : String, content : Html msg }
    -> Document msg
view maybeViewer page { title, content } =
    { title = title ++ " - Castor"
    , body = [ viewHeader page maybeViewer, content, viewFooter ]
    }


viewHeader : Page -> Maybe Viewer -> Html msg
viewHeader page maybeViewer =
    nav [ class "navbar border-bottom" ]
        [ a [ class "nav-item nav-link", Route.href Route.Home ]
            [ text "Home" ]
        , div [ class "navbar ml-auto" ] <|
            viewMenu page maybeViewer
        ]


viewMenu : Page -> Maybe Viewer -> List (Html msg)
viewMenu page maybeViewer =
    let
        navbarLink : Page -> Route -> List (Html msg) -> Html msg
        navbarLink page_ route linkContent =
            a
                [ Route.href route
                , class "nav-item nav-link"
                , classList [ ( "active", isActive page_ route ) ]
                ]
                linkContent
    in
    case maybeViewer of
        Just viewer ->
            let
                fullName =
                    Viewer.getFullName viewer

                avatar =
                    Viewer.getAvatar viewer
            in
            [ text "" ]

        Nothing ->
            [ navbarLink page Route.Signup [ text "Sign up" ]
            , navbarLink page Route.Login [ text "Login" ]
            ]


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        -- ( Signup, Route.Signup ) ->
        --     True
        ( Login, Route.Login ) ->
            True

        _ ->
            False


viewFooter : Html msg
viewFooter =
    text ""
