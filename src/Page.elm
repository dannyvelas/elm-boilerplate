module Page exposing (Page(..), view)

import Browser exposing (Document)
import Html exposing (Html, div, nav, text)
import Html.Attributes exposing (class)
import Viewer exposing (Viewer)


type Page
    = Home
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
    nav []
        [ div [ class "navbar ml-auto" ] <|
            viewMenu page maybeViewer
        ]


viewMenu : Page -> Maybe Viewer -> List (Html msg)
viewMenu page maybeViewer =
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
            [ text "" ]


viewFooter : Html msg
viewFooter =
    text ""
