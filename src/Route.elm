module Route exposing (Route(..), href, parser, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes
import Url.Parser as Parser exposing (Parser, s)


type Route
    = Home
    | Signup
    | Login


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Signup (s "signup")
        , Parser.map Login (s "login")
        ]


href : Route -> Attribute msg
href targetRoute =
    Html.Attributes.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            "/"

        Signup ->
            "/signup"

        Login ->
            "/login"
