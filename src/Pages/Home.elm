module Pages.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (Html, text)
import Route
import Session exposing (Session)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )


type alias Model =
    { session : Session }



-- VIEW


view : Model -> { title : String, content : Html msg }
view _ =
    { title = "Home"
    , content = text "Home"
    }



-- MSG AND UPDATES


type Msg
    = GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.getNavKey session) Route.Home
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.getNavKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
