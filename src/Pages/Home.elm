module Pages.Home exposing (Model, init, view)

import Html exposing (Html, text)
import Session exposing (Session)


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session }
    , Cmd.none
    )


type alias Model =
    { session : Session }



-- VIEW


view : { title : String, content : Html msg }
view  =
    { title = "Home"
    , content = text "Home"
    }
