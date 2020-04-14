module Session exposing (Session, changes, getNavKey, getViewer, fromViewer)

import Api
import Browser.Navigation as Nav
import Viewer exposing (Viewer)


type Session
    = LoggedIn Nav.Key Viewer
    | Guest Nav.Key


getViewer : Session -> Maybe Viewer
getViewer session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.viewerChanges
        (\maybeViewer -> toMsg (fromViewer key maybeViewer))
        Viewer.decode


fromViewer : Nav.Key -> Maybe Viewer -> Session
fromViewer key maybeViewer =
    case maybeViewer of
        Just viewerVal ->
            LoggedIn key viewerVal

        Nothing ->
            Guest key
