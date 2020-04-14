module Viewer exposing (Viewer, decode, getAvatar, getFullName, store)

import Api exposing (Cred)
import Json.Decode as Decode exposing (Decoder)
import Types.Avatar exposing (Avatar)
import Types.FullName exposing (FullName)


type Viewer
    = Viewer FullName Avatar Cred



-- GETTERS


getFullName : Viewer -> FullName
getFullName (Viewer fullName _ _) =
    fullName


getAvatar : Viewer -> Avatar
getAvatar (Viewer _ avatar _) =
    avatar



-- HELPERS


store : Viewer -> Cmd msg
store (Viewer fullName avatar cred) =
    Api.storeCredWith fullName avatar cred



-- DECODERS


decode : Decoder (Cred -> Viewer)
decode =
    Decode.map2 Viewer Types.FullName.decode Types.Avatar.decode
