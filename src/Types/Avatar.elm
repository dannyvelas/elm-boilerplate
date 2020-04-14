module Types.Avatar exposing (Avatar, encode, decode)

import Json.Decode as Decode exposing (Decoder, at)
import Json.Encode as Encode


type Avatar
    = Avatar String


encode : Avatar -> Encode.Value
encode (Avatar avatar) =
    Encode.string avatar


decode : Decoder Avatar
decode =
    Decode.map Avatar (at [ "avatar" ] Decode.string)

