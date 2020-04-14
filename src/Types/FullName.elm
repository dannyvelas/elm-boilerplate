module Types.FullName exposing (FullName, decode, encodeFirst, encodeLast)

import Json.Decode as Decode exposing (Decoder, at, string)
import Json.Encode as Encode


type FullName
    = FullName String String


encodeFirst : FullName -> Encode.Value
encodeFirst (FullName firstName _) =
    Encode.string firstName


encodeLast : FullName -> Encode.Value
encodeLast (FullName _ lastName) =
    Encode.string lastName


decode : Decoder FullName
decode =
    Decode.map2 FullName
        (at [ "firstName" ] string)
        (at [ "lastName" ] string)

