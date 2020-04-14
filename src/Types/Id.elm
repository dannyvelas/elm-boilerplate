module Types.Id exposing (Id, decode, encode, toString, urlParser)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Encode as Encode
import Url.Parser as Parser exposing (Parser)


type Id
    = Id String



-- HELPERS


urlParser : Parser (Id -> a) a
urlParser =
    Parser.custom "ID" (\str -> Just (Id str))


decode : Decoder Id
decode =
    Decode.map Id string


encode : Id -> Encode.Value
encode (Id id) =
    Encode.string id


toString : Id -> String
toString (Id str) =
    str

