port module Api exposing
    ( Cred
    , application
    , decodeErrors
    , login
    , request
    , storeCredWith
    , viewerChanges
    )

import Browser
import Browser.Navigation as Nav
import Http
import Http.Detailed
import Json.Decode as Decode exposing (Decoder, at)
import Json.Encode as Encode
import Types.Avatar exposing (Avatar)
import Types.FullName exposing (FullName)
import Types.Id exposing (Id)
import Url exposing (Url)


type Cred
    = Cred Id String


credHeader : Cred -> Http.Header
credHeader (Cred _ token) =
    Http.header "x-auth-token" token



-- PORTS


port storeCache : Maybe Encode.Value -> Cmd msg


port onStoreChange : (Encode.Value -> msg) -> Sub msg



-- SENDING


request :
    Maybe Cred
    -> Http.Body
    -> (Result (Http.Detailed.Error String) ( Http.Metadata, a ) -> msg)
    -> Decoder a
    -> Cmd msg
request maybeCred body toMsg decoder =
    Http.request
        { method = "POST"
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = body
        , expect = Http.Detailed.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing

        --, url = "https://warm-fjord-23969.herokuapp.com/graphql"
        , url = "http://192.168.122.1:5000/graphql"
        }


login :
    Http.Body
    -> (Result (Http.Detailed.Error String) ( Http.Metadata, a ) -> msg)
    -> Decoder (Cred -> a)
    -> Cmd msg
login body toMsg decoder =
    request
        Nothing
        body
        toMsg
        (at [ "data", "loginResident" ] (decoderFromCred decoder))



-- HELPERS


application :
    Decoder (Cred -> viewer)
    ->
        { init : Maybe viewer -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Encode.Value model msg
application viewerDecoder config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (storageDecoder viewerDecoder))
                        |> Result.toMaybe
            in
            config.init maybeViewer url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


decodeErrors : Http.Detailed.Error String -> List String
decodeErrors err =
    let
        customError : String -> List String
        customError body =
            let
                gqlerr =
                    Decode.decodeString
                        (at [ "errors" ] (Decode.list (at [ "message" ] Decode.string)))
                        body
            in
            case gqlerr of
                Ok errors ->
                    List.map (String.append "Error: ") errors

                Err _ ->
                    [ "Server Error." ]
    in
    case err of
        Http.Detailed.Timeout ->
            [ "Server Error: Timeout exceeded" ]

        Http.Detailed.NetworkError ->
            [ "Server Error: Network error" ]

        Http.Detailed.BadStatus _ body ->
            customError body

        Http.Detailed.BadBody _ body _ ->
            customError body

        Http.Detailed.BadUrl url ->
            [ "Server Error: Malformed url: " ++ url ]


storeCredWith : FullName -> Avatar -> Cred -> Cmd msg
storeCredWith fullName avatar (Cred userId token) =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "id", Types.Id.encode userId )
                        , ( "firstName", Types.FullName.encodeFirst fullName )
                        , ( "lastName", Types.FullName.encodeLast fullName )
                        , ( "avatar", Types.Avatar.encode avatar )
                        , ( "token", Encode.string token )
                        ]
                  )
                ]
    in
    storeCache (Just json)


viewerChanges : (Maybe viewer -> msg) -> Decoder (Cred -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Cred -> viewer) -> Encode.Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storageDecoder : Decoder (Cred -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    at [ "user" ] (decoderFromCred viewerDecoder)


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        decodeCred


decodeCred : Decoder Cred
decodeCred =
    Decode.map2
        Cred
        (at [ "id" ] Types.Id.decode)
        (at [ "token" ] Decode.string)
