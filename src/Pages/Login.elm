module Pages.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api
import Html exposing (Html, button, div, i, input, li, span, text, ul)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Http.Detailed
import Json.Encode as Encode
import Session exposing (Session)
import Viewer exposing (Viewer)



-- INIT


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , problems = []
      , passwordHidden = True
      , form =
            { email = ""
            , password = ""
            }
      }
    , Cmd.none
    )



-- MODEL AND VARS


type alias Model =
    { session : Session
    , problems : List Problem
    , passwordHidden : Bool
    , form : Form
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type alias Form =
    { email : String
    , password : String
    }


type ValidatedField
    = Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    , Password
    ]


type TrimmedForm
    = Trimmed Form



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div [ class "login-page" ]
            [ viewForm model.form model.problems model.passwordHidden
            ]
    }


viewForm : Form -> List Problem -> Bool -> Html Msg
viewForm form problems passwordHidden =
    let
        eyeIcon =
            if passwordHidden then
                "fa fa-eye"

            else
                "fa fa-eye-slash"

        passwordType =
            if passwordHidden then
                "password"

            else
                "text"
    in
    div [ class "mx-auto", style "width" "400px" ]
        [ ul [ class "error-messages" ]
            (List.map viewProblem problems)
        , Html.form [ onSubmit SubmittedForm ]
            [ div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ]
                            [ i
                                [ class "fa fa-user"
                                ]
                                []
                            ]
                        ]
                    , input
                        [ class "form-control"
                        , type_ "text"
                        , placeholder "Email"
                        , value form.email
                        , onInput EnteredEmail
                        ]
                        []
                    ]
                ]
            , div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ]
                            [ i
                                [ class "fa fa-lock"
                                ]
                                []
                            ]
                        ]
                    , input
                        [ class "form-control"
                        , type_ passwordType
                        , placeholder "Password"
                        , value form.password
                        , onInput EnteredPassword
                        ]
                        []
                    , div
                        [ class "input-group-append"
                        , onClick ToggledShowPassword
                        ]
                        [ span [ class "input-group-text" ]
                            [ i [ class eyeIcon ] []
                            ]
                        ]
                    ]
                ]
            , div
                [ class "form-group text-center"
                ]
                [ button
                    [ class "btn btn-primary"
                    , type_ "submit"
                    ]
                    [ text "Login" ]
                ]
            ]
        ]


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ str ->
                    str

                ServerError str ->
                    str
    in
    li [] [ text errorMessage ]



-- MSG AND UPDATES


type Msg
    = EnteredEmail String
    | EnteredPassword String
    | SubmittedForm
    | ToggledShowPassword
    | CompletedLogin (Result (Http.Detailed.Error String) ( Http.Metadata, Viewer ))
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        SubmittedForm ->
            case validate model.form of
                Ok (Trimmed form) ->
                    ( { model | problems = [] }
                    , Api.login
                        (loginBody (Trimmed form))
                        CompletedLogin
                        Viewer.decode
                    )

                Err problems ->
                    ( { model | problems = problems }, Cmd.none )

        ToggledShowPassword ->
            ( { model | passwordHidden = not model.passwordHidden }, Cmd.none )

        CompletedLogin response ->
            case response of
                Ok ( _, viewer ) ->
                    ( model, Viewer.store viewer )

                Err error ->
                    let
                        serverError =
                            Api.decodeErrors error
                                |> List.map ServerError
                    in
                    ( { model | problems = List.append model.problems serverError }
                    , Cmd.none
                    )

        GotSession session ->
            ( { model | session = session }
            , Debug.todo "Make route to dashboard"
            )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.getNavKey model.session)



-- GQL


loginBody : TrimmedForm -> Http.Body
loginBody (Trimmed form) =
    [ ( "query"
      , Encode.string
            """
            mutation 
                ( $employeeEmail: String!
                , $employeePassword: String! 
                )
                    { loginEmployee
                          ( employeeEmail: $employeeEmail
                          , employeePassword: $employeePassword
                          )
                              { id
                                firstName
                                lastName
                                avatar
                                company { name }
                                token
                              }
                    }
            """
      )
    , ( "variables"
      , Encode.object
            [ ( "employeeEmail", Encode.string form.email )
            , ( "employeePassword", Encode.string form.password )
            ]
      )
    ]
        |> Encode.object
        |> Http.jsonBody



-- HELPERS


validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    [ "Error: Email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "Error: Password can't be blank." ]

                else
                    []


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        , password = String.trim form.password
        }



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
