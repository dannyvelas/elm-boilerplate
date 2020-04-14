module Pages.Signup exposing (Model, Msg, init, toSession, update, view)

import Api
import Html exposing (Html, button, div, i, input, li, span, text, ul)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Http.Detailed
import Json.Decode as Decode exposing (at)
import Json.Encode as Encode
import Session exposing (Session)



-- INITS


init : Session -> ( Model, Cmd Msg )
init session =
    ( { form =
            { firstName = ""
            , lastName = ""
            , email = ""
            , password = ""
            }
      , passwordHidden = True
      , problems = []
      , confirmationText = ""
      , session = session
      }
    , Cmd.none
    )



-- VARS AND MODEL


type alias Model =
    { form : Form
    , passwordHidden : Bool
    , problems : List Problem
    , confirmationText : String
    , session : Session
    }


type alias Form =
    { firstName : String
    , lastName : String
    , email : String
    , password : String
    }


minPasswordChars : Int
minPasswordChars =
    6


type TrimmedForm
    = Trimmed Form


type ValidatedField
    = FirstName
    | LastName
    | Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ FirstName
    , LastName
    , Email
    , Password
    ]


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String



-- VIEWS


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Signup"
    , content =
        div [ class "signup-page" ]
            [ viewForm model.form model.problems model.passwordHidden
            , viewConfirmationText model.confirmationText
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
                                [ class "fa fa-user" ]
                                []
                            ]
                        ]
                    , input
                        [ class "form-control"
                        , type_ "text"
                        , placeholder "First Name"
                        , value form.firstName
                        , onInput EnteredFirstName
                        ]
                        []
                    ]
                ]
            , div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ]
                            [ i
                                [ class "fa fa-user" ]
                                []
                            ]
                        ]
                    , input
                        [ class "form-control"
                        , type_ "text"
                        , placeholder "Last Name"
                        , value form.lastName
                        , onInput EnteredLastName
                        ]
                        []
                    ]
                ]
            , div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ]
                            [ i
                                [ class "fa fa-user" ]
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
                                [ class "fa fa-lock" ]
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
            , div [ class "form-group text-center" ]
                [ button
                    [ type_ "submit"
                    , class "btn btn-primary"
                    ]
                    [ text "Sign up" ]
                ]
            ]
        ]


viewConfirmationText : String -> Html msg
viewConfirmationText confirmationText =
    div [ style "text-align" "center" ]
        [ text confirmationText ]


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
    = EnteredFirstName String
    | EnteredLastName String
    | EnteredEmail String
    | EnteredPassword String
    | SubmittedForm
    | ToggledShowPassword
    | SentEmail (Result (Http.Detailed.Error String) ( Http.Metadata, String ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredFirstName firstName ->
            updateForm (\form -> { form | firstName = firstName }) model

        EnteredLastName lastName ->
            updateForm (\form -> { form | lastName = lastName }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Api.request
                        Nothing
                        (signupBody validForm)
                        SentEmail
                        (at [ "data", "signupEmployee" ] Decode.string)
                    )

                Err problems ->
                    ( { model | problems = problems }, Cmd.none )

        ToggledShowPassword ->
            ( { model | passwordHidden = not model.passwordHidden }, Cmd.none )

        SentEmail response ->
            case response of
                Ok ( _, confirmationText ) ->
                    ( { model | confirmationText = confirmationText }, Cmd.none )

                Err error ->
                    ( { model | problems = updateErrors error model.problems }
                    , Cmd.none
                    )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )


updateErrors : Http.Detailed.Error String -> List Problem -> List Problem
updateErrors error problems =
    let
        serverError =
            Api.decodeErrors error
                |> List.map ServerError
    in
    List.append problems serverError



-- GQL


signupBody : TrimmedForm -> Http.Body
signupBody (Trimmed form) =
    [ ( "query"
      , Encode.string
            """
                mutation 
                    ( $employeeFirstName: String!
                    , $employeeLastName: String! 
                    , $employeeEmail: String!
                    , $employeePassword: String! 
                    )
                        { signupEmployee
                              ( employeeFirstName: $employeeFirstName
                              , employeeLastName: $employeeLastName
                              , employeeEmail: $employeeEmail
                              , employeePassword: $employeePassword
                              )
                        }
            """
      )
    , ( "variables"
      , Encode.object
            [ ( "employeeFirstName", Encode.string form.firstName )
            , ( "employeeLastName", Encode.string form.lastName )
            , ( "employeeEmail", Encode.string form.email )
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


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { firstName = String.trim form.firstName
        , lastName = String.trim form.lastName
        , email = String.trim form.email
        , password = String.trim form.password
        }


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            FirstName ->
                if String.isEmpty form.firstName then
                    [ "Error: First Name can't be blank." ]

                else
                    []

            LastName ->
                if String.isEmpty form.lastName then
                    [ "Error: Last Name can't be blank." ]

                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "Error: Email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "Password can't be blank." ]

                else if String.length form.password < minPasswordChars then
                    [ "Password must be at least " ++ String.fromInt minPasswordChars ++ " characters long." ]

                else
                    []



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
