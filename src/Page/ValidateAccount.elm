module Page.ValidateAccount exposing (Model, Msg, init, update, view, toSession)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Avatar
import Browser.Navigation as Nav
import Email exposing (Email)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import Loading
import Log
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Task
import Username as Username exposing (Username)
import Viewer exposing (Viewer)
import Url.Builder


-- MODEL


type alias Model =
    { session : Session, status : Status }


type Status
    = Loading
    | Validated
    | Failed


init : Session -> String -> ( Model, Cmd Msg )
init session validateKeyword =
    let
        params =
            [ Url.Builder.string "vtk" validateKeyword ]
    in
        ( { session = session, status = Loading }
        , Api.get (Endpoint.validateUser params) Nothing (Decode.succeed ())
            |> Http.send CompletedValidate
        )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Validate your account"
    , content =
        div [ class "settings-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ] <|
                        [ h1 [ class "text-xs-center" ] [ text "Validating your account" ]
                        , case model.status of
                            Loading ->
                                div []
                                    [ Loading.centeredIcon
                                    , text "We're validating your account."
                                    ]

                            Validated ->
                                div []
                                    [ p [] [ text "Success! Your account has been validated." ]
                                    , p [] [ text "Sign in to view your account." ]
                                    ]

                            Failed ->
                                text "Your account couldn't be validated"
                        ]
                    ]
                ]
            ]
    }



-- UPDATE


type Msg
    = CompletedValidate (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedValidate (Ok _) ->
            ( { model | status = Validated }
            , Cmd.none
            )

        CompletedValidate (Err _) ->
            ( { model | status = Failed }
            , Cmd.none
            )


toSession : Model -> Session
toSession model =
    model.session
