module Page.Settings exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Avatar
import Browser.Navigation as Nav
import Email exposing (Email)
import Html exposing (Html, button, div, fieldset, h1, input, li, text, textarea, ul)
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


-- MODEL


type alias Model =
    { session : Session
    , problems : List String
    , status : Status
    }


type alias Form =
    { avatar : String
    , calories : String
    }


type Status
    = Loading
    | LoadingSlowly
    | Loaded Form
    | Failed


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , problems = []
      , status = Loading
      }
    , Cmd.batch
        [ Api.get Endpoint.user (Session.cred session) (Decode.field "user" formDecoder)
            |> Http.send CompletedFormLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )


formDecoder : Decoder Form
formDecoder =
    Decode.succeed Form
        |> required "image" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "expectedCalories" (Decode.map String.fromInt Decode.int)


{-| A form that has been validated. Only the `edit` function uses this. Its
purpose is to prevent us from forgetting to validate the form before passing
it to `edit`.

This doesn't create any guarantees that the form was actually validated. If
we wanted to do that, we'd need to move the form data into a separate module!

-}
type ValidForm
    = Valid Form



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Settings"
    , content =
        case Session.cred model.session of
            Just cred ->
                div [ class "settings-page" ]
                    [ div [ class "container page" ]
                        [ div [ class "row" ]
                            [ div [ class "col-md-6 offset-md-3 col-xs-12" ] <|
                                [ h1 [ class "text-xs-center" ] [ text "Your Settings" ]
                                , ul [ class "error-messages" ]
                                    (List.map viewProblem model.problems)
                                , case model.status of
                                    Loaded form ->
                                        viewForm cred form

                                    Loading ->
                                        text ""

                                    LoadingSlowly ->
                                        Loading.centeredIcon

                                    Failed ->
                                        text "Error loading page."
                                ]
                            ]
                        ]
                    ]

            Nothing ->
                text "Sign in to view your settings."
    }


viewForm : Cred -> Form -> Html Msg
viewForm cred form =
    Html.form [ onSubmit (SubmittedForm cred form) ]
        [ fieldset []
            [ fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"

                    -- TODO: Add file upload
                    , placeholder "URL of profile picture"
                    , value form.avatar
                    , onInput EnteredAvatar
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"

                    -- TODO: Implement
                    , placeholder "Expected calories per day (cal)"
                    , value form.calories
                    , onInput EnteredCalories
                    , type_ "number"
                    ]
                    []
                ]
            , button
                [ class "btn btn-lg btn-primary pull-xs-right" ]
                [ text "Update Settings" ]
            ]
        ]


viewProblem : String -> Html msg
viewProblem problem =
    li [] [ text problem ]



-- UPDATE


type Msg
    = SubmittedForm Cred Form
    | EnteredAvatar String
    | EnteredCalories String
    | CompletedFormLoad (Result Http.Error Form)
    | CompletedSave (Result Http.Error Viewer)
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedFormLoad (Ok form) ->
            ( { model | status = Loaded form }
            , Cmd.none
            )

        CompletedFormLoad (Err _) ->
            ( { model | status = Failed }
            , Cmd.none
            )

        SubmittedForm cred form ->
            case validate form of
                Ok validForm ->
                    ( { model | status = Loaded form }
                    , edit cred validForm
                        |> Http.send CompletedSave
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredAvatar avatar ->
            updateForm (\form -> { form | avatar = avatar }) model

        EnteredCalories cal ->
            updateForm (\form -> { form | calories = cal }) model

        CompletedSave (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
            in
                ( { model | problems = List.append model.problems serverErrors }
                , Cmd.none
                )

        CompletedSave (Ok viewer) ->
            ( model
            , Viewer.store viewer
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            case model.status of
                Loading ->
                    ( { model | status = LoadingSlowly }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd msg )
updateForm transform model =
    case model.status of
        Loaded form ->
            ( { model | status = Loaded (transform form) }, Cmd.none )

        _ ->
            ( model, Log.error )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!

NOTE: there are no ImageUrl or Bio variants here, because they aren't validated!

-}
type ValidatedField
    = Username
    | Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Username
    , Email
    , Password
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List String) ValidForm2
validate form =
    let
        trimmedForm =
            trimFields form
    in
        validateFields trimmedForm


type alias ValidForm2 =
    { avatar : String, calories : Int }


validateFields : TrimmedForm -> Result (List String) ValidForm2
validateFields (Trimmed form) =
    let
        rCalories : Result (List String) Int
        rCalories =
            form.calories
                |> String.toInt
                |> Result.fromMaybe [ "Calories can't be blank and must be a number." ]
    in
        Result.map (\cal -> { calories = cal, avatar = form.avatar }) rCalories


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { avatar = String.trim form.avatar
        , calories = String.trim form.calories
        }



-- HTTP


{-| This takes a Valid Form as a reminder that it needs to have been validated
first.
-}
edit : Cred -> ValidForm2 -> Http.Request Viewer
edit cred form =
    let
        encodedAvatar =
            case form.avatar of
                "" ->
                    Encode.null

                avatar ->
                    Encode.string avatar

        updates =
            Encode.object
                [ ( "image", encodedAvatar )
                , ( "expectedCalories", Encode.int form.calories )
                ]

        body =
            Encode.object [ ( "user", updates ) ]
                |> Http.jsonBody
    in
        Api.settings cred body Viewer.decoder


nothingIfEmpty : String -> Maybe String
nothingIfEmpty str =
    if String.isEmpty str then
        Nothing
    else
        Just str
