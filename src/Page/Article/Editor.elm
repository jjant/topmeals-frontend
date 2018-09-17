module Page.Article.Editor exposing (Model, Msg, initEdit, initNew, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Full)
import Article.Body exposing (Body)
import Article.Slug as Slug exposing (Slug)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Loading
import Page
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Task exposing (Task)
import Time exposing (Posix)
import Meal exposing (Meal)
import Iso8601


-- MODEL


type alias Model =
    { session : Session
    , status : Status
    }


type
    Status
    -- Edit meal
    = Loading Slug
    | LoadingSlowly Slug
    | LoadingFailed Slug
    | Saving Slug Form
    | Editing Slug (List String) Form
      -- New meal
    | EditingNew (List String) Form
    | Creating Form


type alias Form =
    { text : String
    , calories : String
    , datetime : String
    }


initNew : Session -> ( Model, Cmd msg )
initNew session =
    ( { session = session
      , status =
            EditingNew []
                { text = ""
                , calories = ""
                , datetime = ""
                }
      }
    , Cmd.none
    )


initEdit : Session -> Slug -> ( Model, Cmd Msg )
initEdit session slug =
    ( { session = session
      , status = Loading slug
      }
    , Cmd.batch
        [ Meal.fetch (Session.cred session) slug
            |> Http.toTask
            -- If init fails, store the slug that failed in the msg, so we can
            -- at least have it later to display the page's title properly!
            |> Task.mapError (\httpError -> ( slug, httpError ))
            |> Task.attempt CompletedArticleLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title =
        case getSlug model.status of
            Just slug ->
                "Edit Meal - " ++ Slug.toString slug

            Nothing ->
                "New Meal"
    , content =
        case Session.cred model.session of
            Just cred ->
                viewAuthenticated cred model

            Nothing ->
                text "Sign in to edit this meal."
    }


viewProblems : List String -> Html msg
viewProblems problems =
    ul [ class "error-messages" ]
        (List.map viewProblem problems)


viewProblem : String -> Html msg
viewProblem problem =
    li [] [ text problem ]


viewAuthenticated : Cred -> Model -> Html Msg
viewAuthenticated cred model =
    let
        formHtml =
            case model.status of
                Loading _ ->
                    []

                LoadingSlowly _ ->
                    [ Loading.icon ]

                Saving slug form ->
                    [ viewForm cred form (editArticleSaveButton [ disabled True ]) ]

                Creating form ->
                    [ viewForm cred form (newArticleSaveButton [ disabled True ]) ]

                Editing slug problems form ->
                    [ viewProblems problems
                    , viewForm cred form (editArticleSaveButton [])
                    ]

                EditingNew problems form ->
                    [ viewProblems problems
                    , viewForm cred form (newArticleSaveButton [])
                    ]

                LoadingFailed _ ->
                    [ text "Article failed to load." ]
    in
        div [ class "editor-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
                        formHtml
                    ]
                ]
            ]


viewForm : Cred -> Form -> Html Msg -> Html Msg
viewForm cred fields saveButton =
    Html.form [ onSubmit (ClickedSave cred) ]
        [ fieldset []
            [ fieldset [ class "form-group" ]
                [ input
                    [ class "form-control form-control-lg"
                    , placeholder "Meal name"
                    , onInput EnteredText
                    , value fields.text
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "How many calories did this meal contain?"
                    , onInput EnteredCalories
                    , value fields.calories
                    , type_ "number"
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "When did you have this meal?"
                    , onInput EnteredDatetime
                    , value fields.datetime
                    ]
                    []
                ]
            , saveButton
            ]
        ]


editArticleSaveButton : List (Attribute msg) -> Html msg
editArticleSaveButton extraAttrs =
    saveArticleButton "Update Article" extraAttrs


newArticleSaveButton : List (Attribute msg) -> Html msg
newArticleSaveButton extraAttrs =
    saveArticleButton "Save meal" extraAttrs


saveArticleButton : String -> List (Attribute msg) -> Html msg
saveArticleButton caption extraAttrs =
    button (class "btn btn-lg pull-xs-right btn-primary" :: extraAttrs)
        [ text caption ]



-- UPDATE


type Msg
    = ClickedSave Cred
    | EnteredDatetime String
    | EnteredCalories String
    | EnteredText String
    | CompletedCreate (Result Http.Error Meal)
    | CompletedEdit (Result Http.Error Meal)
    | CompletedArticleLoad (Result ( Slug, Http.Error ) Meal)
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSave cred ->
            model.status
                |> save cred
                |> Tuple.mapFirst (\status -> { model | status = status })

        EnteredText text ->
            updateForm (\form -> { form | text = text }) model

        EnteredCalories calories ->
            updateForm (\form -> { form | calories = calories }) model

        EnteredDatetime datetime ->
            updateForm (\form -> { form | datetime = datetime }) model

        CompletedCreate (Ok meal) ->
            ( model
            , Route.Meal (Meal.slug meal)
                |> Route.replaceUrl (Session.navKey model.session)
            )

        CompletedCreate (Err error) ->
            ( { model | status = savingError error model.status }
            , Cmd.none
            )

        CompletedEdit (Ok meal) ->
            ( model
            , Route.Meal (Meal.slug meal)
                |> Route.replaceUrl (Session.navKey model.session)
            )

        CompletedEdit (Err error) ->
            ( { model | status = savingError error model.status }
            , Cmd.none
            )

        CompletedArticleLoad (Err ( slug, error )) ->
            ( { model | status = LoadingFailed slug }
            , Cmd.none
            )

        CompletedArticleLoad (Ok meal) ->
            let
                { text, calories, datetime } =
                    meal

                status =
                    Editing (Meal.slug meal)
                        []
                        { text = text
                        , calories = String.fromInt calories
                        , datetime = Iso8601.fromTime datetime
                        }
            in
                ( { model | status = status }
                , Cmd.none
                )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                status =
                    case model.status of
                        Loading slug ->
                            LoadingSlowly slug

                        other ->
                            other
            in
                ( { model | status = status }, Cmd.none )


save : Cred -> Status -> ( Status, Cmd Msg )
save cred status =
    case status of
        Editing slug _ form ->
            case validate form of
                Ok validForm ->
                    ( Saving slug form
                    , edit slug validForm cred
                        |> Http.send CompletedEdit
                    )

                Err problems ->
                    ( Editing slug problems form
                    , Cmd.none
                    )

        EditingNew _ form ->
            case validate form of
                Ok validForm ->
                    ( Creating form
                    , create validForm cred
                        |> Http.send CompletedCreate
                    )

                Err problems ->
                    ( EditingNew problems form
                    , Cmd.none
                    )

        _ ->
            -- We're in a state where saving is not allowed.
            -- We tried to prevent getting here by disabling the Save
            -- button, but somehow the user got here anyway!
            --
            -- If we had an error logging service, we would send
            -- something to it here!
            ( status, Cmd.none )


savingError : Http.Error -> Status -> Status
savingError error status =
    let
        problems =
            [ "Error creating meal" ]
    in
        case status of
            Saving slug form ->
                Editing slug problems form

            Creating form ->
                EditingNew problems form

            _ ->
                status


{-| Helper function for `update`. Updates the form, if there is one,
and returns Cmd.none.

Useful for recording form fields!

This could also log errors to the server if we are trying to record things in
the form and we don't actually have a form.

-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    let
        newModel =
            case model.status of
                Loading _ ->
                    model

                LoadingSlowly _ ->
                    model

                LoadingFailed _ ->
                    model

                Saving slug form ->
                    { model | status = Saving slug (transform form) }

                Editing slug errors form ->
                    { model | status = Editing slug errors (transform form) }

                EditingNew errors form ->
                    { model | status = EditingNew errors (transform form) }

                Creating form ->
                    { model | status = Creating (transform form) }
    in
        ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!
-}
type ValidatedField
    = Text
    | Calories
    | Datetime


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Text
    , Calories
    , Datetime
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List String) ValidForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
        validateFields trimmedForm


type alias ValidForm =
    { text : String, calories : Int, datetime : Posix }


validateFields : TrimmedForm -> Result (List String) ValidForm
validateFields (Trimmed form) =
    let
        rCalories : Result (List String) Int
        rCalories =
            form.calories
                |> String.toInt
                |> Result.fromMaybe [ "Calories can't be blank and must be a number." ]

        rText : Result (List String) String
        rText =
            case String.isEmpty form.text of
                True ->
                    Err [ "Meal name can't be blank." ]

                False ->
                    Ok form.text

        rDatetime : Result (List String) Posix
        rDatetime =
            Iso8601.toTime form.datetime
                |> Result.mapError (\_ -> [ "Must be a valid date" ])
    in
        combine3 (\cal text dt -> { text = text, calories = cal, datetime = dt })
            rCalories
            rText
            rDatetime


combine2 : (v1 -> v2 -> r) -> Result (List a) v1 -> Result (List a) v2 -> Result (List a) r
combine2 f res1 res2 =
    case ( res1, res2 ) of
        ( Err er1, Err er2 ) ->
            Err (er1 ++ er2)

        ( Err er1, _ ) ->
            Err er1

        ( _, Err er2 ) ->
            Err er2

        ( Ok ok1, Ok ok2 ) ->
            Ok (f ok1 ok2)


combine3 : (v1 -> v2 -> v3 -> r) -> Result (List a) v1 -> Result (List a) v2 -> Result (List a) v3 -> Result (List a) r
combine3 f res1 res2 res3 =
    case ( combine2 f res1 res2, res3 ) of
        ( Err er12, Err er3 ) ->
            Err (er12 ++ er3)

        ( Err er12, Ok _ ) ->
            Err er12

        ( Ok _, Err er3 ) ->
            Err er3

        ( Ok ok12, Ok ok3 ) ->
            Ok (ok12 ok3)



-- combine3 : (v1 -> v2 -> v3 -> r) -> Result (List a) v1 -> Result (List a) v2 -> Result (List a) v3 -> Result (List a) r
-- combine3 f res1 res2 res3 =
--   case (res1, res2, res3) of


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields { text, calories, datetime } =
    Trimmed
        { text = String.trim text
        , calories = String.trim calories
        , datetime = String.trim datetime
        }



-- HTTP


create : ValidForm -> Cred -> Http.Request Meal
create validForm cred =
    let
        meal =
            Encode.object
                [ ( "text", Encode.string validForm.text )
                , ( "calories", Encode.int validForm.calories )
                , ( "datetime", Encode.string (Iso8601.fromTime validForm.datetime) )
                ]

        body =
            Encode.object [ ( "meal", meal ) ]
                |> Http.jsonBody
    in
        Decode.field "meal" (Meal.fullDecoder (Just cred))
            |> Api.post (Endpoint.meals []) (Just cred) body


edit : Slug -> ValidForm -> Cred -> Http.Request Meal
edit slug validForm cred =
    let
        meal =
            Encode.object
                [ ( "text", Encode.string validForm.text )
                , ( "calories", Encode.int validForm.calories )
                , ( "datetime", Encode.string (Iso8601.fromTime validForm.datetime) )
                ]

        body =
            Encode.object [ ( "meal", meal ) ]
                |> Http.jsonBody
    in
        Decode.field "meal" (Meal.fullDecoder (Just cred))
            |> Api.put (Endpoint.meal slug) cred body



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- INTERNAL


{-| Used for setting the page's title.
-}
getSlug : Status -> Maybe Slug
getSlug status =
    case status of
        Loading slug ->
            Just slug

        LoadingSlowly slug ->
            Just slug

        LoadingFailed slug ->
            Just slug

        Saving slug _ ->
            Just slug

        Editing slug _ _ ->
            Just slug

        EditingNew _ _ ->
            Nothing

        Creating _ ->
            Nothing
