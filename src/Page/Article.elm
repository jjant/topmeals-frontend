module Page.Article exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| Viewing an individual article.
-}

import Api exposing (Cred, Role(..))
import Api.Endpoint as Endpoint
import Article.Slug as Slug exposing (Slug)
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Avatar
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Loading
import Log
import Page
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Username exposing (Username)
import Viewer exposing (Viewer)
import Meal exposing (Meal)


-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , errors : List String

    -- Loaded independently from server
    , meal : Status Meal
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


{-| A regular user is only allowed to see his meals.
A manager is only allowed to see his meals.
An admin can see all users' meals
Unlogged viewers can't see profiles.
Those that are not authorized will be redirected to home screen.
-}
checkPermissions : Session -> Username -> Cmd Msg
checkPermissions session profileUsername =
    let
        redirectToHome =
            Route.replaceUrl (Session.navKey session) Route.Home
    in
        session
            |> Session.cred
            |> Maybe.map (\cred -> ( Api.username cred, Api.role cred ))
            |> Maybe.map
                (\( username, role ) ->
                    case role of
                        Regular ->
                            if username == profileUsername then
                                Cmd.none
                            else
                                redirectToHome

                        Manager ->
                            redirectToHome

                        Admin ->
                            Cmd.none
                )
            |> Maybe.withDefault redirectToHome


init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    let
        maybeCred =
            Session.cred session
    in
        ( { session = session
          , timeZone = Time.utc
          , errors = []
          , meal = Loading
          }
        , Cmd.batch
            [ Meal.fetch maybeCred slug
                |> Http.send CompletedLoadMeal
            , Task.perform GotTimeZone Time.here
            , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
            ]
        )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    case model.meal of
        Loaded meal ->
            let
                author =
                    Meal.author meal

                avatar =
                    Profile.avatar (Author.profile author)

                slug =
                    Meal.slug meal

                profile =
                    Author.profile author

                buttons =
                    case Session.cred model.session of
                        Just cred ->
                            viewButtons cred meal author

                        Nothing ->
                            []
            in
                { title = meal.text
                , content =
                    div [ class "article-page" ]
                        [ div [ class "banner" ]
                            [ div [ class "container" ]
                                [ h1 [] [ text meal.text ]
                                , div [ class "article-meta" ] <|
                                    List.append
                                        [ a [ Route.href (Route.Profile (Author.username author)) ]
                                            [ img [ Avatar.src (Profile.avatar profile) ] [] ]
                                        , div [ class "info" ]
                                            [ Author.view (Author.username author)
                                            , Timestamp.view model.timeZone meal.datetime
                                            ]
                                        ]
                                        buttons
                                , Page.viewErrors ClickedDismissErrors model.errors
                                ]
                            ]
                        , div [ class "container page" ]
                            [ div [ class "row article-content" ]
                                [ div [ class "col-md-12" ]
                                    [ p [] [ text <| "Calories: " ++ (String.fromInt meal.calories) ++ " (cal)" ] ]
                                ]
                            , hr [] []
                            ]
                        ]
                }

        Loading ->
            { title = "Meal", content = text "" }

        LoadingSlowly ->
            { title = "Meal", content = Loading.centeredIcon }

        Failed ->
            { title = "Meal", content = Loading.error "meal" }


isSameUser : Cred -> Author -> Bool
isSameUser cred author =
    Api.username cred == Author.username author


viewButtons : Cred -> Meal -> Author -> List (Html Msg)
viewButtons cred meal author =
    let
        defaultButtons =
            [ text ""
            , text ""
            , text ""
            ]
    in
        case Api.role cred of
            Admin ->
                [ editButton meal
                , text " "
                , deleteButton cred meal
                ]

            Manager ->
                defaultButtons

            Regular ->
                if isSameUser cred author then
                    [ editButton meal
                    , text " "
                    , deleteButton cred meal
                    ]
                else
                    defaultButtons



-- UPDATE


type Msg
    = ClickedDeleteMeal Cred Slug
    | ClickedDismissErrors
    | CompletedLoadMeal (Result Http.Error Meal)
    | CompletedDeleteMeal (Result Http.Error ())
    | GotTimeZone Time.Zone
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        CompletedLoadMeal (Ok meal) ->
            ( { model | meal = Loaded meal }, Cmd.none )

        CompletedLoadMeal (Err error) ->
            ( { model | meal = Failed }
            , Log.error
            )

        ClickedDeleteMeal cred slug ->
            ( model
            , delete slug cred
                |> Http.send CompletedDeleteMeal
            )

        CompletedDeleteMeal (Ok ()) ->
            ( model, Route.replaceUrl (Session.navKey model.session) Route.Home )

        CompletedDeleteMeal (Err error) ->
            ( model, Route.replaceUrl (Session.navKey model.session) Route.Home )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                meal =
                    case model.meal of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
                ( { model | meal = meal }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- HTTP


delete : Slug -> Cred -> Http.Request ()
delete slug cred =
    Api.delete (Endpoint.meal slug) cred Http.emptyBody (Decode.succeed ())



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- INTERNAL


deleteButton : Cred -> Meal -> Html Msg
deleteButton cred meal =
    let
        msg =
            ClickedDeleteMeal cred (Meal.slug meal)
    in
        button [ class "btn btn-outline-danger btn-sm", onClick msg ]
            [ i [ class "ion-trash-a" ] [], text " Delete Meal" ]


editButton : Meal -> Html Msg
editButton meal =
    a
        [ class "btn btn-outline-secondary btn-sm"
        , Route.href (Route.EditArticle (Meal.slug meal))
        ]
        [ i [ class "ion-edit" ] [], text " Edit Meal" ]
