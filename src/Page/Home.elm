module Page.Home
    exposing
        ( Model
        , Msg
        , init
        , subscriptions
        , toSession
        , update
        , view
        )

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Author
import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Preview)
import Article.Feed as Feed
import Avatar
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, value)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Http
import Loading
import Log
import Page
import Json.Decode as Decode
import PaginatedList exposing (PaginatedList)
import Session exposing (Session)
import Task exposing (Task)
import Time exposing (Posix)
import Url.Builder exposing (QueryParameter)
import Username exposing (Username)
import Route
import Meal exposing (Meal)
import Profile exposing (ProfileWithUsername)
import Iso8601
import Util exposing (isJust)


-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , feedTab : FeedTab
    , feedPage : Int

    -- Loaded independently from server
    , feed : Status Feed.Model
    , users : Status (List ProfileWithUsername)

    -- Filters
    , minTime : Maybe Int
    , maxTime : Maybe Int
    , minDate : Maybe Posix
    , maxDate : Maybe Posix
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type FeedTab
    = YourFeed Cred
    | GlobalFeed
    | AllUsers


isMealsTab : FeedTab -> Bool
isMealsTab feedTab =
    case feedTab of
        AllUsers ->
            False

        _ ->
            True


isYourFeed : FeedTab -> Bool
isYourFeed tab =
    case tab of
        YourFeed _ ->
            True

        _ ->
            False


checkLoggedIn : Session -> Cmd Msg
checkLoggedIn session =
    case Session.cred session of
        Just _ ->
            Cmd.none

        Nothing ->
            Route.replaceUrl (Session.navKey session) Route.Login


init : Session -> ( Model, Cmd Msg )
init session =
    let
        feedTab =
            case Session.cred session of
                Just cred ->
                    YourFeed cred

                Nothing ->
                    GlobalFeed

        model =
            { session = session
            , timeZone = Time.utc
            , feedTab = feedTab
            , feedPage = 1
            , feed = Loading
            , users = Loading
            , minTime = Nothing
            , maxTime = Nothing
            , minDate = Nothing
            , maxDate = Nothing
            }
    in
        ( model
        , Cmd.batch
            [ checkLoggedIn session
            , case feedTab of
                YourFeed _ ->
                    fetchMyMeals session 1 model
                        |> Task.attempt CompletedFeedLoad

                GlobalFeed ->
                    fetchAllMeals session 1 model
                        |> Task.attempt CompletedFeedLoad

                AllUsers ->
                    fetchUsers session 1
                        |> Task.attempt CompletedUsersLoad
            , Task.perform GotTimeZone Time.here
            , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
            ]
        )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div [ class "home-page" ]
            [ viewBanner
            , div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] <|
                        case model.feedTab of
                            AllUsers ->
                                case model.users of
                                    Loaded profileList ->
                                        [ div [ class "feed-toggle" ] <|
                                            List.concat
                                                [ [ viewTabs
                                                        (Session.cred model.session)
                                                        model.feedTab
                                                  ]
                                                , profileList
                                                    |> List.map (viewPreview (Session.cred model.session) model.timeZone)
                                                ]
                                        ]

                                    Loading ->
                                        []

                                    LoadingSlowly ->
                                        [ Loading.icon ]

                                    Failed ->
                                        [ Loading.error "feed" ]

                            _ ->
                                case model.feed of
                                    -- TODO: Renderear user en all meal
                                    Loaded feed ->
                                        [ div [ class "feed-toggle" ] <|
                                            List.concat
                                                [ [ viewTabs
                                                        (Session.cred model.session)
                                                        model.feedTab
                                                  ]
                                                , Feed.viewMeals model.timeZone feed (isYourFeed model.feedTab)
                                                    |> List.map (Html.map GotFeedMsg)
                                                ]
                                        ]

                                    Loading ->
                                        []

                                    LoadingSlowly ->
                                        [ Loading.icon ]

                                    Failed ->
                                        [ Loading.error "feed" ]
                    , viewFilters model
                    ]
                ]
            ]
    }


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "TopMeals" ]
            , p [] [ text "The best way to track your daily calorie intake." ]
            ]
        ]



--- USERS


viewPreview : Maybe Cred -> Time.Zone -> ProfileWithUsername -> Html Msg
viewPreview maybeCred timeZone profileFull =
    let
        profile =
            profileFull.profile

        username =
            profileFull.username

        calories =
            String.fromInt <| Profile.calories profile
    in
        div [ class "article-preview" ]
            [ div [ class "article-meta" ]
                [ a [ Route.href (Route.Profile username) ]
                    [ img [ Avatar.src (Profile.avatar profile) ] [] ]
                , div [ class "info" ]
                    [ Author.view username
                    , span [] [ text <| "Calories: " ++ calories ++ " (cal)" ]
                    ]
                ]
            ]



-- TABS


viewTabs : Maybe Cred -> FeedTab -> Html Msg
viewTabs maybeCred tab =
    case tab of
        YourFeed cred ->
            case Api.role cred of
                Api.Admin ->
                    Feed.viewTabs [] (yourFeed cred) [ allMeals, allUsers ]

                Api.Manager ->
                    Feed.viewTabs [] (yourFeed cred) [ allUsers ]

                Api.Regular ->
                    Feed.viewTabs [] (yourFeed cred) []

        GlobalFeed ->
            let
                otherTabs =
                    case maybeCred of
                        Just cred ->
                            [ yourFeed cred ]

                        Nothing ->
                            []
            in
                case Maybe.map Api.role maybeCred of
                    Just Api.Admin ->
                        Feed.viewTabs otherTabs allMeals [ allUsers ]

                    Just Api.Manager ->
                        Feed.viewTabs otherTabs allMeals [ allUsers ]

                    Just Api.Regular ->
                        Feed.viewTabs otherTabs allMeals []

                    _ ->
                        Feed.viewTabs [] allMeals []

        AllUsers ->
            let
                otherTabs =
                    (case maybeCred of
                        Just cred ->
                            [ yourFeed cred ]

                        Nothing ->
                            []
                    )
                        ++ (case Maybe.map Api.role maybeCred of
                                Just Api.Admin ->
                                    [ allMeals ]

                                _ ->
                                    []
                           )
            in
                Feed.viewTabs otherTabs allUsers []


yourFeed : Cred -> ( String, Msg )
yourFeed cred =
    ( "Your Meals", ClickedTab (YourFeed cred) )


allMeals : ( String, Msg )
allMeals =
    ( "All Meals", ClickedTab GlobalFeed )


allUsers : ( String, Msg )
allUsers =
    ( "All Users", ClickedAllUsersTab )


intToOption : Int -> Html msg
intToOption int =
    option [ value (String.fromInt int) ]
        [ text <| String.fromInt int ++ ":00" ]


viewFilters { minTime, maxTime, minDate, maxDate } =
    let
        minTimeRange =
            List.range 0 23

        maxTimeRange =
            List.range (Maybe.withDefault 0 minTime) 23

        emptyOption =
            option [ value "" ] [ text "Select an option" ]
    in
        div [ class "col-md-3" ] <|
            [ div [ class "sidebar" ] <|
                [ p [] [ text "Filter Meals" ]
                , fieldset [ class "form-group" ]
                    [ label [] [ text "Min time (hs)" ]
                    , select
                        [ class "form-control form-control-lg"
                        , onInput EnteredMinTime
                        ]
                        (emptyOption :: (List.map intToOption minTimeRange))
                    , label [] [ text "Max time (hs)" ]
                    , select
                        [ class "form-control form-control-lg"
                        , onInput EnteredMaxTime
                        ]
                        (emptyOption :: (List.map intToOption maxTimeRange))
                    ]
                , fieldset [ class "form-group" ]
                    [ label [] [ text "Min date (DD/MM/YYYY)" ]
                    , input
                        [ class "form-control form-control-lg"
                        , onInput EnteredMinDate
                        ]
                        []
                    , label [] [ text "Max date (DD/MM/YYYY)" ]
                    , input
                        [ class "form-control form-control-lg"
                        , onInput EnteredMaxDate
                        ]
                        []
                    ]
                ]
            ]



-- UPDATE


type Msg
    = ClickedTab FeedTab
    | ClickedAllUsersTab
    | ClickedFeedPage Int
    | CompletedFeedLoad (Result Http.Error Feed.Model)
    | CompletedUsersLoad (Result Http.Error (List ProfileWithUsername))
    | GotTimeZone Time.Zone
    | GotFeedMsg Feed.Msg
    | GotSession Session
    | PassedSlowLoadThreshold
    | EnteredMinTime String
    | EnteredMaxTime String
    | EnteredMinDate String
    | EnteredMaxDate String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTab tab ->
            let
                newModel =
                    { model | feedTab = tab }
            in
                ( newModel
                , fetchForTab newModel.feedTab newModel.session 1 newModel
                )

        ClickedAllUsersTab ->
            ( { model | feedTab = AllUsers }
            , fetchUsers model.session 1
                |> Task.attempt CompletedUsersLoad
            )

        ClickedFeedPage page ->
            ( { model | feedPage = page }
            , case model.feedTab of
                YourFeed _ ->
                    fetchMyMeals model.session page model
                        |> Task.attempt CompletedFeedLoad

                GlobalFeed ->
                    fetchAllMeals model.session page model
                        |> Task.attempt CompletedFeedLoad

                AllUsers ->
                    fetchUsers model.session page
                        |> Task.attempt CompletedUsersLoad
            )

        CompletedFeedLoad (Ok feed) ->
            ( { model | feed = Loaded feed }, Cmd.none )

        CompletedFeedLoad (Err error) ->
            ( { model | feed = Failed }, Cmd.none )

        CompletedUsersLoad (Ok users) ->
            ( { model | users = Loaded users }, Cmd.none )

        CompletedUsersLoad (Err _) ->
            ( { model | users = Failed }, Cmd.none )

        GotFeedMsg subMsg ->
            case model.feed of
                Loaded feed ->
                    let
                        ( newFeed, subCmd ) =
                            Feed.update (Session.cred model.session) subMsg feed
                    in
                        ( { model | feed = Loaded newFeed }
                        , Cmd.map GotFeedMsg subCmd
                        )

                Loading ->
                    ( model, Log.error )

                LoadingSlowly ->
                    ( model, Log.error )

                Failed ->
                    ( model, Log.error )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, checkLoggedIn session )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                feed =
                    case model.feed of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
                ( { model | feed = feed }, Cmd.none )

        EnteredMinTime sint ->
            let
                newModel =
                    { model | minTime = String.toInt sint }
            in
                ( newModel
                , fetchForTab newModel.feedTab newModel.session newModel.feedPage newModel
                )

        EnteredMaxTime sint ->
            let
                newModel =
                    { model | maxTime = String.toInt sint }
            in
                ( newModel
                , fetchForTab newModel.feedTab newModel.session newModel.feedPage newModel
                )

        EnteredMinDate sDate ->
            let
                formatted =
                    sDate
                        |> String.split "/"
                        |> List.reverse
                        |> String.join "-"
                        |> Iso8601.toTime
                        |> Result.toMaybe

                shouldRequest =
                    String.isEmpty sDate || isJust formatted

                newModel =
                    { model | minDate = formatted }
            in
                ( newModel
                , if shouldRequest then
                    fetchForTab newModel.feedTab newModel.session newModel.feedPage newModel
                  else
                    Cmd.none
                )

        EnteredMaxDate sDate ->
            let
                formatted =
                    sDate
                        |> String.split "/"
                        |> List.reverse
                        |> String.join "-"
                        |> Iso8601.toTime
                        |> Result.toMaybe

                newModel =
                    { model | maxDate = formatted }

                shouldRequest =
                    String.isEmpty sDate || isJust formatted
            in
                ( newModel
                , if shouldRequest then
                    fetchForTab newModel.feedTab newModel.session newModel.feedPage newModel
                  else
                    Cmd.none
                )



-- HTTP


type alias Filters a =
    { a
        | minDate : Maybe Posix
        , maxDate : Maybe Posix
        , minTime : Maybe Int
        , maxTime : Maybe Int
    }


makeFilters : Filters a -> List QueryParameter
makeFilters filters =
    let
        _ =
            Debug.log "" filters.minTime

        minTimeFilter =
            filters.minTime
                |> Maybe.map (\mind -> [ Url.Builder.int "minTime" mind ])
                |> Maybe.withDefault []

        maxTimeFilter =
            filters.maxTime
                |> Maybe.map (\mind -> [ Url.Builder.int "maxTime" mind ])
                |> Maybe.withDefault []

        minDateFilter =
            filters.minDate
                |> Maybe.map (\mdate -> [ Url.Builder.string "minDate" (Iso8601.fromTime mdate) ])
                |> Maybe.withDefault []

        maxDateFilter =
            filters.maxDate
                |> Maybe.map (\mdate -> [ Url.Builder.string "maxDate" (Iso8601.fromTime mdate) ])
                |> Maybe.withDefault []
    in
        minTimeFilter ++ maxTimeFilter ++ minDateFilter ++ maxDateFilter


fetchMyMeals : Session -> Int -> Filters a -> Task Http.Error Feed.Model
fetchMyMeals session page filters =
    let
        maybeCred =
            Session.cred session

        decoder =
            Feed.decoder maybeCred articlesPerPage

        filterParams =
            makeFilters filters

        params =
            filterParams ++ PaginatedList.params { page = page, resultsPerPage = articlesPerPage }
    in
        Api.get (Endpoint.mealsFeed params) maybeCred decoder
            |> Http.toTask
            |> Task.map (Feed.init session)


fetchAllMeals : Session -> Int -> Filters a -> Task Http.Error Feed.Model
fetchAllMeals session page filters =
    let
        maybeCred =
            Session.cred session

        decoder =
            Feed.decoder maybeCred articlesPerPage

        filterParams =
            makeFilters filters

        params =
            filterParams ++ PaginatedList.params { page = page, resultsPerPage = articlesPerPage }
    in
        Api.get (Endpoint.meals params) maybeCred decoder
            |> Http.toTask
            |> Task.map (Feed.init session)


fetchUsers : Session -> Int -> Task Http.Error (List ProfileWithUsername)
fetchUsers session page =
    let
        maybeCred =
            Session.cred session

        params =
            PaginatedList.params { page = page, resultsPerPage = articlesPerPage }
    in
        Api.get Endpoint.users maybeCred (Decode.list Profile.decoderFull)
            |> Http.toTask


fetchForTab : FeedTab -> Session -> Int -> Filters a -> Cmd Msg
fetchForTab feedTab session int filters =
    case feedTab of
        YourFeed _ ->
            fetchMyMeals session 1 filters
                |> Task.attempt CompletedFeedLoad

        GlobalFeed ->
            fetchAllMeals session 1 filters
                |> Task.attempt CompletedFeedLoad

        AllUsers ->
            fetchUsers session 1
                |> Task.attempt CompletedUsersLoad


articlesPerPage : Int
articlesPerPage =
    10


scrollToTop : Task x ()
scrollToTop =
    Dom.setViewport 0 0
        -- It's not worth showing the user anything special if scrolling fails.
        -- If anything, we'd log this to an error recording service.
        |> Task.onError (\_ -> Task.succeed ())



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



--- Util


maybeIntToString : Maybe Int -> String
maybeIntToString mint =
    mint
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ""
