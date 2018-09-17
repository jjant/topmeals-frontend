module Article.Feed
    exposing
        ( Model
        , Msg
        , decoder
        , init
        , update
          -- , viewArticles
        , viewPagination
        , viewTabs
        , viewMeals
        )

import Api exposing (Cred)
import Article exposing (Article, Preview)
import Article.Slug as ArticleSlug exposing (Slug)
import Author
import Avatar exposing (Avatar)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Page
import PaginatedList exposing (PaginatedList)
import Profile
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time exposing (..)
import Util exposing (monthToString)
import Timestamp
import Url exposing (Url)
import Username exposing (Username)
import Meal exposing (Meal)
import List.Extra
import Viewer


{-| NOTE: This module has its own Model, view, and update. This is not normal!
If you find yourself doing this often, please watch <https://www.youtube.com/watch?v=DoA4Txr4GUs>

This is the reusable Article Feed that appears on both the Home page as well as
on the Profile page. There's a lot of logic here, so it's more convenient to use
the heavyweight approach of giving this its own Model, view, and update.

This means callers must use Html.map and Cmd.map to use this thing, but in
this case that's totally worth it because of the amount of logic wrapped up
in this thing.

For every other reusable view in this application, this API would be totally
overkill, so we use simpler APIs instead.

-}



-- MODEL


type Model
    = Model Internals


{-| This should not be exposed! We want to benefit from the guarantee that only
this module can create or alter this model. This way if it ever ends up in
a surprising state, we know exactly where to look: this module.
-}
type alias Internals =
    { session : Session
    , errors : List String
    , meals : PaginatedList Meal
    , isLoading : Bool
    }


init : Session -> PaginatedList Meal -> Model
init session meals =
    Model
        { session = session
        , errors = []
        , meals = meals
        , isLoading = False
        }



-- VIEW


viewMeals : Time.Zone -> Model -> List (Html Msg)
viewMeals timeZone (Model { meals, session, errors }) =
    let
        maybeCred =
            Session.cred session

        mealsHtml =
            PaginatedList.values meals
                |> groupFeedByDay
                |> List.map (viewMealsForDay session timeZone)
    in
        Page.viewErrors ClickedDismissErrors errors :: mealsHtml


viewDate : Posix -> String
viewDate time =
    (String.fromInt <| toDay utc time) ++ "/" ++ (monthToString <| toMonth utc time) ++ "/" ++ (String.fromInt <| toYear utc time)


viewTime : Posix -> String
viewTime time =
    (String.fromInt <| toHour utc time) ++ ":" ++ (String.fromInt <| toMinute utc time)


type CalorieResult
    = Over
    | Under
    | Unknown


viewMealsForDay : Session -> Time.Zone -> List Meal -> Html Msg
viewMealsForDay session timeZone mealsForDay =
    let
        caloriesInMeals =
            mealsForDay
                |> List.map (\a -> a.calories)
                |> List.sum

        calorieResult =
            session
                |> Session.viewer
                |> Maybe.map Viewer.calories
                |> Maybe.map
                    (\cal ->
                        if caloriesInMeals <= cal then
                            Under
                        else
                            Over
                    )
                |> Maybe.withDefault Unknown
    in
        List.head mealsForDay
            |> Maybe.map .datetime
            |> Maybe.map (Timestamp.format timeZone)
            |> Maybe.map
                (\d ->
                    div []
                        [ h3 [ class "meals-group" ] [ text d ]
                        , div [] ((List.map (viewMeal calorieResult timeZone)) mealsForDay)
                        ]
                )
            |> Maybe.withDefault (text "")


viewMeal : CalorieResult -> Time.Zone -> Meal -> Html Msg
viewMeal cr timeZone meal =
    let
        crClass =
            case cr of
                Over ->
                    "over-calories"

                Under ->
                    "under-calories"

                Unknown ->
                    ""
    in
        div [ class <| "article-preview " ++ crClass ]
            [ a [ class "preview-link", Route.href (Route.Meal <| Meal.slug meal) ]
                [ h1 [] [ text <| meal.text ]
                , p [] [ text <| "Date: " ++ (viewDate meal.datetime) ]
                , p [] [ text <| "Time: " ++ (viewTime meal.datetime) ]
                , p [] [ text <| "Calories: " ++ (String.fromInt meal.calories) ++ " (cal)" ]
                ]
            ]


viewTabs :
    List ( String, msg )
    -> ( String, msg )
    -> List ( String, msg )
    -> Html msg
viewTabs before selected after =
    ul [ class "nav nav-pills outline-active" ] <|
        List.concat
            [ List.map (viewTab []) before
            , [ viewTab [ class "active" ] selected ]
            , List.map (viewTab []) after
            ]


viewTab : List (Attribute msg) -> ( String, msg ) -> Html msg
viewTab attrs ( name, msg ) =
    li [ class "nav-item" ]
        [ -- Note: The RealWorld CSS requires an href to work properly.
          a (class "nav-link" :: onClick msg :: href "" :: attrs)
            [ text name ]
        ]


viewPagination : (Int -> msg) -> Int -> Model -> Html msg
viewPagination toMsg page (Model feed) =
    let
        viewPageLink currentPage =
            pageLink toMsg currentPage (currentPage == page)

        totalPages =
            PaginatedList.total feed.meals
    in
        if totalPages > 1 then
            List.range 1 totalPages
                |> List.map viewPageLink
                |> ul [ class "pagination" ]
        else
            Html.text ""


pageLink : (Int -> msg) -> Int -> Bool -> Html msg
pageLink toMsg targetPage isActive =
    li [ classList [ ( "page-item", True ), ( "active", isActive ) ] ]
        [ a
            [ class "page-link"
            , onClick (toMsg targetPage)

            -- The RealWorld CSS requires an href to work properly.
            , href ""
            ]
            [ text (String.fromInt targetPage) ]
        ]


viewTag : String -> Html msg
viewTag tagName =
    li [ class "tag-default tag-pill tag-outline" ] [ text tagName ]



-- UPDATE


type Msg
    = ClickedDismissErrors


update : Maybe Cred -> Msg -> Model -> ( Model, Cmd Msg )
update maybeCred msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = [] }, Cmd.none )


replaceArticle : Article a -> Article a -> Article a
replaceArticle newArticle oldArticle =
    if Article.slug newArticle == Article.slug oldArticle then
        newArticle
    else
        oldArticle



-- SERIALIZATION


decoder : Maybe Cred -> Int -> Decoder (PaginatedList Meal)
decoder maybeCred resultsPerPage =
    Decode.succeed PaginatedList.fromList
        |> required "mealsCount" (pageCountDecoder resultsPerPage)
        |> required "meals" (Decode.list (Meal.fullDecoder maybeCred))


pageCountDecoder : Int -> Decoder Int
pageCountDecoder resultsPerPage =
    Decode.int
        |> Decode.map (\total -> ceiling (toFloat total / toFloat resultsPerPage))



-- Sort by date and group


groupFeedByDay : List Meal -> List (List Meal)
groupFeedByDay mealList =
    mealList
        |> List.sortBy (\a -> negate <| Time.posixToMillis a.datetime)
        |> List.Extra.groupWhile (\a b -> isSameDate (a.datetime) (b.datetime))
        |> List.map (\( x, xs ) -> x :: xs)



--


isSameDate : Posix -> Posix -> Bool
isSameDate p1 p2 =
    (Time.toDay utc p1 == Time.toDay utc p2)
        && (Time.toMonth utc p1 == Time.toMonth utc p2)
        && (Time.toYear utc p1 == Time.toYear utc p2)
