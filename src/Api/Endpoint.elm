module Api.Endpoint
    exposing
        ( Endpoint
        , article
        , articles
        , feed
        , follow
        , login
        , profiles
        , request
        , tags
        , user
        , users
        , meal
        , meals
        , mealsFeed
        )

import Article.Slug as Slug exposing (Slug)
import Http
import Url.Builder exposing (QueryParameter)
import Username exposing (Username)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , withCredentials : Bool
    }
    -> Http.Request a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , withCredentials = config.withCredentials
        }



-- TYPES


{-| Get a URL to the Conduit API.

This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.

-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin "https://topmeals-backend.now.sh"
        -- Url.Builder.crossOrigin "https://topcalories-upuvlunrwl.now.sh"
        ("api" :: paths)
        queryParams
        |> Endpoint



-- ENDPOINTS


login : Endpoint
login =
    url [ "users", "login" ] []


user : Endpoint
user =
    url [ "user" ] []


users : Endpoint
users =
    url [ "users" ] []


follow : Username -> Endpoint
follow uname =
    url [ "profiles", Username.toString uname, "follow" ] []



-- ARTICLE ENDPOINTS


article : Slug -> Endpoint
article slug =
    url [ "articles", Slug.toString slug ] []


articles : List QueryParameter -> Endpoint
articles params =
    url [ "articles" ] params


profiles : Username -> Endpoint
profiles uname =
    url [ "profiles", Username.toString uname ] []


feed : List QueryParameter -> Endpoint
feed params =
    url [ "articles", "feed" ] params


tags : Endpoint
tags =
    url [ "tags" ] []



---- MEALS


meal : Slug -> Endpoint
meal slug =
    url [ "meals", Slug.toString slug ] []


meals : List QueryParameter -> Endpoint
meals params =
    url [ "meals" ] params


mealsFeed : List QueryParameter -> Endpoint
mealsFeed params =
    url [ "meals", "feed" ] params
