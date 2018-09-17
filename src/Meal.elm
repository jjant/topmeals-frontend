module Meal
    exposing
        ( Meal
        , fullDecoder
        , slug
        , fetch
        )

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Time exposing (Posix)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, hardcoded)
import Article.Slug as Slug exposing (Slug)
import Author exposing (Author)
import Http


type alias Meal =
    { slug : Slug
    , author : Author
    , datetime : Posix
    , text : String
    , calories : Int
    }



-- meals : List Meal
-- meals =
--     [ { id = "hola1"
--       , dateTime = Time.millisToPosix 0
--       , text = "Churro con fritas"
--       , calories = 1000
--       }
--     , { id = "hola2"
--       , dateTime = Time.millisToPosix 0
--       , text = "Mila con fritas"
--       , calories = 400
--       }
--     , { id = "hola3"
--       , dateTime = Time.millisToPosix 0
--       , text = "Dulce de leche"
--       , calories = 550
--       }
--     ]


fullDecoder : Maybe Cred -> Decoder Meal
fullDecoder maybeCred =
    Decode.succeed Meal
        |> required "id" Slug.decoder
        |> required "author" (Author.decoder maybeCred)
        -- TODO: Hacer
        -- |> required "datetime"
        |> hardcoded (Time.millisToPosix 0)
        |> required "text" Decode.string
        -- |> required "username" Decode.string
        |> required "calories" Decode.int



--


slug : Meal -> Slug
slug meal =
    meal.slug



-- SINGLE


fetch : Maybe Cred -> Slug -> Http.Request Meal
fetch maybeCred mealSlug =
    Decode.field "meal" (fullDecoder maybeCred)
        |> Api.get (Endpoint.meal mealSlug) maybeCred
