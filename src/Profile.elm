module Profile exposing (Profile, avatar, bio, calories, decoder)

{-| A user's profile - potentially your own!

Contrast with Cred, which is the currently signed-in user.

-}

import Api exposing (Cred)
import Avatar exposing (Avatar)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, hardcoded)
import Username exposing (Username)


-- TYPES


type Profile
    = Profile Internals


type alias Internals =
    { bio : Maybe String
    , avatar : Avatar
    , calories : Int
    }



-- INFO


bio : Profile -> Maybe String
bio (Profile info) =
    info.bio


avatar : Profile -> Avatar
avatar (Profile info) =
    info.avatar


calories : Profile -> Int
calories (Profile info) =
    info.calories



-- SERIALIZATION


decoder : Decoder Profile
decoder =
    Decode.succeed Internals
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" Avatar.decoder
        |> hardcoded 42
        -- TODO: Add this when API is ready
        -- |> required "calories" Decode.int
        |> Decode.map Profile
