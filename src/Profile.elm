module Profile
    exposing
        ( Profile
        , ProfileWithUsername
        , avatar
        , bio
        , calories
        , username
        , decoder
        , decoderFull
        )

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


username : ProfileWithUsername -> Username
username p =
    p.username



-- SERIALIZATION


decoder : Decoder Profile
decoder =
    Decode.succeed Internals
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" Avatar.decoder
        |> required "expectedCalories" Decode.int
        |> Decode.map Profile


type alias ProfileWithUsername =
    { username : Username
    , profile : Profile
    }


decoderFull : Decoder ProfileWithUsername
decoderFull =
    decoder
        |> Decode.andThen
            (\profile ->
                Decode.succeed (\uname -> ProfileWithUsername uname profile)
                    |> required "username" Username.decoder
            )
