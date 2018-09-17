module Viewer
    exposing
        ( Viewer
        , avatar
        , cred
        , calories
        , decoder
        , minPasswordChars
        , store
        , username
        )

{-| The logged-in user currently viewing this page. It stores enough data to
be able to render the menu bar (username and avatar), along with Cred so it's
impossible to have a Viewer if you aren't logged in.
-}

import Api exposing (Cred)
import Avatar exposing (Avatar)
import Email exposing (Email)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required, hardcoded)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Username exposing (Username)


-- TYPES


type Viewer
    = Viewer Avatar Cred Int



-- INFO


cred : Viewer -> Cred
cred (Viewer _ val _) =
    val


username : Viewer -> Username
username (Viewer _ val _) =
    Api.username val


avatar : Viewer -> Avatar
avatar (Viewer val _ _) =
    val


calories : Viewer -> Int
calories (Viewer _ _ cal) =
    cal


{-| Passwords must be at least this many characters long!
-}
minPasswordChars : Int
minPasswordChars =
    6



-- SERIALIZATION


decoder : Decoder (Cred -> Viewer)
decoder =
    Decode.succeed (\av cal credentials -> Viewer av credentials cal)
        |> custom (Decode.field "image" Avatar.decoder)
        -- TODO: Decode calories per day
        -- |> hardcoded 0
        |> required "expectedCalories" Decode.int


store : Viewer -> Cmd msg
store (Viewer avatarVal credVal cals) =
    Api.storeCredWith
        credVal
        avatarVal
        cals
