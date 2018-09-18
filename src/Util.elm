module Util
    exposing
        ( monthToString
        , combine2
        , combine3
        , toDDMMYYYY
        , toHHMM
        , isJust
        )

import Time exposing (Month(..), Posix, utc)


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


toDDMMYYYY : Posix -> String
toDDMMYYYY datetime =
    (intToStringPadded <| Time.toDay utc datetime) ++ "/" ++ (intToStringPadded <| monthToInt <| Time.toMonth utc datetime) ++ "/" ++ (intToStringPadded <| Time.toYear utc datetime)


toHHMM : Posix -> String
toHHMM datetime =
    (intToStringPadded <| Time.toHour utc datetime) ++ ":" ++ (intToStringPadded <| Time.toMinute utc datetime)


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


intToStringPadded : Int -> String
intToStringPadded int =
    if int < 10 then
        "0" ++ (String.fromInt int)
    else
        (String.fromInt int)


isJust : Maybe a -> Bool
isJust aMaybe =
    case aMaybe of
        Just _ ->
            True

        Nothing ->
            False
