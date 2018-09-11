module Loading exposing (error, icon, centeredIcon, slowThreshold)

{-| A loading spinner icon.
-}

import Asset
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (alt, height, src, width, class)
import Process
import Task exposing (Task)


icon : Html msg
icon =
    Html.img
        [ Asset.src Asset.loading
        , width 64
        , height 64
        , alt "Loading..."
        ]
        []


centeredIcon : Html msg
centeredIcon =
    div [ class "centered-spinner" ]
        [ icon ]


error : String -> Html msg
error str =
    Html.text ("Error loading " ++ str ++ ".")


slowThreshold : Task x ()
slowThreshold =
    Process.sleep 500
