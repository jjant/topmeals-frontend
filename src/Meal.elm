module Meal exposing (..)

import Time exposing (Posix)


type alias Meal =
    { id : String
    , dateTime : Posix
    , text : String
    , calories : Int
    }


meals : List Meal
meals =
    [ { id = "hola1"
      , dateTime = Time.millisToPosix 0
      , text = "Churro con fritas"
      , calories = 1000
      }
    , { id = "hola2"
      , dateTime = Time.millisToPosix 0
      , text = "Mila con fritas"
      , calories = 400
      }
    , { id = "hola3"
      , dateTime = Time.millisToPosix 0
      , text = "Dulce de leche"
      , calories = 550
      }
    ]
