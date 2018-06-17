module Layout exposing (..)

import Material.Color as Color


type alias Model =
    { primary : Color.Hue
    }


model : Model
model =
    { primary = Color.Indigo }
