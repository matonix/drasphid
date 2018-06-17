module Msgs exposing (..)

import Material
import Models exposing (AllSongs)
import RemoteData exposing (WebData)


type Msg
    = OnFetchAllSongs (WebData AllSongs)
    | Mdl (Material.Msg Msg)
