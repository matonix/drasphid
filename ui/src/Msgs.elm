module Msgs exposing (..)

import Models exposing (AllSongs)
import RemoteData exposing (WebData)


type Msg
    = OnFetchAllSongs (WebData AllSongs)
