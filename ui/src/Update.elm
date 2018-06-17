module Update exposing (..)

import Models exposing (Model)
import Msgs exposing (Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFetchAllSongs response ->
            ( { model | allSongs = response }, Cmd.none )
