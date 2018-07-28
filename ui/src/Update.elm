module Update exposing (..)

import Material
import Models exposing (Model)
import Msgs exposing (Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFetchAllSongs response ->
            ( { model | allSongs = response }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        Slider value ->
            ( { model | speed = value }, Cmd.none )
