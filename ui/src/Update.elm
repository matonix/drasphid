module Update exposing (..)

import Models exposing (Model)
import Msgs exposing (Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetModel newModel ->
            ( newModel
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )
