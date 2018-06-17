port module Subs exposing (..)

import Json.Decode
import Model exposing (Model)


subscriptions : Model -> Sub Msg
subscriptions model =
    allSongs mapAllsongs


mapAllsongs : Json.Decode.Value -> Msg
mapAllsongs modelJson =
    case decodeModel modelJson of
        Ok model ->
            SetModel model

        Err errorMessage ->
            let
                _ =
                    Debug.log "Error in mapAllsongs:" errorMessage
            in
            NoOp


port allSongs : (Json.Decode.Value -> msg) -> Sub msg
