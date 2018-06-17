port module Main exposing (..)

import Cmds exposing (fetchAllSongs)
import Html exposing (program)
import Models exposing (Model, initModel)
import Msgs exposing (Msg)
import Subs exposing (subscriptions)
import Update exposing (update)
import View exposing (view)


init : ( Model, Cmd Msg )
init =
    ( initModel, fetchAllSongs )


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
