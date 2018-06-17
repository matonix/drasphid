module Cmds exposing (..)

import Http
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Models exposing (..)
import Msgs exposing (Msg)
import RemoteData


fetchAllSongs : Cmd Msg
fetchAllSongs =
    Http.get jsonPath decodeAllSongs
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchAllSongs


jsonPath =
    "resources/output.json"



-- semi-automatically generated by http://eeue56.github.io/json-to-elm/


decodeAllSongs : Json.Decode.Decoder AllSongs
decodeAllSongs =
    Json.Decode.Pipeline.decode AllSongs
        |> Json.Decode.Pipeline.required "songsByfoot" (Json.Decode.list decodeSongsByFoot)


encodeAllSongs : AllSongs -> Json.Encode.Value
encodeAllSongs record =
    Json.Encode.object
        [ ( "songsByfoot", Json.Encode.list <| List.map encodeSongsByFoot <| record.songsByfoot )
        ]


decodeSongsByFoot : Json.Decode.Decoder SongsByFoot
decodeSongsByFoot =
    Json.Decode.Pipeline.decode SongsByFoot
        |> Json.Decode.Pipeline.required "foot" Json.Decode.int
        |> Json.Decode.Pipeline.required "songs" (Json.Decode.list decodeSong)


encodeSongsByFoot : SongsByFoot -> Json.Encode.Value
encodeSongsByFoot record =
    Json.Encode.object
        [ ( "foot", Json.Encode.int <| record.foot )
        , ( "songs", Json.Encode.list <| List.map encodeSong <| record.songs )
        ]


decodeSong : Json.Decode.Decoder Song
decodeSong =
    Json.Decode.Pipeline.decode Song
        |> Json.Decode.Pipeline.required "folder" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "bpm" Json.Decode.string
        |> Json.Decode.Pipeline.required "notes" Json.Decode.string


encodeSong : Song -> Json.Encode.Value
encodeSong record =
    Json.Encode.object
        [ ( "folder", Json.Encode.string <| record.folder )
        , ( "name", Json.Encode.string <| record.name )
        , ( "bpm", Json.Encode.string <| record.bpm )
        , ( "notes", Json.Encode.string <| record.notes )
        ]