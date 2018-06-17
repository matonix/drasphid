module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import List exposing (head)
import Models exposing (..)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)


view : Model -> Html Msg
view model =
    div
        []
        [ page model ]


page : Model -> Html Msg
page model =
    div []
        [ nav
        , maybeSongs model.allSongs
        ]


nav : Html Msg
nav =
    div [ class "clearfix mb2 white bg-black" ]
        [ div [ class "left p2" ] [ text "Drasphid" ] ]


maybeSongs : WebData AllSongs -> Html Msg
maybeSongs response =
    case response of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success allSongs ->
            viewAllSongs allSongs

        RemoteData.Failure error ->
            text (toString error)


viewAllSongs : AllSongs -> Html Msg
viewAllSongs allSongs =
    div []
        (List.map viewSongsByFoot allSongs.songsByfoot)


viewSongsByFoot : SongsByFoot -> Html Msg
viewSongsByFoot songsByFoot =
    div [ class "p2" ]
        [ h1 [] [ text (toString songsByFoot.foot) ]
        , table []
            [ thead [] [ songHead ]
            , tbody [] (List.map songRow songsByFoot.songs)
            ]
        ]


songHead : Html Msg
songHead =
    tr []
        [ td [] [ text "Folder" ]
        , td [] [ text "Name" ]
        , td [] [ text "BPM" ]
        , td [] [ text "Notes" ]
        ]


songRow : Song -> Html Msg
songRow song =
    tr []
        [ td [] [ text song.folder ]
        , td [] [ text song.name ]
        , td [] [ text song.bpm ]
        , td [] [ text song.notes ]
        ]
