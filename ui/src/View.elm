module View exposing (..)

import Html exposing (Html, div, h1, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, href)
import List exposing (head)
import Material.Color as Color
import Material.Layout as Layout
import Models exposing (..)
import Msgs exposing (Msg(..))
import RemoteData exposing (WebData)


view : Model -> Html Msg
view model =
    div
        []
        [ response model ]


response : Model -> Html Msg
response model =
    case model.allSongs of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success allSongs ->
            page model allSongs

        RemoteData.Failure error ->
            text (toString error)


page : Model -> AllSongs -> Html Msg
page model allSongs =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = header model
        , drawer = []
        , tabs =
            ( tabTitles allSongs
            , [ Color.background (Color.color model.layout.primary Color.S400) ]
            )
        , main = [ viewAllSongs allSongs ]
        }


header : Model -> List (Html Msg)
header model =
    [ Layout.row [ Color.background (Color.color model.layout.primary Color.S600) ]
        [ Layout.title []
            [ text "Drasphid" ]
        ]
    ]


tabTitles : AllSongs -> List (Html Msg)
tabTitles =
    .songsByfoot >> List.map (.foot >> toString >> text)


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
