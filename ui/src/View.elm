module View exposing (..)

import Html exposing (Html, div, h4, img, p, text)
import Html.Attributes exposing (class, href, src)
import List exposing (head)
import Material.Color as Color
import Material.Layout as Layout
import Material.Options as Options
import Material.Table as Table
import Material.Typography as Typo
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
            [ img [ src "resources/title.svg" ] [] ]
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
    div []
        [ Options.styled p
            [ Options.css "align-self" "flex-end"
            , Typo.display3
            ]
            [ text (toString songsByFoot.foot) ]
        , Table.table []
            [ Table.thead [] [ songHead ]
            , Table.tbody [] (List.map songRow songsByFoot.songs)
            ]
        ]


songHead : Html Msg
songHead =
    Table.tr []
        [ Table.td [] [ text "Folder" ]
        , Table.td [] [ text "Name" ]
        , Table.td [] [ text "BPM" ]
        , Table.td [] [ text "Notes" ]
        ]


songRow : Song -> Html Msg
songRow song =
    Table.tr []
        [ Table.td [] [ text song.folder ]
        , Table.td [] [ text song.name ]
        , Table.td [] [ text song.bpm ]
        , Table.td [] [ text song.notes ]
        ]
