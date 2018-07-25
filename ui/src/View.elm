module View exposing (..)

import Char
import Html exposing (Html, div, h4, img, p, text)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import List
import List.Extra
import Material.Button as Button
import Material.Color as Color
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options
import Material.Table as Table
import Material.Typography as Typo
import Maybe
import Models exposing (..)
import Msgs exposing (Msg(..))
import RemoteData exposing (WebData)
import Result.Extra


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
            , [ Color.background (Color.color model.layout.primary Color.S600) ]
            )
        , main =
            [ controlArea model
            , viewAllSongs model allSongs
            ]
        }


header : Model -> List (Html Msg)
header model =
    [ Layout.row [ Color.background (Color.color model.layout.primary Color.S600) ]
        [ Layout.title []
            [ img [ src "resources/title.svg" ] []
            ]
        ]
    ]


tabTitles : AllSongs -> List (Html Msg)
tabTitles =
    .songsByfoot >> List.map (.foot >> toString >> text)


controlArea : Model -> Html Msg
controlArea model =
    div
        []
        [ Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.fab
            , Options.onClick (Increment 2)
            ]
            [ Icon.i "add" ]
        ]


viewAllSongs : Model -> AllSongs -> Html Msg
viewAllSongs model allSongs =
    div []
        (List.map (viewSongsByFoot model) allSongs.songsByfoot)


viewSongsByFoot : Model -> SongsByFoot -> Html Msg
viewSongsByFoot model songsByFoot =
    div []
        [ Options.styled p
            [ Options.css "align-self" "flex-end"
            , Typo.display3
            ]
            [ text (toString songsByFoot.foot) ]
        , Table.table []
            [ Table.thead [] [ songHead ]
            , Table.tbody [] (List.map (songRow model) songsByFoot.songs)
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


songRow : Model -> Song -> Html Msg
songRow model song =
    let
        bpm =
            song.bpm
                |> String.toList
                |> List.Extra.groupWhile (\x y -> Char.isDigit x == Char.isDigit y)
                |> List.map
                    (\x ->
                        if List.head x |> Maybe.withDefault ' ' |> Char.isDigit then
                            String.fromList x
                                |> String.toFloat
                                |> Result.map ((*) model.speed >> round >> toString)
                                |> Result.withDefault ""
                        else
                            String.fromList x
                    )
                |> String.concat
    in
    Table.tr []
        [ Table.td [] [ text song.folder ]
        , Table.td [] [ text song.name ]
        , Table.td [] [ text bpm ]
        , Table.td [] [ text song.notes ]
        ]
