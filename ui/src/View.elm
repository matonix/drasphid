module View exposing (..)

import Bpm
import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import List
import List.Extra
import Material.Button as Button
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options
import Material.Scheme as Scheme
import Material.Slider as Slider
import Material.Table as Table
import Material.Typography as Typo
import Maybe
import Models exposing (..)
import Msgs exposing (Msg(..))
import RemoteData exposing (WebData)
import Util


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
    Scheme.topWithScheme model.layout.primary model.layout.accent <|
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
            , main = [ mainArea model allSongs ]
            }


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title []
            [ img [ src "resources/title.svg" ] []
            ]
        ]
    ]


tabTitles : AllSongs -> List (Html Msg)
tabTitles =
    .songsByfoot >> List.map (.foot >> toString >> text)


mainArea : Model -> AllSongs -> Html Msg
mainArea model allSongs =
    Options.div [ Options.center ]
        [ viewAllSongs model allSongs ]


cardStyle : List (Options.Style Msg)
cardStyle =
    [ Options.css "width" "750px"
    , Options.css "margin" "24px"
    , Options.css "padding" "24px"
    , Elevation.e2
    ]


controlArea : Model -> Html Msg
controlArea model =
    Options.div
        cardStyle
        [ Options.div
            [ Options.css "display" "table"
            , Options.css "width" "100%"
            ]
            [ Options.div
                [ Options.css "display" "table-cell"
                , Options.css "width" "10%"
                ]
                [ Options.styled p
                    [ Typo.caption ]
                    [ text ("Spped " ++ toString model.speed) ]
                ]
            , Options.div [ Options.css "display" "table-cell" ]
                [ Slider.view
                    [ Slider.onChange Slider
                    , Slider.value model.speed
                    , Slider.max 900
                    , Slider.min 100
                    , Slider.step 10
                    ]
                ]
            ]
        ]


viewAllSongs : Model -> AllSongs -> Html Msg
viewAllSongs model allSongs =
    div []
        (controlArea model
            :: List.map (viewSongsByFoot model) allSongs.songsByfoot
        )


viewSongsByFoot : Model -> SongsByFoot -> Html Msg
viewSongsByFoot model songsByFoot =
    Options.div
        cardStyle
        [ Options.styled p
            [ Typo.display3 ]
            [ text (toString songsByFoot.foot) ]
        , Table.table
            [ Options.css "width" "100%"
            , Options.css "table-layout" "fixed"
            ]
            [ Table.thead [] [ songHead ]
            , Table.tbody [] (List.map (songRow model) songsByFoot.songs)
            ]
        ]


songHead : Html Msg
songHead =
    Table.tr []
        [ Table.td [ Options.css "width" "10%" ] [ btext "Folder" ]
        , Table.td [ Options.css "width" "40%" ] [ btext "Name" ]
        , Table.td [ Options.css "width" "25%" ] [ btext "BPM/Speed" ]
        , Table.td [ Options.css "width" "10%" ] [ btext "HS" ]
        , Table.td [ Options.css "width" "15%" ] [ btext "Notes" ]
        ]


btext str =
    b [] [ text str ]


songRow : Model -> Song -> Html Msg
songRow model song =
    let
        hss =
            song.bpm |> Bpm.toFloats |> List.map (Util.calcHs model.speed)
    in
    Table.tr []
        [ Table.td [] [ text song.folder ]
        , Table.td [] [ text song.name ]
        , Table.td []
            (hss
                |> List.map (\hs -> rtext model (song.bpm |> Bpm.mapString ((*) hs)))
                |> (::) (text song.bpm)
                |> List.intersperse (br [] [])
            )
        , Table.td []
            (hss
                |> List.map (\hs -> rtext model ("x" ++ toString hs))
                |> (::) (text "")
                |> List.intersperse (br [] [])
            )
        , Table.td [] [ text song.notes ]
        ]


rtext : Model -> String -> Html Msg
rtext model str =
    Options.span
        [ Color.text (Color.color model.layout.accent Color.A400)
        ]
        [ text str ]
