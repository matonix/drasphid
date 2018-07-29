module View exposing (..)

import Bpm
import Html exposing (Html, b, br, p, text)
import Html.Attributes exposing (src)
import List
import List.Extra
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid exposing (..)
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options exposing (..)
import Material.Scheme as Scheme
import Material.Slider as Slider
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
            , Layout.selectedTab model.selectedTab
            , Layout.onSelectTab SelectTab
            , Layout.fixedTabs

            -- , Layout.waterfall True
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
            [ img [] [ src "resources/title.svg" ]
            ]
        ]
    ]


tabTitles : AllSongs -> List (Html Msg)
tabTitles =
    .songsByfoot
        >> List.reverse
        >> List.map (.foot >> toString >> text)


mainArea : Model -> AllSongs -> Html Msg
mainArea model allSongs =
    div [ center ]
        [ div []
            [ controlArea model
            , viewSelectedSongs model allSongs
            ]
        ]


cardStyle : List (Style Msg)
cardStyle =
    [ css "max-width" "800px"
    , css "margin" "16px"
    , Elevation.e2
    ]


controlArea : Model -> Html Msg
controlArea model =
    div
        []
        [ div
            [ css "display" "table"
            , css "width" "100%"
            , css "padding" "16px"
            , css "position" "fixed"
            , css "bottom" "0px"
            , css "left" "0px"
            , css "z-index" "9999"
            , Elevation.e2
            , Color.background (Color.color model.layout.primary Color.S400)
            , Color.text Color.white
            ]
            [ div
                [ css "display" "table-cell"
                , css "width" "10%"
                ]
                [ styled p
                    [ Typo.body1 ]
                    [ text (toString model.speed) ]
                ]
            , div
                [ css "display" "table-cell"
                , css "width" "90%"
                ]
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


viewSelectedSongs : Model -> AllSongs -> Html Msg
viewSelectedSongs model =
    .songsByfoot
        >> List.reverse
        >> List.Extra.getAt model.selectedTab
        >> Maybe.map (viewSongsByFoot model >> List.singleton)
        >> Maybe.withDefault []
        >> div []


viewSongsByFoot : Model -> SongsByFoot -> Html Msg
viewSongsByFoot model songsByFoot =
    div
        cardStyle
        [ styled p
            [ Typo.display1
            , css "padding" "16px"
            ]
            [ text
                (if songsByFoot.foot == 8 then
                    "8以下"
                 else
                    toString songsByFoot.foot
                )
            ]
        , List.map (songRow model) songsByFoot.songs |> div []
        ]


songRow : Model -> Song -> Html Msg
songRow model song =
    let
        hss =
            song.bpm |> Bpm.toFloats |> List.map (Util.calcHs model.speed)
    in
    Card.view []
        [ Card.title [] [ text song.name ]
        , Card.text [ css "width" "100%", css "display" "table" ]
            [ div [ css "width" "35%", css "display" "table-cell" ]
                [ text song.bpm ]
            , div [ css "width" "45%", css "display" "table-cell" ]
                (hss
                    |> List.map (\hs -> text (song.bpm |> Bpm.mapString ((*) hs)))
                    |> List.intersperse (br [] [])
                )
            , div [ css "width" "20%", css "display" "table-cell" ]
                (hss
                    |> List.map (\hs -> text ("x" ++ toString hs))
                    |> List.intersperse (br [] [])
                )
            ]
        ]


btext : String -> Html Msg
btext str =
    b [] [ text str ]


rtext : Model -> String -> Html Msg
rtext model str =
    span
        [ Color.text (Color.color model.layout.accent Color.A400) ]
        [ text str ]
