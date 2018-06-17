module Models exposing (..)

import RemoteData exposing (WebData)


type alias Model =
    { allSongs : WebData AllSongs
    }


initModel : Model
initModel =
    { allSongs = RemoteData.Loading
    }



-- semi-automatically generated by http://eeue56.github.io/json-to-elm/


type alias AllSongs =
    { songsByfoot : List SongsByFoot
    }


type alias SongsByFoot =
    { foot : Int
    , songs : List Song
    }


type alias Song =
    { folder : String
    , name : String
    , bpm : String
    , notes : String
    }
