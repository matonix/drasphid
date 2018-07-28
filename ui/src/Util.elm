module Util
    exposing
        ( calcHs
        )

import List
import Maybe


calcHs : Float -> Float -> Float
calcHs speed bpm =
    hss
        |> List.sortBy (\hs -> bpm * hs - speed |> abs)
        |> List.head
        |> Maybe.withDefault 0


hss : List Float
hss =
    List.map ((*) 0.25 << toFloat) (List.range 1 16)
        ++ List.map ((*) 0.5 << toFloat) (List.range 9 16)
