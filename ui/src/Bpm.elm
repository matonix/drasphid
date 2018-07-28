module Bpm
    exposing
        ( Bpm
        , fromString
        , map
        , mapString
        , toFloats
        , toString
        )

import Char
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra


type alias Bpm =
    List (Either String Float)


toString : Bpm -> String
toString =
    List.map (Either.unpack identity (round >> Basics.toString)) >> String.concat


map : (Float -> Float) -> Bpm -> Bpm
map f =
    List.map (Either.map f)


fromString : String -> Bpm
fromString =
    String.toList
        >> List.map toEither
        >> groupEither
        >> List.map (Either.mapEach String.fromList >> Either.map strToFloat)


mapString : (Float -> Float) -> String -> String
mapString f =
    fromString >> map f >> toString


toFloats : String -> List Float
toFloats =
    fromString >> Either.rights


toEither : Char -> Either Char Char
toEither char =
    if Char.isDigit char then
        Right char
    else
        Left char


groupEither : List (Either a b) -> List (Either (List a) (List b))
groupEither xs_ =
    case xs_ of
        [] ->
            []

        x_ :: xs ->
            let
                ( ys, zs ) =
                    List.Extra.span (eqEither x_) xs
            in
            case x_ of
                Left x ->
                    Left (x :: Either.lefts ys) :: groupEither zs

                Right x ->
                    Right (x :: Either.rights ys) :: groupEither zs


eqEither : Either a b -> Either a b -> Bool
eqEither x y =
    Either.isLeft x == Either.isLeft y


strToFloat : String -> Float
strToFloat =
    String.toFloat >> Result.withDefault 0
