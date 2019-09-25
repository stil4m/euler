module Problems.Problem20 exposing (main)

import BigInt
import Html exposing (..)


main : Html msg
main =
    factorial 100
        |> String.split ""
        |> List.filterMap String.toInt
        |> List.sum
        |> Debug.toString
        |> text


factorial : Int -> String
factorial n =
    List.range 1 n
        |> List.map BigInt.fromInt
        |> List.foldl BigInt.mul (BigInt.fromInt 1)
        |> BigInt.toString
