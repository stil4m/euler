module Problems.Problem16 exposing (main)

import BigInt
import Html exposing (..)


main : Html msg
main =
    BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 1000)
        |> BigInt.toString
        |> String.split ""
        |> List.filterMap String.toInt
        |> List.sum
        |> Debug.toString
        |> text
