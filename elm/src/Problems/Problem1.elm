module Problems.Problem1 exposing (main)

import Elements.Natural exposing (..)
import Html exposing (..)


main : Html msg
main =
    List.range 1 (1000 - 1)
        |> List.filter (or (multipleOf 3) (multipleOf 5))
        |> List.sum
        |> String.fromInt
        |> text


or : (a -> Bool) -> (a -> Bool) -> (a -> Bool)
or f g =
    \x -> f x || g x
