module Problems.Problem2 exposing (main)

import Elements.Fibonacci as Fibonacci
import Elements.Natural exposing (..)
import Html exposing (..)


main : Html msg
main =
    Fibonacci.while (\n -> n < 4000000)
        |> List.filter isEven
        |> List.sum
        |> String.fromInt
        |> text
