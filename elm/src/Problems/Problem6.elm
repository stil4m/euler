module Problems.Problem6 exposing (main)

import Html exposing (..)


main : Html msg
main =
    let
        till =
            100
    in
    (squareOfSum till - sumOfSquares till)
        |> Debug.toString
        |> text


sumOfSquares n =
    n
        |> List.range 1
        |> List.map (\x -> x * x)
        |> List.sum


squareOfSum n =
    let
        x =
            List.sum (List.range 1 n)
    in
    x * x
