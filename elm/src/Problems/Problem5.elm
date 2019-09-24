module Problems.Problem5 exposing (main)

import Elements.Factors as Factors
import Html exposing (..)


main : Html msg
main =
    List.range 1 20
        |> List.map Factors.factors
        |> Factors.flatten
        |> Factors.sum
        |> String.fromInt
        |> text


firstNumberDivisableBy : List Int -> Int
firstNumberDivisableBy xs =
    let
        firstNumberDivisableByRec n ys =
            case ys of
                [] ->
                    n

                y :: rest ->
                    if modBy y n == 0 then
                        firstNumberDivisableByRec n rest

                    else
                        firstNumberDivisableByRec (y * n) rest
    in
    firstNumberDivisableByRec 1 xs
