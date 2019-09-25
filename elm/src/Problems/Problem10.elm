module Problems.Problem10 exposing (main)

import Elements.Eratosthenes as Eratosthenes
import Elements.Primes as Primes
import Html exposing (..)


main : Html msg
main =
    Eratosthenes.upTo 2000000
        |> List.sum
        |> Debug.toString
        |> text
