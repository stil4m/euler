module Problems.Problem7 exposing (main)

import Elements.Primes as Primes
import Html exposing (..)


main : Html msg
main =
    Primes.primes
        |> Primes.take 10001
        |> List.reverse
        |> List.head
        |> Maybe.withDefault 0
        |> Debug.toString
        |> text
