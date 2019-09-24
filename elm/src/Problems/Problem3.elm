module Problems.Problem3 exposing (main)

import Elements.Factors as Factors
import Elements.Primes as Primes
import Html exposing (..)


main : Html msg
main =
    -- div []
    --     [ text <| Debug.toString 600851475143
    --     , text <| Debug.toString 600851475143
    --     ]
    Factors.factors 600851475143
        -- Primes.primes
        -- |> Primes.takeWhile (\v -> v < 10000)
        -- |> List.sum
        -- |> Factors.sum
        |> Debug.toString
        |> text
