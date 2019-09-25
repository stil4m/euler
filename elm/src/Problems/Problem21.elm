module Problems.Problem21 exposing (main)

import Elements.Natural as Natural
import Html exposing (..)
import Set


main : Html msg
main =
    List.range 1 10000
        |> List.filterMap amicablePair
        |> List.map (\( a, b ) -> a + b)
        |> List.sum
        |> Debug.toString
        |> text


amicablePair : Int -> Maybe ( Int, Int )
amicablePair v =
    let
        d x =
            (Natural.divisors x |> Set.fromList |> Set.remove x |> Set.toList) |> List.sum

        other =
            d v
    in
    if other > v then
        if d other == v then
            Just ( v, other )

        else
            Nothing

    else
        Nothing
