module Problems.Problem9 exposing (main)

import Html exposing (..)


main : Html msg
main =
    triplets 1000
        |> List.filter isPythagoreanTriplet
        |> List.head
        |> Maybe.map (\( a, b, c ) -> a * b * c)
        |> Maybe.withDefault 0
        |> Debug.toString
        |> text


isPythagoreanTriplet : ( Int, Int, Int ) -> Bool
isPythagoreanTriplet ( a, b, c ) =
    let
        x =
            a ^ 2 + b ^ 2
    in
    c ^ 2 == x


triplets : Int -> List ( Int, Int, Int )
triplets n =
    let
        cMax =
            floor (toFloat n / 2)

        tripletsRec a =
            let
                rem =
                    n - a

                bMax =
                    floor (toFloat rem / 2)

                bRange =
                    List.range a bMax
            in
            List.map (\b -> ( a, b, n - a - b )) bRange
    in
    List.range 1 cMax
        |> List.concatMap tripletsRec
