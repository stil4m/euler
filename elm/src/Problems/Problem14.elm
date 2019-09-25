module Problems.Problem14 exposing (main)

import BigInt exposing (BigInt)
import Dict exposing (Dict)
import Html exposing (..)


initial =
    Dict.fromList [ ( "1", 1 ) ]


main : Html msg
main =
    let
        initialPath =
            List.range 1 1000000
                |> List.foldl (\a b -> collatzLength a b |> Tuple.second) initial
                |> Dict.toList
    in
    initialPath
        |> List.filterMap (\( a, b ) -> String.toInt a |> Maybe.map (\x -> ( x, b )))
        |> List.filter (Tuple.first >> (\a -> a > 0 && a < 1000000))
        |> List.sortBy Tuple.second
        |> List.reverse
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0
        |> String.fromInt
        |> text


collatzLengthSlow : BigInt -> Dict String Int -> ( Int, Dict String Int )
collatzLengthSlow n d =
    let
        s =
            BigInt.toString n
    in
    case Dict.get s d of
        Just v ->
            ( v, d )

        Nothing ->
            let
                next =
                    nextCollatzValueSlow n

                ( v, newD ) =
                    collatzLengthSlow next d
            in
            ( v + 1
            , Dict.insert s (v + 1) newD
            )


collatzLength : Int -> Dict String Int -> ( Int, Dict String Int )
collatzLength n d =
    let
        s =
            String.fromInt n
    in
    case Dict.get s d of
        Just v ->
            ( v, d )

        Nothing ->
            let
                next : Int
                next =
                    nextCollatzValue n

                ( v, newD ) =
                    if next < 0 then
                        collatzLengthSlow (BigInt.fromInt n) d

                    else
                        collatzLength next d
            in
            ( v + 1
            , Dict.insert s (v + 1) newD
            )


two =
    BigInt.fromInt 2


three =
    BigInt.fromInt 3


one =
    BigInt.fromInt 1


nextCollatzValueSlow : BigInt -> BigInt
nextCollatzValueSlow n =
    if BigInt.isEven n then
        BigInt.div n two

    else
        BigInt.mul n three |> BigInt.add one


nextCollatzValue : Int -> Int
nextCollatzValue n =
    if modBy 2 n == 0 then
        n // 2

    else
        n * 3 + 1
