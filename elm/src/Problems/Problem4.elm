module Problems.Problem4 exposing (main)

import Html exposing (..)


main : Html msg
main =
    let
        digits =
            3
    in
    maxForDigits digits
        |> List.range (maxForDigits (digits - 1))
        |> List.reverse
        |> List.filter isPalindrone
        |> List.filter (divisableByPairOfDigitValues digits)
        |> List.head
        |> Maybe.withDefault 0
        |> String.fromInt
        |> text


divisableByPairOfDigitValues : Int -> Int -> Bool
divisableByPairOfDigitValues n x =
    let
        m =
            (10 ^ n) - 1

        low =
            ceiling (sqrt (toFloat x))

        vs =
            List.range low m
    in
    vs
        |> List.any (\v -> modBy v x == 0 && x // v < m)


isPalindrone : Int -> Bool
isPalindrone x =
    let
        s =
            String.fromInt x
    in
    String.reverse s == s


maxForDigits : Int -> Int
maxForDigits n =
    let
        x =
            (10 ^ n) - 1
    in
    x * x
