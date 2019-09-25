module Problems.Problem17 exposing (main)

import Html exposing (..)


main : Html msg
main =
    List.range 1 1000
        |> List.map asWords
        |> List.map (String.join "" >> String.length)
        |> List.sum
        |> Debug.toString
        |> text


asWords : Int -> List String
asWords n =
    if Debug.log "n" n == 0 then
        []

    else if n >= 1000 then
        asWords (n // 1000) ++ [ "thousand" ] ++ asWords (modBy 1000 n)

    else if n >= 100 then
        let
            rem =
                modBy 100 n
        in
        if rem == 0 then
            asWords (n // 100) ++ [ "hundred" ]

        else
            asWords (n // 100) ++ [ "hundred", "and" ] ++ asWords rem

    else if n < 20 then
        [ case n of
            1 ->
                "one"

            2 ->
                "two"

            3 ->
                "three"

            4 ->
                "four"

            5 ->
                "five"

            6 ->
                "six"

            7 ->
                "seven"

            8 ->
                "eight"

            9 ->
                "nine"

            10 ->
                "ten"

            11 ->
                "eleven"

            12 ->
                "twelve"

            13 ->
                "thirteen"

            14 ->
                "fourteen"

            15 ->
                "fifteen"

            16 ->
                "sixteen"

            17 ->
                "seventeen"

            18 ->
                "eighteen"

            19 ->
                "nineteen"

            x ->
                "TODO-" ++ String.fromInt x
        ]

    else if n >= 90 then
        "ninety" :: asWords (n - 90)

    else if n >= 80 then
        "eighty" :: asWords (n - 80)

    else if n >= 70 then
        "seventy" :: asWords (n - 70)

    else if n >= 60 then
        "sixty" :: asWords (n - 60)

    else if n >= 50 then
        "fifty" :: asWords (n - 50)

    else if n >= 40 then
        "forty" :: asWords (n - 40)

    else if n >= 30 then
        "thirty" :: asWords (n - 30)

    else
        "twenty" :: asWords (n - 20)
