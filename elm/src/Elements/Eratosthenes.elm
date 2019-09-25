module Elements.Eratosthenes exposing (upTo)

import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


upTo : Int -> List Int
upTo n =
    let
        values =
            List.range 2 (n - 1)

        dict : Dict Int Bool
        dict =
            values
                |> List.map (\v -> ( v, True ))
                |> Dict.fromList

        maxN =
            floor (sqrt <| toFloat n)
    in
    sieve n maxN values dict
        |> Dict.filter (\_ v -> v)
        |> Dict.keys


sieve : Int -> Int -> List Int -> Dict Int Bool -> Dict Int Bool
sieve n maxN values b =
    let
        newValues =
            values
                |> List.Extra.dropWhile (\k -> not <| Maybe.withDefault True <| Dict.get k b)
    in
    case newValues of
        [] ->
            b

        x :: xs ->
            if x > maxN then
                b

            else
                let
                    noPrimes =
                        List.range 2 (ceiling <| toFloat n / toFloat x)
                            |> List.map ((*) x)

                    newDict =
                        List.foldl (\noPrime y -> Dict.insert noPrime False y) b noPrimes
                in
                sieve n maxN xs newDict
