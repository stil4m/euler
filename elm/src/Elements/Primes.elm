module Elements.Primes exposing (next, primes, take, takeWhile)

import Elements.Natural exposing (multipleOf)
import List.Extra


type Stream
    = Two
    | Three
    | S (List Int) Int


primes : Stream
primes =
    Two


takeWhile : (Int -> Bool) -> Stream -> List Int
takeWhile f s =
    let
        takeRec ys t =
            let
                ( y, s2 ) =
                    next t
            in
            if f y then
                takeRec (y :: ys) s2

            else
                ys
    in
    takeRec [] s
        |> List.reverse


take : Int -> Stream -> List Int
take n s =
    let
        takeRec x ys t =
            if x > 0 then
                let
                    ( y, s2 ) =
                        next t
                in
                takeRec (x - 1) (y :: ys) s2

            else
                ys
    in
    takeRec n [] s
        |> List.reverse


next : Stream -> ( Int, Stream )
next s =
    case s of
        S known x ->
            sieve known x

        Three ->
            ( 3, S [ 3, 2 ] 5 )

        Two ->
            ( 2, Three )


sieve : List Int -> Int -> ( Int, Stream )
sieve known x =
    if isPrime known x then
        ( x, S (known ++ [ x ]) (x + 2) )

    else
        sieve known (x + 2)


isPrime : List Int -> Int -> Bool
isPrime xs n =
    let
        upper =
            Basics.sqrt (toFloat n)
                |> floor

        isPrimeRec ys =
            case ys of
                [] ->
                    True

                y :: rest ->
                    if y > upper then
                        True

                    else if multipleOf y n then
                        False

                    else
                        isPrimeRec rest
    in
    isPrimeRec xs
