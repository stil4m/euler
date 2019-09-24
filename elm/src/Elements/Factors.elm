module Elements.Factors exposing (factors)

import Elements.Natural exposing (..)
import Elements.Primes as Primes


type alias FactorList =
    List ( Int, Int )


factors : Int -> FactorList
factors n =
    let
        factorsRec x stream answer =
            let
                ( next, stream2 ) =
                    Primes.next stream

                ( times, rem ) =
                    break next x

                answer2 =
                    if times > 0 then
                        ( next, times ) :: answer

                    else
                        answer
            in
            if rem == 1 then
                answer2

            else
                factorsRec rem stream2 answer2
    in
    factorsRec n Primes.primes []


break : Int -> Int -> ( Int, Int )
break a b =
    let
        breakR v c =
            if multipleOf a c then
                breakR (v + 1) (round (toFloat c / toFloat a))

            else
                ( v, c )
    in
    breakR 0 b
