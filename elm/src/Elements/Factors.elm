module Elements.Factors exposing (factors, flatten, sum)

import Elements.Natural exposing (..)
import Elements.Primes as Primes


type alias FactorList =
    List FactorElem


type alias FactorElem =
    ( Int, Int )


addFactorElem : FactorElem -> FactorList -> FactorList
addFactorElem (( ax, acount ) as a) b =
    case b of
        [] ->
            [ a ]

        (( zx, zcount ) as z) :: xs ->
            if ax == zx then
                ( zx, max zcount acount ) :: xs

            else
                z :: addFactorElem a xs


sum : FactorList -> Int
sum x =
    case x of
        ( a, b ) :: rest ->
            a ^ b * sum rest

        [] ->
            1


flatten : List FactorList -> FactorList
flatten x =
    let
        xs =
            List.concat x

        flattenRec ys zs =
            case zs of
                z :: rest ->
                    flattenRec (addFactorElem z ys) rest

                [] ->
                    ys
    in
    flattenRec [] xs


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
