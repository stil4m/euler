module Problems.Problem12 exposing (main)

import Elements.Factors as Factors
import Html exposing (..)


main : Html msg
main =
    findFirstTriangularWithOverNDivisors 500 1
        |> Debug.toString
        |> text


findFirstTriangularWithOverNDivisors : Int -> Int -> Int
findFirstTriangularWithOverNDivisors divs n =
    let
        t =
            triangleNumber n

        count =
            List.length (divisors t)
    in
    if count > divs then
        t

    else
        findFirstTriangularWithOverNDivisors divs (n + 1)


triangleNumber : Int -> Int
triangleNumber x =
    List.range 1 x |> List.sum


divisors : Int -> List Int
divisors x =
    let
        end =
            floor (sqrt <| toFloat x)
                |> Debug.log "end"
    in
    List.range 1 end
        |> Debug.log "x"
        |> List.concatMap
            (\v ->
                if v * v == x then
                    [ v ]

                else if modBy v x == 0 then
                    [ x // v, v ]

                else
                    []
            )
        |> List.sort



--     Factors.factors
--     28
-- |> Debug.toString
-- |> text
-- findDivisors : Int -> Int -> Int
-- findDivisors count n =
-- Factors.factors n
