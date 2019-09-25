module Problems.Problem12 exposing (main)

import Elements.Factors as Factors
import Elements.Natural as Natural
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
            List.length (Natural.divisors t)
    in
    if count > divs then
        t

    else
        findFirstTriangularWithOverNDivisors divs (n + 1)


triangleNumber : Int -> Int
triangleNumber x =
    List.range 1 x |> List.sum
