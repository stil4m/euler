module Problems.Problem15 exposing (main)

import Dict exposing (Dict)
import Html exposing (..)


main : Html msg
main =
    forDimension ( 20, 20 ) Dict.empty
        |> Tuple.first
        |> Debug.toString
        |> text


forDimension : ( Int, Int ) -> Dict String Int -> ( Int, Dict String Int )
forDimension x d =
    let
        k =
            key x
    in
    case Dict.get k d of
        Just v ->
            ( v, d )

        Nothing ->
            case x of
                ( 0, 0 ) ->
                    ( 1, d )

                ( 0, n ) ->
                    let
                        ( rec, newD ) =
                            forDimension ( 0, n - 1 ) d
                    in
                    ( rec, Dict.insert k rec newD )

                ( n, 0 ) ->
                    let
                        ( rec, newD ) =
                            forDimension ( n - 1, 0 ) d
                    in
                    ( rec, Dict.insert k rec newD )

                ( n, m ) ->
                    let
                        ( r1, d1 ) =
                            forDimension ( n - 1, m ) d

                        ( r2, d2 ) =
                            forDimension ( n, m - 1 ) d1
                    in
                    ( r1 + r2, Dict.insert k (r1 + r2) d2 )


key : ( Int, Int ) -> String
key ( a, b ) =
    if a < b then
        String.fromInt a ++ "x" ++ String.fromInt b

    else
        String.fromInt b ++ "x" ++ String.fromInt a
