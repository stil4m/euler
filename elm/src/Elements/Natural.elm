module Elements.Natural exposing (divisors, isEven, multipleOf)


multipleOf : Int -> Int -> Bool
multipleOf x y =
    modBy x y == 0


isEven : Int -> Bool
isEven =
    multipleOf 2


divisors : Int -> List Int
divisors x =
    let
        end =
            floor (sqrt <| toFloat x)
    in
    List.range 1 end
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
