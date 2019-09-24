module Elements.Natural exposing (isEven, multipleOf)


multipleOf : Int -> Int -> Bool
multipleOf x y =
    modBy x y == 0


isEven : Int -> Bool
isEven =
    multipleOf 2
