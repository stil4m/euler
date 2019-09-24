module Elements.Fibonacci exposing (while)


while : (Int -> Bool) -> List Int
while f =
    let
        whileRec xs a b =
            if f a then
                whileRec (a :: xs) b (a + b)

            else
                xs
    in
    whileRec [] 1 2
        |> List.reverse
