module AllYourBase exposing (rebase)


toDecimalBase : Int -> List Int -> Int
toDecimalBase base digits =
    digits
        |> List.reverse
        |> List.indexedMap (\i -> \d -> d * (base ^ i))
        |> List.foldl (+) 0


fromDecimalBase : Int -> Int -> List Int
fromDecimalBase outBase value =
    if value == 0 then
        []

    else
        fromDecimalBase outBase (value // outBase) ++ [ Basics.modBy outBase value ]


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    if inBase <= 0 then
        Nothing

    else if outBase <= 0 then
        Nothing

    else if digits == [] then
        Nothing

    else if List.any ((>) 0) digits then
        Nothing

    else if List.any ((<=) inBase) digits then
        Nothing

    else if List.all ((==) 0) digits then
        Nothing

    else
        toDecimalBase inBase digits
            |> fromDecimalBase outBase
            |> Just
