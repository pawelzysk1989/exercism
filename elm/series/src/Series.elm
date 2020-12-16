module Series exposing (slices)


flip : (a -> b -> c) -> b -> a -> c
flip fn a b =
    fn b a


digitSequence : Int -> List Char -> List Int
digitSequence size =
    List.map (flip (-) 48) << List.map Char.toCode << List.take size


digitSequences : Int -> List Char -> List (List Int)
digitSequences size chars =
    if List.length chars < size then
        []

    else
        digitSequence size chars :: digitSequences size (Maybe.withDefault [] (List.tail chars))


slices : Int -> String -> Result String (List (List Int))
slices size input =
    if String.length input == 0 then
        Err "series cannot be empty"

    else if size > String.length input then
        Err "slice length cannot be greater than series length"

    else if size == 0 then
        Err "slice length cannot be zero"

    else if size < 0 then
        Err "slice length cannot be negative"

    else
        Ok (digitSequences size (String.toList input))
