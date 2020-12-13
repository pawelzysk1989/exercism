module RunLengthEncoding exposing (decode, encode)


groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile f list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                next =
                    groupWhile f xs

                ys =
                    List.head next |> Maybe.withDefault []

                yss =
                    List.tail next |> Maybe.withDefault []

                y =
                    ys |> List.head |> Maybe.withDefault x
            in
            if f y x then
                (x :: ys) :: yss

            else
                [ x ] :: ys :: yss


charListToString : List Char -> String
charListToString charList =
    case charList of
        [] ->
            ""

        char :: [] ->
            String.fromChar char

        char :: _ ->
            String.fromInt (List.length charList) ++ String.fromChar char


encode : String -> String
encode =
    String.join "" << List.map charListToString << groupWhile (==) << String.toList


replaceInts : List String -> List String
replaceInts list =
    case list of
        [] ->
            []

        string :: [] ->
            [ string ]

        string1 :: string2 :: rest ->
            case ( String.toInt string1, String.toInt string2 ) of
                ( Just int1, Just int2 ) ->
                    replaceInts ((string1 ++ string2) :: rest)

                ( Just int, Nothing ) ->
                    replaceInts (String.repeat int string2 :: rest)

                _ ->
                    string1 :: replaceInts (string2 :: rest)


decode : String -> String
decode =
    String.join "" << replaceInts << String.split ""
