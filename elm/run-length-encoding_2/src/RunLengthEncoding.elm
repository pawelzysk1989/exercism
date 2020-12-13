module RunLengthEncoding exposing (decode, encode)

import Parser exposing (..)


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


zeroOrMore : (Char -> Bool) -> Parser String
zeroOrMore isOk =
    succeed identity
        |. chompWhile isOk
        |> getChompedString


getChar : Parser Char
getChar =
    succeed ()
        |. chompIf (Basics.always True)
        |> getChompedString
        |> andThen stringHead


stringHead : String -> Parser Char
stringHead string =
    case String.uncons string of
        Just ( char, _ ) ->
            succeed char

        _ ->
            problem ""


occurenceCharPair : Parser ( Int, Char )
occurenceCharPair =
    succeed Tuple.pair
        |= oneOf
            [ int
            , succeed 1
            ]
        |= getChar


occurenceCharPairs : Parser (List ( Int, Char ))
occurenceCharPairs =
    loop [] step


step : List ( Int, Char ) -> Parser (Step (List ( Int, Char )) (List ( Int, Char )))
step pairs =
    oneOf
        [ succeed (\pair -> Loop (pair :: pairs))
            |= occurenceCharPair
        , succeed ()
            |> map (\_ -> Done (List.reverse pairs))
        ]


pairToString : ( Int, Char ) -> String
pairToString ( occurence, char ) =
    List.repeat occurence char
        |> String.fromList


decode : String -> String
decode string =
    string
        |> run occurenceCharPairs
        |> Result.map (List.map pairToString)
        |> Result.map (String.join "")
        |> Result.withDefault ""
