module MatchingBrackets exposing (isPaired)


matchingBrackets =
    [ ( '(', ')' ), ( '{', '}' ), ( '[', ']' ) ]


openings =
    List.map Tuple.first matchingBrackets


closings =
    List.map Tuple.second matchingBrackets


find : (comparable -> Bool) -> List comparable -> Maybe comparable
find fn =
    List.head << List.filter fn


isClosingMatchingOpening : Char -> Char -> Bool
isClosingMatchingOpening opening closing =
    find ((==) opening << Tuple.first) matchingBrackets
        |> Maybe.map ((==) closing << Tuple.second)
        |> Maybe.withDefault False


parse : List Char -> List Char -> Bool
parse stack chars =
    case ( stack, chars ) of
        ( [], [] ) ->
            True

        ( _, [] ) ->
            False

        ( [], char :: restChars ) ->
            if List.member char openings then
                parse (char :: []) restChars

            else if List.member char closings then
                False

            else
                parse stack restChars

        ( stackTop :: stackRest, char :: restChars ) ->
            if List.member char openings then
                parse (char :: stack) restChars

            else if List.member char closings then
                isClosingMatchingOpening stackTop char && parse stackRest restChars

            else
                parse stack restChars


isPaired : String -> Bool
isPaired =
    parse [] << String.toList
