module Sublist exposing (ListComparison(..), sublist)


type ListComparison
    = Equal
    | Superlist
    | Sublist
    | Unequal


contains : List a -> List a -> Bool
contains aList bList =
    case ( aList, bList ) of
        ( _, [] ) ->
            True

        ( [], _ ) ->
            False

        ( a :: aRest, b :: _ ) ->
            if a == b && bList == List.take (List.length bList) aList then
                True

            else
                contains aRest bList


sublist : List a -> List a -> ListComparison
sublist aList bList =
    if aList == bList then
        Equal

    else if contains aList bList then
        Superlist

    else if contains bList aList then
        Sublist

    else
        Unequal
