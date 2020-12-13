module Transpose exposing (transpose)


join : List Char -> List (List Char) -> List (List Char)
join aList aListList =
    case ( aList, aListList ) of
        ( [], [] ) ->
            []

        ( [], ys :: yss ) ->
            (' ' :: ys) :: join [] yss

        ( x :: xs, [] ) ->
            [ x ] :: join xs []

        ( x :: xs, ys :: yss ) ->
            (x :: ys) :: join xs yss


transpose : List String -> List String
transpose =
    List.map String.fromList << List.foldr join [] << List.map String.toList
