module Acronym exposing (abbreviate)


abbreviate : String -> String
abbreviate phrase =
    phrase
        |> (String.split " " << String.replace "-" " ")
        |> List.map (String.left 1)
        |> List.map String.toUpper
        |> List.foldr (++) ""
