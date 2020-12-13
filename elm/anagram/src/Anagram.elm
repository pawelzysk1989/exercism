module Anagram exposing (detect)


isAnagram : String -> String -> Bool
isAnagram first second =
    let
        firstLower =
            String.toLower first

        secondLower =
            String.toLower second

        sortChars =
            List.sort << String.toList

        firstSorted =
            sortChars firstLower

        secondSorted =
            sortChars secondLower
    in
    firstLower /= secondLower && firstSorted == secondSorted


detect : String -> List String -> List String
detect word =
    List.filter (isAnagram word)
