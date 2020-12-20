module Wordy exposing (answer)

import Parser exposing (..)


type Expr
    = Add Int
    | Subtract Int
    | Multiply Int
    | Divide Int


exprToFunction : Expr -> (Int -> Int)
exprToFunction expr arg =
    case expr of
        Add int ->
            arg + int

        Subtract int ->
            arg - int

        Multiply int ->
            arg * int

        Divide int ->
            arg // int


solve : Int -> List Expr -> Int
solve initValue expressions =
    let
        composed =
            List.foldr ((>>) << exprToFunction) identity expressions
    in
    composed initValue


myInt : Parser Int
myInt =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]


expression : Parser Expr
expression =
    oneOf
        [ succeed Add
            |. keyword "plus"
            |. spaces
            |= myInt
        , succeed Subtract
            |. keyword "minus"
            |. spaces
            |= myInt
        , succeed Multiply
            |. keyword "multiplied by"
            |. spaces
            |= myInt
        , succeed Divide
            |. keyword "divided by"
            |. spaces
            |= myInt
        ]


parser : Parser Int
parser =
    succeed solve
        |. keyword "What is"
        |. spaces
        |= myInt
        |. spaces
        |= loop [] parserStep
        |. symbol "?"


parserStep : List Expr -> Parser (Step (List Expr) (List Expr))
parserStep revExprs =
    oneOf
        [ succeed (\exp -> Loop (exp :: revExprs))
            |= expression
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revExprs))
        ]


answer : String -> Maybe Int
answer input =
    case run parser input of
        Err _ ->
            Nothing

        Ok result ->
            Just result
