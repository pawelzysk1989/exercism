module Wordy exposing (answer)

import Parser exposing (..)


type Expr
    = Add Int
    | Subtract Int
    | Multiply Int
    | Divide Int


toExpr : String -> Int -> Expr
toExpr opr =
    case opr of
        "plus" ->
            Add

        "minus" ->
            Subtract

        "multiplied by" ->
            Multiply

        _ ->
            Divide


exprToFunction : Expr -> (Int -> Int)
exprToFunction expr =
    \arg ->
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
    initValue
        |> List.foldr ((>>) << exprToFunction) identity expressions


myInt : Parser Int
myInt =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]


operation : Parser String
operation =
    getChompedString <|
        oneOf
            [ succeed ()
                |. keyword "plus"
            , succeed ()
                |. keyword "minus"
            , succeed ()
                |. keyword "multiplied by"
            , succeed ()
                |. keyword "divided by"
            ]


expression : Parser Expr
expression =
    succeed toExpr
        |= operation
        |. spaces
        |= myInt


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
