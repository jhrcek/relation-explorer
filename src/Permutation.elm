module Permutation exposing
    ( Permutation
    , State
    , fixedPoints
    , fromListUnsafe
    , get
    , init
    , isEven
    , order
    , parse
    , showCycleType
    , showCycles
    , showOneLineNotation
    , toArray
    , update
    , updateSize
    )

import Arithmetic exposing (lcm)
import Array exposing (Array)
import Dict
import List
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Problem(..))
import Set


type Permutation
    = Permutation (Array Int)


{-| Doesn't check all elements are distinct / within bounds
-}
fromListUnsafe : List Int -> Permutation
fromListUnsafe =
    Permutation << Array.fromList


{-| For testing purposes
-}
toArray : Permutation -> Array Int
toArray (Permutation p) =
    p


type alias State =
    { rawInput : String
    , result : Result String Permutation
    }


init : Int -> State
init n =
    { rawInput = ""
    , result = parse n ""
    }


update : String -> Int -> State -> State
update rawInput n state =
    { state
        | rawInput = rawInput
        , result = parse n rawInput
    }


updateSize : Int -> State -> State
updateSize n state =
    { state
        | result = parse n state.rawInput
    }


get : Int -> Permutation -> Maybe Int
get i (Permutation p) =
    Array.get i p


parse : Int -> String -> Result String Permutation
parse n input =
    Parser.run (parser n) input
        |> Result.mapError
            (\errs ->
                if String.isEmpty input then
                    "Empty input"

                else
                    "Invalid cycle notation: "
                        ++ (case errs of
                                [] ->
                                    "no further details"

                                e :: _ ->
                                    showDeadEnd e
                           )
            )


fromCycles : Int -> List (List Int) -> Result String Permutation
fromCycles n cycs =
    let
        successivePairs : List a -> List ( a, a )
        successivePairs xs =
            case xs of
                first :: _ ->
                    let
                        go ys acc =
                            case ys of
                                [] ->
                                    acc

                                [ last ] ->
                                    ( last, first ) :: acc

                                x :: y :: rest ->
                                    go (y :: rest) (( x, y ) :: acc)
                    in
                    go xs []

                [] ->
                    []

        concatCycles =
            List.concat cycs
    in
    case findDuplicate concatCycles of
        Just dup ->
            Err <| "duplicate element: " ++ String.fromInt dup

        Nothing ->
            case List.find (\i -> i < 0 || n <= i) concatCycles of
                Just outOfBound ->
                    Err <| "element " ++ String.fromInt outOfBound ++ " out of bounds (0-" ++ String.fromInt (n - 1) ++ ")"

                Nothing ->
                    let
                        mappings =
                            Dict.fromList <| List.concatMap successivePairs cycs
                    in
                    Ok <| Permutation <| Array.initialize n <| \i -> Dict.get i mappings |> Maybe.withDefault i


toCycles : Permutation -> List (List Int)
toCycles (Permutation p) =
    let
        initRemaining =
            List.range 0 <| Array.length p - 1

        go : List Int -> List (List Int) -> List (List Int)
        go remaining cyclesAcc =
            case remaining of
                [] ->
                    List.reverse cyclesAcc

                start :: rest ->
                    let
                        newCycle =
                            unrollCycle start p

                        newRemaining =
                            List.filter (\i -> List.notMember i newCycle) rest
                    in
                    go newRemaining (newCycle :: cyclesAcc)
    in
    go initRemaining []


unrollCycle : Int -> Array Int -> List Int
unrollCycle start a =
    let
        go : Int -> List Int -> List Int
        go i acc =
            let
                next =
                    Array.get i a |> Maybe.withDefault i
            in
            if next == start then
                List.reverse acc

            else
                go next (next :: acc)
    in
    go start [ start ]


findDuplicate : List Int -> Maybe Int
findDuplicate =
    let
        go seen xs =
            case xs of
                [] ->
                    Nothing

                y :: ys ->
                    if Set.member y seen then
                        Just y

                    else
                        go (Set.insert y seen) ys
    in
    go Set.empty


parser : Int -> Parser Permutation
parser n =
    Parser.oneOf
        [ Parser.backtrackable emptyCycle
        , oneOrMoreNonEmptyCycles
        ]
        |. Parser.spaces
        |. Parser.end
        |> Parser.andThen
            (\cycs ->
                case fromCycles n cycs of
                    Ok perm ->
                        Parser.succeed perm

                    Err err ->
                        Parser.problem err
            )


oneOrMoreNonEmptyCycles : Parser (List (List Int))
oneOrMoreNonEmptyCycles =
    oneOrMoreSpaceSep nonEmptyCycle


emptyCycle : Parser (List (List Int))
emptyCycle =
    Parser.succeed []
        |. Parser.spaces
        |. Parser.symbol "("
        |. Parser.spaces
        |. Parser.symbol ")"


nonEmptyCycle : Parser (List Int)
nonEmptyCycle =
    Parser.succeed identity
        |. Parser.symbol "("
        |= oneOrMoreSpaceSep Parser.int
        |. Parser.symbol ")"


oneOrMoreSpaceSep : Parser a -> Parser (List a)
oneOrMoreSpaceSep p =
    Parser.succeed (::)
        |. Parser.spaces
        |= p
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.spaces
            , item = p
            , trailing = Parser.Forbidden
            }


showDeadEnd : Parser.DeadEnd -> String
showDeadEnd deadEnd =
    case deadEnd.problem of
        Expecting str ->
            "Expecting " ++ str

        ExpectingInt ->
            "Expecting Int"

        ExpectingHex ->
            "Expecting Hex"

        ExpectingOctal ->
            "Expecting Octal"

        ExpectingBinary ->
            "Expecting Binary"

        ExpectingFloat ->
            "Expecting Float"

        ExpectingNumber ->
            "Expecting Number"

        ExpectingVariable ->
            "Expecting Variable"

        ExpectingSymbol str ->
            "Expecting Symbol " ++ str

        ExpectingKeyword str ->
            "Expecting Keyword " ++ str

        ExpectingEnd ->
            "Expecting End"

        UnexpectedChar ->
            "Unexpected Char"

        Problem str ->
            str

        BadRepeat ->
            "Bad Repeat"


showOneLineNotation : Permutation -> String
showOneLineNotation (Permutation p) =
    Array.toList p
        |> List.map String.fromInt
        |> String.join " "


showCycles : Permutation -> String
showCycles p =
    case toCycles p of
        [] ->
            "()"

        cycles ->
            List.filter (\c -> List.length c > 1) cycles
                |> List.map (\c -> "(" ++ String.join " " (List.map String.fromInt c) ++ ")")
                |> String.concat


showCycleType : Permutation -> String
showCycleType p =
    cycleType p
        |> List.map String.fromInt
        |> String.join ","
        |> (\s -> "(" ++ s ++ ")")


fixedPoints : Permutation -> List Int
fixedPoints (Permutation p) =
    Array.toIndexedList p
        |> List.filterMap
            (\( i, j ) ->
                if i == j then
                    Just i

                else
                    Nothing
            )


order : Permutation -> Int
order p =
    toCycles p
        |> List.map List.length
        |> List.foldl lcm 1


cycleType : Permutation -> List Int
cycleType p =
    toCycles p
        |> List.map List.length
        |> List.sort
        |> List.reverse


isEven : Permutation -> Bool
isEven p =
    cycleType p
        |> List.map (\cycleLength -> cycleLength + 1)
        |> List.sum
        |> modBy 2
        |> (==) 0
