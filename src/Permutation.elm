module Permutation exposing
    ( Permutation
    , State
    , get
    , init
    , parse
    , toArray
    , update
    , updateSize
    )

import Array exposing (Array)
import Dict
import List
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Problem(..))
import Set


type Permutation
    = Permutation (Array Int)


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


validatePermutation : Int -> List (List Int) -> Result String Permutation
validatePermutation n cycs =
    let
        successivePairs xs =
            case xs of
                y :: _ ->
                    spHelp y xs []

                [] ->
                    []

        spHelp first xs acc =
            case xs of
                [] ->
                    acc

                [ last ] ->
                    ( last, first ) :: acc

                x :: y :: rest ->
                    spHelp first (y :: rest) (( x, y ) :: acc)
    in
    if List.length cycs == 0 then
        Err "empty input"

    else
        let
            concatCycles =
                List.concat cycs
        in
        case findDuplicate concatCycles of
            Just dup ->
                Err <| "duplicate element: " ++ String.fromInt dup

            Nothing ->
                case List.find (\i -> i < 0 || n <= i) concatCycles of
                    Just outOfBound ->
                        Err <| "out of bound element: " ++ String.fromInt outOfBound

                    Nothing ->
                        let
                            mappings =
                                Dict.fromList <| List.concatMap successivePairs cycs
                        in
                        Ok <| Permutation <| Array.initialize n <| \i -> Dict.get i mappings |> Maybe.withDefault i


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
    Parser.succeed identity
        |= cycles
        |. Parser.end
        |> Parser.andThen
            (\cycs ->
                case validatePermutation n cycs of
                    Ok perm ->
                        Parser.succeed perm

                    Err err ->
                        Parser.problem err
            )


cycles : Parser (List (List Int))
cycles =
    -- Hacky way to enforce parsing 1+ cycles
    Parser.succeed (::)
        |= cycle
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.spaces
            , item = cycle
            , trailing = Parser.Forbidden
            }


cycle : Parser (List Int)
cycle =
    Parser.succeed identity
        |. Parser.symbol "("
        |= cycleElements
        |. Parser.symbol ")"


cycleElements : Parser (List Int)
cycleElements =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item = Parser.int
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
