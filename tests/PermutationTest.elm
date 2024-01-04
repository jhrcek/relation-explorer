module PermutationTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Permutation
import Test exposing (..)


suite : Test
suite =
    describe "Permutation"
        [ describe "parse"
            [ test "rejects empty String" <|
                \_ -> failedParseTest 2 "" "Empty input"
            , test "fails with nonsense input" <|
                \_ -> failedParseTest 2 "asdf" "Invalid cycle notation: Expecting Symbol ("
            , test "fails with ok input followed by nonsense" <|
                \_ -> failedParseTest 2 "(1 2)asdf" "Invalid cycle notation: Expecting End"
            , test "rejects out of bounds elements" <|
                \_ -> failedParseTest 2 "(1 2)" "Invalid cycle notation: out of bound element: 2"
            , test "rejects duplicate elements" <|
                \_ -> failedParseTest 2 "(1 1)" "Invalid cycle notation: duplicate element: 1"
            , test "fails with out of bounds - negative" <|
                \_ -> failedParseTest 2 "(0 0)" "Invalid cycle notation: duplicate element: 0"
            , test "accepts empty cycle as identity" <|
                \_ -> okParseTest 2 "()" (Array.fromList [ 0, 1 ])
            , test "accepts single element cycle as identity" <|
                \_ -> okParseTest 2 "(1)" (Array.fromList [ 0, 1 ])
            , test "accepts single transposition" <|
                \_ -> okParseTest 2 "(0 1)" (Array.fromList [ 1, 0 ])
            , test "accepts longer single cycle" <|
                \_ -> okParseTest 4 "(0 1 3 2)" (Array.fromList [ 1, 3, 0, 2 ])
            , test "accepts two cycles" <|
                \_ -> okParseTest 4 "(0 1)(2 3)" (Array.fromList [ 1, 0, 3, 2 ])
            ]
        ]


failedParseTest : Int -> String -> String -> Expectation
failedParseTest n input expectedErr =
    Permutation.parse n input
        |> Expect.equal (Err expectedErr)


okParseTest : Int -> String -> Array Int -> Expectation
okParseTest n input expected =
    Permutation.parse n input
        |> Result.map Permutation.toArray
        |> Expect.equal (Ok expected)
