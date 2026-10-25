module CountTest exposing (suite)

import Count
import Expect
import Natural
import Test exposing (..)


suite : Test
suite =
    describe "Count"
        [ test "equivalence gives Bell numbers (A000110)" <|
            \_ ->
                List.map (Count.equivalence >> Natural.toString) (List.range 0 10)
                    |> Expect.equal [ "1", "1", "2", "5", "15", "52", "203", "877", "4140", "21147", "115975" ]
        , test "derangement gives rencontres numbers (A000166)" <|
            \_ ->
                List.map (Count.derangement >> Natural.toString) (List.range 0 10)
                    |> Expect.equal [ "1", "0", "1", "2", "9", "44", "265", "1854", "14833", "133496", "1334961" ]
        ]
