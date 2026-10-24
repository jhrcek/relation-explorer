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
        ]
