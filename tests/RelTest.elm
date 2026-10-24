module RelTest exposing (..)

import Expect exposing (Expectation)
import Rel exposing (Pair, Rel)
import Test exposing (..)


suite : Test
suite =
    describe "Rel"
        [ describe "missingForTransitivity steps"
            [ test "contain one step per missing pair, with shortest distance" <|
                \_ ->
                    -- (0,4) is first reached via longer path 0->1->2->4 (through 2),
                    -- and only later via shorter path 0->3->4 (through 3)
                    transitivityStepsTest 5
                        [ ( 0, 1 ), ( 1, 2 ), ( 2, 4 ), ( 0, 3 ), ( 3, 4 ) ]
                        [ ( ( 0, 1, 2 ), 2 )
                        , ( ( 0, 3, 4 ), 2 )
                        , ( ( 1, 2, 4 ), 2 )
                        ]
            ]
        ]


fromElements : Int -> List Pair -> Rel
fromElements n =
    List.foldl (\( i, j ) -> Rel.toggle i j) (Rel.empty n)


transitivityStepsTest : Int -> List Pair -> List ( ( Int, Int, Int ), Int ) -> Expectation
transitivityStepsTest n elems expectedSteps =
    (Rel.deriveInfo (fromElements n elems)).missingForTransitivity
        |> Tuple.second
        |> List.map (\{ from, through, to, distance } -> ( ( from, through, to ), distance ))
        |> Expect.equal expectedSteps
