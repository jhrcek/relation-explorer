module RelTest exposing (..)

import Expect exposing (Expectation)
import Rel exposing (DerivedInfo, Pair, Rel)
import Set exposing (Set)
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
        , describe "permutation properties agree with their definitions on all relations of size <= 3"
            [ permutationPropertyTest "bijective function" Rel.isBijectiveFunction isBijectiveRef
            , permutationPropertyTest "derangement" Rel.isDerangement isDerangementRef
            , permutationPropertyTest "involution" Rel.isInvolution isInvolutionRef
            ]
        ]


permutationPropertyTest : String -> (DerivedInfo -> Bool) -> (Int -> Set Pair -> Bool) -> Test
permutationPropertyTest name actual reference =
    test name <|
        \_ ->
            List.concatMap allRels [ 1, 2, 3 ]
                |> List.map Rel.deriveInfo
                |> List.filter
                    (\info ->
                        actual info /= reference info.relSize (Set.union info.onDiagonalElements info.offDiagonalElements)
                    )
                |> List.map (\info -> Set.toList (Set.union info.onDiagonalElements info.offDiagonalElements))
                |> Expect.equal []


{-| All 2^(n\*n) relations on a set of size n
-}
allRels : Int -> List Rel
allRels n =
    List.foldl (\_ ( rel, acc ) -> ( Rel.next rel, rel :: acc ))
        ( Rel.empty n, [] )
        (List.range 1 (2 ^ (n * n)))
        |> Tuple.second


{-| Every x is related to exactly one y and every y to exactly one x
-}
isBijectiveRef : Int -> Set Pair -> Bool
isBijectiveRef n elems =
    let
        exactlyOnce f =
            List.all (\k -> Set.size (Set.filter (\p -> f p == k) elems) == 1) (List.range 0 (n - 1))
    in
    exactlyOnce Tuple.first && exactlyOnce Tuple.second


isDerangementRef : Int -> Set Pair -> Bool
isDerangementRef n elems =
    isBijectiveRef n elems && Set.isEmpty (Set.filter (\( x, y ) -> x == y) elems)


{-| Bijection equal to its own converse
-}
isInvolutionRef : Int -> Set Pair -> Bool
isInvolutionRef n elems =
    isBijectiveRef n elems && Set.map (\( x, y ) -> ( y, x )) elems == elems


fromElements : Int -> List Pair -> Rel
fromElements n =
    List.foldl (\( i, j ) -> Rel.toggle i j) (Rel.empty n)


transitivityStepsTest : Int -> List Pair -> List ( ( Int, Int, Int ), Int ) -> Expectation
transitivityStepsTest n elems expectedSteps =
    (Rel.deriveInfo (fromElements n elems)).missingForTransitivity
        |> Tuple.second
        |> List.map (\{ from, through, to, distance } -> ( ( from, through, to ), distance ))
        |> Expect.equal expectedSteps
