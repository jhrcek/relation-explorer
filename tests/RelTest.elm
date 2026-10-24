module RelTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Random
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
        , describe "equivalence"
            [ test "number of equivalences on sets of size <= 3 are Bell numbers" <|
                \_ ->
                    List.map (\n -> List.length (allEquivalences n)) [ 1, 2, 3 ]
                        |> Expect.equal [ 1, 2, 5 ]
            , test "closure is the smallest equivalence containing the relation (all relations of size <= 3)" <|
                \_ ->
                    [ 1, 2, 3 ]
                        |> List.concatMap
                            (\n ->
                                let
                                    equivalences =
                                        allEquivalences n
                                in
                                List.filter
                                    (\rel ->
                                        let
                                            closure =
                                                Rel.equivalenceClosure rel

                                            isSmallest =
                                                equivalences
                                                    |> List.filter (isSubset rel)
                                                    |> List.all (isSubset closure)
                                        in
                                        not (Rel.isEquivalence (Rel.deriveInfo closure) && isSubset rel closure && isSmallest)
                                    )
                                    (allRels n)
                            )
                        |> List.map Rel.showElements
                        |> Expect.equal []
            , test "explanation lists the failed defining properties and highlights their missing pairs" <|
                \_ ->
                    let
                        explanation =
                            Rel.getExplanationData Rel.ExplainEquivalence
                                (Rel.deriveInfo (fromElements 3 [ ( 0, 0 ), ( 1, 1 ), ( 0, 1 ), ( 1, 2 ) ]))
                    in
                    Expect.equal
                        ( Set.toList explanation.redHighlight, List.take 5 explanation.lines )
                        ( [ ( 0, 2 ), ( 1, 0 ), ( 2, 1 ), ( 2, 2 ) ]
                        , [ "This relation is not an equivalence relation."
                          , "Definition: a relation R ⊆ X ⨯ X is an equivalence relation if it is reflexive, symmetric and transitive."
                          , "It is not reflexive, because this pair is missing: {(2,2)}. ✗"
                          , "It is not symmetric, because these pairs are missing: {(1,0), (2,1)}. ✗"
                          , "It is not transitive, because this pair is missing: {(0,2)}. ✗"
                          ]
                        )
            , test "explanation of an equivalence confirms all defining properties" <|
                \_ ->
                    Rel.getExplanationData Rel.ExplainEquivalence (Rel.deriveInfo (Rel.equivalenceClosure (fromElements 3 [ ( 0, 1 ) ])))
                        |> .lines
                        |> List.drop 2
                        |> Expect.equal [ "It is reflexive. ✓", "It is symmetric. ✓", "It is transitive. ✓" ]
            , test "classes form a sorted partition" <|
                \_ ->
                    fromElements 5 [ ( 3, 1 ), ( 4, 0 ) ]
                        |> Rel.equivalenceClosure
                        |> Rel.equivalenceClasses
                        |> Expect.equal [ [ 0, 4 ], [ 1, 3 ], [ 2 ] ]
            , test "generator produces equivalences, with all partitions of a 3-element set about equally likely" <|
                \_ ->
                    let
                        sampleCount =
                            5000

                        samples =
                            Random.step (Random.list sampleCount (Rel.genEquivalence 3)) (Random.initialSeed 42)
                                |> Tuple.first

                        counts =
                            List.foldl (\rel -> Dict.update (Rel.showElements rel) (Just << (+) 1 << Maybe.withDefault 0)) Dict.empty samples
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (List.all (Rel.isEquivalence << Rel.deriveInfo) samples)
                        , \_ -> Expect.equal 5 (Dict.size counts)
                        , \_ -> Expect.equal [] (List.filter (\c -> abs (c - sampleCount // 5) > 100) (Dict.values counts))
                        ]
                        ()
            ]
        ]


allEquivalences : Int -> List Rel
allEquivalences n =
    List.filter (Rel.isEquivalence << Rel.deriveInfo) (allRels n)


isSubset : Rel -> Rel -> Bool
isSubset a b =
    Set.isEmpty (Set.diff (toPairSet a) (toPairSet b))


toPairSet : Rel -> Set Pair
toPairSet rel =
    let
        info =
            Rel.deriveInfo rel
    in
    Set.union info.onDiagonalElements info.offDiagonalElements


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
