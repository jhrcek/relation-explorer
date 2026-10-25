module RelTest exposing (..)

import Count
import Dict
import Expect exposing (Expectation)
import Natural
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
            [ propertyTest "bijective function" Rel.isBijectiveFunction isBijectiveRef
            , propertyTest "derangement" Rel.isDerangement isDerangementRef
            , propertyTest "involution" Rel.isInvolution isInvolutionRef
            ]
        , describe "transitive and partial order generators"
            [ test "transitive generator only produces transitive relations" <|
                \_ -> generatorTest Rel.isTransitive (Rel.genTransitive 0.3)
            , test "partial order generator only produces partial orders" <|
                \_ -> generatorTest Rel.isPartialOrder (Rel.genPartialOrder 0.3)
            , test "transitive generator can produce all 13 transitive relations on a 2-element set" <|
                \_ ->
                    Random.step (Random.list 2000 (Rel.genTransitive 0.5 2)) (Random.initialSeed 42)
                        |> Tuple.first
                        |> List.map Rel.showElements
                        |> Set.fromList
                        |> Set.size
                        |> Expect.equal 13
            ]
        , describe "derangement generator"
            [ test "only produces derangements" <|
                \_ ->
                    -- There's no derangement of 1 element set, so start from 2
                    Random.step (Random.list 200 (Random.int 2 10 |> Random.andThen Rel.genDerangement)) (Random.initialSeed 42)
                        |> Tuple.first
                        |> List.filter (not << Rel.isDerangement << Rel.deriveInfo)
                        |> List.map Rel.showElements
                        |> Expect.equal []
            , test "all derangements of a 4-element set are about equally likely" <|
                \_ ->
                    let
                        sampleCount =
                            9000

                        samples =
                            Random.step (Random.list sampleCount (Rel.genDerangement 4)) (Random.initialSeed 42)
                                |> Tuple.first

                        counts =
                            List.foldl (\rel -> Dict.update (Rel.showElements rel) (Just << (+) 1 << Maybe.withDefault 0)) Dict.empty samples
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (List.all (Rel.isDerangement << Rel.deriveInfo) samples)
                        , \_ -> Expect.equal 9 (Dict.size counts)
                        , \_ -> Expect.equal [] (List.filter (\c -> abs (c - sampleCount // 9) > 120) (Dict.values counts))
                        ]
                        ()
            ]
        , describe "injective and surjective"
            [ describe "agree with their definitions on all relations of size <= 3"
                [ propertyTest "injective" Rel.isInjective isInjectiveRef
                , propertyTest "surjective" Rel.isSurjective isSurjectiveRef
                ]
            , test "number of injective relations on sets of size <= 3 matches count formula" <|
                \_ -> countTest Rel.isInjective Count.functionalOrInjective
            , test "number of surjective relations on sets of size <= 3 matches count formula" <|
                \_ -> countTest Rel.isSurjective Count.leftTotalOrSurjective
            , test "injective generator only produces injective relations" <|
                \_ -> generatorTest Rel.isInjective (Rel.genInjectiveRelation 0.7)
            , test "surjective generator only produces surjective relations" <|
                \_ -> generatorTest Rel.isSurjective (Rel.genSurjectiveRelation 0.3)
            , test "explanation of non-injective relation highlights columns with more than one element" <|
                \_ ->
                    explanationTest Rel.ExplainInjective
                        (fromElements 3 [ ( 0, 2 ), ( 1, 2 ), ( 2, 0 ) ])
                        ( [ ( 0, 2 ), ( 1, 2 ) ]
                        , [ "This relation is not injective."
                          , "Definition: a relation R ⊆ X ⨯ X is injective (also called left-unique) if ∀ x, y, z ∈ X: (x, z) ∈ R ∧ (y, z) ∈ R ⇒ x = y"
                          , "Explanation: Negating the condition from the definition above, we get the following condition satisfied by relations which are not injective: ∃ x, y, z ∈ X: (x, z) ∈ R ∧ (y, z) ∈ R ∧ x ≠ y."
                          , "There is one element y ∈ X with which more than one x ∈ X is in relation:"
                          , "2 elements are in relation with 2: {0, 1}"
                          ]
                        )
            , test "explanation of non-surjective relation highlights empty columns" <|
                \_ ->
                    explanationTest Rel.ExplainSurjective
                        (fromElements 3 [ ( 0, 2 ), ( 1, 2 ) ])
                        ( [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ), ( 2, 0 ), ( 2, 1 ) ]
                        , [ "This relation is not surjective."
                          , "Definition: a relation R ⊆ X ⨯ X is surjective (also called right-total or onto) if ∀ y ∈ X: ∃ x ∈ X: (x, y) ∈ R."
                          , "Explanation: Negating the condition from the definition above, we get the following condition satisfied by relations which are not surjective: ∃ y ∈ X: ∀ x ∈ X: (x, y) ∉ R."
                          , "There are 2 elements y ∈ X with which no x ∈ X is in relation: {0, 1}"
                          ]
                        )
            , test "bijection explanation confirms all four properties exactly for bijections (all relations of size <= 3)" <|
                \_ ->
                    List.concatMap allRels [ 1, 2, 3 ]
                        |> List.map Rel.deriveInfo
                        |> List.filter
                            (\info ->
                                let
                                    lines =
                                        (Rel.getExplanationData Rel.ExplainBijective info).lines

                                    allConfirmed =
                                        List.length (List.filter (String.endsWith "✓") lines) == 4
                                in
                                Rel.isBijectiveFunction info /= allConfirmed
                            )
                        |> List.map (\info -> Set.toList (Set.union info.onDiagonalElements info.offDiagonalElements))
                        |> Expect.equal []
            , test "explanation of non-bijection lists the violated properties and highlights the problematic pairs" <|
                \_ ->
                    -- 0 -> {1, 2}, 1 -> {2}, 2 -> {}
                    explanationTest Rel.ExplainBijective
                        (fromElements 3 [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ) ])
                        ( [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
                        , [ "This relation is not a bijection."
                          , "Definition: a relation R ⊆ X ⨯ X is a bijection if it is a function (functional and left-total) which is injective and surjective."
                          , "It is not functional, because this element is in relation with more than one y ∈ X: {0}. ✗"
                          , "It is not left-total, because this element is not in relation with any y ∈ X: {2}. ✗"
                          , "It is not injective, because more than one x ∈ X is in relation with this element: {2}. ✗"
                          , "It is not surjective, because no x ∈ X is in relation with this element: {0}. ✗"
                          , "See the explanations of the individual properties for more details."
                          , "Note: because X is finite, a function from X to X is injective if and only if it is surjective."
                          ]
                        )
            , test "functional and left-total explanations are unchanged by the refactoring" <|
                \_ ->
                    let
                        info =
                            Rel.deriveInfo (fromElements 3 [ ( 0, 1 ), ( 0, 2 ) ])
                    in
                    ( List.drop 3 (Rel.getExplanationData Rel.ExplainFunctional info).lines
                    , List.drop 3 (Rel.getExplanationData Rel.ExplainLeftTotal info).lines
                    )
                        |> Expect.equal
                            ( [ "There is one element which is in relation with more than one y ∈ X:"
                              , "0 is in relation with 2 elements: {1, 2}"
                              ]
                            , [ "There are 2 elements which are not in relation with any elements y ∈ X: {1, 2}" ]
                            )
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


countTest : (DerivedInfo -> Bool) -> (Int -> Natural.Natural) -> Expectation
countTest hasProperty countFormula =
    List.map (\n -> String.fromInt (List.length (List.filter (hasProperty << Rel.deriveInfo) (allRels n)))) [ 1, 2, 3 ]
        |> Expect.equal (List.map (Natural.toString << countFormula) [ 1, 2, 3 ])


generatorTest : (DerivedInfo -> Bool) -> (Int -> Random.Generator Rel) -> Expectation
generatorTest hasProperty gen =
    Random.step (Random.list 200 (Random.int 1 10 |> Random.andThen gen)) (Random.initialSeed 42)
        |> Tuple.first
        |> List.filter (not << hasProperty << Rel.deriveInfo)
        |> List.map Rel.showElements
        |> Expect.equal []


explanationTest : Rel.ExplainableProperty -> Rel -> ( List Pair, List String ) -> Expectation
explanationTest property rel expected =
    let
        explanation =
            Rel.getExplanationData property (Rel.deriveInfo rel)
    in
    ( Set.toList explanation.redHighlight, explanation.lines )
        |> Expect.equal expected


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


propertyTest : String -> (DerivedInfo -> Bool) -> (Int -> Set Pair -> Bool) -> Test
propertyTest name actual reference =
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


{-| No y is related to by two different xs
-}
isInjectiveRef : Int -> Set Pair -> Bool
isInjectiveRef _ elems =
    Set.toList elems
        |> List.all (\( x1, y1 ) -> Set.toList elems |> List.all (\( x2, y2 ) -> y1 /= y2 || x1 == x2))


{-| Every y is related to by some x
-}
isSurjectiveRef : Int -> Set Pair -> Bool
isSurjectiveRef n elems =
    List.all (\y -> Set.toList elems |> List.any (\( _, y2 ) -> y2 == y)) (List.range 0 (n - 1))


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
