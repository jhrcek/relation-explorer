module Rel exposing
    ( Acyclic
    , Config
    , DerivedInfo
    , ExplainableProperty(..)
    , ExplanationData
    , Highlight(..)
    , JoinOrMeetResult(..)
    , Pair
    , Rel
    , applyPermutation
    , cartesianProduct
    , complement
    , conceptLatticeToDotSource
    , converse
    , deriveInfo
    , domain
    , empty
    , errorInBoxDot
    , genAcyclic
    , genAntisymmetricRelation
    , genAsymmetricRelation
    , genBijectiveFunction
    , genConnectedRelation
    , genFunctionalRelation
    , genInvolution
    , genIrreflexiveRelation
    , genLeftTotal
    , genReflexiveRelation
    , genRelation
    , genSymmetricRelation
    , genTotalOrder
    , getExplanationData
    , hasseDiagramToDotSource
    , isAcyclic
    , isAntisymmetric
    , isAsymmetric
    , isConnected
    , isFunctional
    , isIrreflexive
    , isLattice
    , isLeftTotal
    , isPartialOrder
    , isPosetLattice
    , isReflexive
    , isSymmetric
    , isTotalOrder
    , isTransitive
    , next
    , objectsSharingAllAttributes
    , prev
    , reflexiveClosure
    , reflexiveReduction
    , relationBipartiteGraphDotSource
    , relationGraphToDotSource
    , resize
    , scc
    , showElements
    , showIntListAsList
    , showIntListAsSet
    , showIntSet
    , size
    , symmetricClosure
    , toRelIndex
    , toggle
    , topologicalSort
    , transitiveClosure
    , view
    , viewJoinInfo
    , viewMeetInfo
    )

import Array exposing (Array)
import Array.Extra as Array
import BitSet exposing (BitSet)
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import List
import List.Extra as List
import Natural exposing (Natural)
import Permutation exposing (Permutation)
import Random exposing (Generator)
import Random.Array
import Random.Extra as Random
import Random.List
import Set exposing (Set)


{-| A homogenous relation
-}
type Rel
    = Rel (Array (Array Bool))


{-| This wrapper is intended to sort as a proof that given Rel is acyclic.

Being acyclic makes the following operations possible:

  - topological sorting
  - transitive reduction (being unique)

-}
type Acyclic
    = Acyclic Rel


type Poset
    = Poset Rel


{-| An element of a relation.
For relation on set of size n, the are n^2 possible elements:
{(a,b) | a in {0..n-1}, b in {0..n-1}}
-}
type alias Pair =
    ( Int, Int )


type alias ExplanationData =
    { greenHighlight : Set Pair
    , redHighlight : Set Pair
    , lines : List String
    }


type Highlight
    = NoHighlight
    | Explanation ExplainableProperty ExplanationData
    | Pairs (Set Pair)


type ExplainableProperty
    = ExplainRelation
    | ExplainReflexive
    | ExplainIrreflexive
    | ExplainSymmetric
    | ExplainAntisymmetric
    | ExplainAsymmetric
    | ExplainTransitive
    | ExplainAcyclic
    | ExplainConnected
    | ExplainFunctional
    | ExplainLeftTotal


getExplanationData : ExplainableProperty -> DerivedInfo -> ExplanationData
getExplanationData property =
    case property of
        ExplainRelation ->
            explainRelation

        ExplainReflexive ->
            explainReflexive

        ExplainIrreflexive ->
            explainIrreflexive

        ExplainSymmetric ->
            explainSymmetric

        ExplainAntisymmetric ->
            explainAntisymmetric

        ExplainAsymmetric ->
            explainAsymmetric

        ExplainTransitive ->
            explainTransitive

        ExplainAcyclic ->
            explainAcyclic

        ExplainFunctional ->
            explainFunctional

        ExplainConnected ->
            explainConnected

        ExplainLeftTotal ->
            explainLeftTotal


type alias DerivedInfo =
    { relSize : Int
    , elementCount : Int
    , domain : List Int
    , offDiagonalElements : Set Pair
    , onDiagonalElements : Set Pair
    , missingForReflexivity : Set Pair
    , superfluousForIrreflexivity : Set Pair
    , missingForSymmetry : Set Pair
    , superfuousForAntisymmetry : Set Pair
    , superfluousForAsymmetry : ( Set Pair, Set Pair )
    , missingForTransitivity : ( Set Pair, List TransitiveClosureStep )
    , superfluousForFunctional : Set Pair
    , superfluousForFunction : Set Pair
    , missingForConnectedness : Set Pair
    , formalConcepts : List ( Set Int, Set Int )
    , emptyRowIndices : Set Int
    , isBijectiveFunction : Bool
    , isDerangement : Bool
    , isInvolution : Bool
    , -- Either cycle or proof that it's acyclic
      acyclicInfo : Result (List Int) AcyclicInfo
    , permutationInfo : Maybe Permutation
    }


type alias AcyclicInfo =
    { acyclic : Acyclic
    , -- Edges that would be removed by transitive reduction
      redundantTransitiveEdges : Set Pair
    , transitivelyReduced : Rel
    , -- Forms the basis for drawing hasse diagram.
      -- It's transitive and irreflexive reduction of the relation.
      -- https://en.wikipedia.org/wiki/Covering_relation
      coveringRelation : Rel
    , -- This is here, becaues only acyclic relations have a chance of being a poset
      posetInfo : Maybe PosetInfo
    }


type alias PosetInfo =
    { poset : Poset
    , minimalElements : Set Int
    , maximalElements : Set Int
    , joinTable : Result JoinOrMeetErrors (Array (Array JoinOrMeetResult))
    , meetTable : Result JoinOrMeetErrors (Array (Array JoinOrMeetResult))
    }


deriveInfo : Rel -> DerivedInfo
deriveInfo rel =
    let
        ( onDiagonalElements, offDiagonalElements ) =
            onAndOffDiagnoalElements rel

        ( superfluousForFunction, emptyRowIndices ) =
            superfluousAndMissingForFunction rel

        missingForReflexivityRes =
            missingForReflexivity rel

        superfuousForAntisymmetryRes =
            superfluousForAntisymmetry rel

        missingForTransitivityRes =
            missingForTransitivity rel
    in
    { relSize = size rel
    , elementCount = List.length <| elements rel
    , domain = domain rel
    , onDiagonalElements = onDiagonalElements
    , offDiagonalElements = offDiagonalElements
    , missingForReflexivity = missingForReflexivityRes
    , superfluousForIrreflexivity = superfluousForIrreflexivity rel
    , missingForSymmetry = missingForSymmetry rel
    , superfuousForAntisymmetry = superfuousForAntisymmetryRes
    , superfluousForAsymmetry = superfluousForAsymmetry rel
    , missingForTransitivity = missingForTransitivityRes
    , superfluousForFunctional = superfluousForFunctional rel
    , missingForConnectedness = missingForConnectedness rel
    , superfluousForFunction = superfluousForFunction
    , emptyRowIndices = emptyRowIndices
    , isBijectiveFunction = isBijectiveFunction rel
    , isDerangement = isDerangement rel
    , isInvolution = isInvolution rel
    , permutationInfo = mkPermutation rel
    , formalConcepts =
        listAttributeClosures rel
            |> List.map
                (\attrClosure ->
                    ( objectsSharingAllAttributes rel attrClosure
                    , attrClosure
                    )
                )
    , acyclicInfo =
        mkAcyclic rel
            |> Result.map
                (\acyclic ->
                    let
                        tred =
                            transitiveReduction acyclic

                        covering =
                            reflexiveReduction tred
                    in
                    { acyclic = acyclic
                    , transitivelyReduced = tred
                    , coveringRelation = covering
                    , redundantTransitiveEdges =
                        difference rel tred
                            |> elements
                            |> Set.fromList
                    , posetInfo =
                        if
                            Set.isEmpty missingForReflexivityRes
                                && Set.isEmpty superfuousForAntisymmetryRes
                                && Set.isEmpty (Tuple.first missingForTransitivityRes)
                        then
                            let
                                poset =
                                    Poset rel

                                ( minimalElements, maximalElements ) =
                                    minMaxElements covering
                            in
                            Just
                                { poset = poset
                                , minimalElements = minimalElements
                                , maximalElements = maximalElements
                                , joinTable = joinAttempt poset
                                , meetTable = meetAttempt poset
                                }

                        else
                            Nothing
                    }
                )
    }


type alias Config msg =
    { toggle : Int -> Int -> msg
    }


size : Rel -> Int
size (Rel rows) =
    Array.length rows


empty : Int -> Rel
empty n =
    Rel <| Array.repeat n <| Array.repeat n False


fromElements : Int -> List Pair -> Rel
fromElements n elems =
    List.foldl (\( i, j ) -> toggle i j) (empty n) elems


cartesianProduct : Set Int -> Set Int -> Set Pair
cartesianProduct xs ys =
    List.lift2 Tuple.pair (Set.toList xs) (Set.toList ys)
        |> Set.fromList


{-| Identity relation
-}
eye : Int -> Rel
eye n =
    Rel <| Array.initialize n (\i -> Array.initialize n (\j -> i == j))


onAndOffDiagnoalElements : Rel -> ( Set Pair, Set Pair )
onAndOffDiagnoalElements =
    elements
        >> Set.fromList
        >> Set.partition (\( i, j ) -> i == j)


{-| Change the size, while preserving as much as possible from the original.
-}
resize : Int -> Rel -> Rel
resize n (Rel rows) =
    Rel <|
        Array.initialize n
            (\i ->
                Array.initialize n
                    (\j ->
                        unsafeGet i j rows
                    )
            )


{-| List of pairs that are members of this relation
-}
elements : Rel -> List Pair
elements (Rel rows) =
    Array.toList rows
        |> List.indexedMap
            (\i row ->
                Array.toList row
                    |> List.indexedMap
                        (\j cell ->
                            if cell then
                                Just ( i, j )

                            else
                                Nothing
                        )
            )
        |> List.concat
        |> List.filterMap identity


domain : Rel -> List Int
domain (Rel rows) =
    List.range 0 (Array.length rows - 1)


{-| The relation that occurs when the order of the elements is switched in the relation.
-}
converse : Rel -> Rel
converse (Rel rows) =
    let
        n =
            Array.length rows
    in
    Rel <|
        Array.initialize n
            (\i ->
                Array.initialize n
                    (\j -> unsafeGet j i rows)
            )


{-| Complement consists of all the pairs that are not in the input relation
-}
complement : Rel -> Rel
complement (Rel rows) =
    Rel <| Array.map (Array.map not) rows


{-| Union consists of all the pairs that are in first or second relation
-}
union : Rel -> Rel -> Rel
union =
    pointwise (||)


{-| Intersection consists of all the pairs that are in both first and second relation
-}
intersection : Rel -> Rel -> Rel
intersection =
    pointwise (&&)


{-| Difference consists of all the pairs that are in the first, but not in the 2nd relation
-}
difference : Rel -> Rel -> Rel
difference =
    pointwise (\a b -> a && not b)


{-| Compose 2 Rels with each other. Assumes both have the same size.
-}
compose : Rel -> Rel -> Rel
compose (Rel rows) rel2 =
    let
        (Rel cols) =
            converse rel2

        n =
            Array.length rows
    in
    Rel <|
        Array.initialize n
            (\i ->
                Array.initialize n
                    (\j ->
                        Maybe.map2
                            (\rowi colj ->
                                -- composition of relations corresponds to matrix multiplication in boolean semiring
                                -- (+ corresponds to OR, * corresponds to AND)
                                arrayOr <| Array.map2 (&&) rowi colj
                            )
                            (Array.get i rows)
                            (Array.get j cols)
                            |> Maybe.withDefault False
                    )
            )


toggle : Int -> Int -> Rel -> Rel
toggle i j ((Rel rows) as rel) =
    case Array.get i rows of
        Just row ->
            case Array.get j row of
                Just bool ->
                    Rel <| Array.set i (Array.set j (not bool) row) rows

                Nothing ->
                    rel

        Nothing ->
            rel


prev : Rel -> Rel
prev (Rel rows) =
    -- Traverse the rows from 0 to n-1, keep flipping all the Falses to True
    -- until first True is found and flipped, then keep the rest.
    -- It's like subtracting 1 in binary, but from left to right
    Array.foldl
        (\row ( acc, flipped ) ->
            if flipped then
                ( row :: acc, flipped )

            else
                let
                    ( newRow, newFlipped ) =
                        Array.foldl
                            (\bool ( rowAcc, flipped1 ) ->
                                if flipped1 then
                                    ( bool :: rowAcc, flipped1 )

                                else if bool then
                                    ( False :: rowAcc, True )

                                else
                                    ( True :: rowAcc, flipped1 )
                            )
                            ( [], flipped )
                            row
                in
                ( Array.fromList (List.reverse newRow) :: acc
                , newFlipped
                )
        )
        ( []
        , False
        )
        rows
        |> Tuple.first
        |> List.reverse
        |> Array.fromList
        |> Rel


{-| Is characteristic vector of the relation (considered as a set) converted to a natural number
-}
toRelIndex : Rel -> Natural
toRelIndex (Rel rows) =
    Array.toList rows
        |> List.concatMap Array.toList
        |> List.map
            (\bool ->
                if bool then
                    '1'

                else
                    '0'
            )
        |> List.reverse
        |> String.fromList
        |> Natural.fromBinaryString
        |> Maybe.withDefault Natural.zero
        -- Empty set is "relation 1"
        |> Natural.add Natural.one


next : Rel -> Rel
next (Rel rows) =
    -- Traverse the rows from 0 to n-1, keep flipping all the Trues to False
    -- until first False is found and flipped, then keep the rest.
    -- It's like adding 1 in binary, but from left to right
    Array.foldl
        (\row ( acc, flipped ) ->
            if flipped then
                ( row :: acc, flipped )

            else
                let
                    ( newRow, newFlipped ) =
                        Array.foldl
                            (\bool ( rowAcc, flipped1 ) ->
                                if flipped1 then
                                    ( bool :: rowAcc, flipped1 )

                                else if bool then
                                    ( False :: rowAcc, flipped1 )

                                else
                                    ( True :: rowAcc, True )
                            )
                            ( [], flipped )
                            row
                in
                ( Array.fromList (List.reverse newRow) :: acc
                , newFlipped
                )
        )
        ( []
        , False
        )
        rows
        |> Tuple.first
        |> List.reverse
        |> Array.fromList
        |> Rel


explainRelation : DerivedInfo -> ExplanationData
explainRelation info =
    let
        definition =
            "Definition: a relation R is a subset of cartesian product X ⨯ X."
    in
    { greenHighlight = Set.union info.offDiagonalElements info.onDiagonalElements
    , redHighlight = Set.empty
    , lines =
        [ "This is a relation on X."
        , definition
        , "There are no special properties that relation must satisfy. It's just a set of pairs of elements from X ☺."
        ]
    }


{-| ∀ x ∈ X: (x, x) ∈ R
-}
isReflexive : DerivedInfo -> Bool
isReflexive info =
    Set.isEmpty info.missingForReflexivity


{-| Set of Pairs missing for the relation to be reflexive
-}
missingForReflexivity : Rel -> Set Pair
missingForReflexivity (Rel rows) =
    indexedFoldr
        (\i row ->
            if Array.get i row |> Maybe.withDefault False then
                identity

            else
                Set.insert ( i, i )
        )
        Set.empty
        rows


explainReflexive : DerivedInfo -> ExplanationData
explainReflexive info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is reflexive if ∀ x ∈ X: (x, x) ∈ R."

        diagonalPairs =
            Set.fromList <| List.map (\i -> ( i, i )) info.domain
    in
    if Set.isEmpty info.missingForReflexivity then
        { greenHighlight = diagonalPairs
        , redHighlight = Set.empty
        , lines =
            [ "This relation is reflexive."
            , definition
            , "Explanation: this relation contains all elements of the form (x, x), so it is reflexive."
            ]
        }

    else
        { greenHighlight = Set.diff diagonalPairs info.missingForReflexivity
        , redHighlight = info.missingForReflexivity
        , lines =
            [ "This relation is not reflexive."
            , definition
            , explanationPrefix "reflexive: ∃ x ∈ X: (x, x) ∉ R."
            , let
                missingCount =
                    Set.size info.missingForReflexivity

                ( isAre, elements_ ) =
                    isAreElements missingCount
              in
              "There "
                ++ isAre
                ++ " "
                ++ elements_
                ++ " of the form (x, x) which "
                ++ isAre
                ++ " not in the relation: "
                ++ showPairSet info.missingForReflexivity
            , "By adding these missing pairs, we get a Reflexive Closure of this relation, "
                ++ "which is the smallest (with respect to ⊆) reflexive relation on X that is the superset of R."
            ]
        }


explanationPrefix : String -> String
explanationPrefix negatedDefinition =
    "Explanation: Negating the condition from the definition above, "
        ++ "we get the following condition satisfied by relations which "
        ++ "are not "
        ++ negatedDefinition


{-| ∀ x ∈ X: (x, x) ∉ R
-}
isIrreflexive : DerivedInfo -> Bool
isIrreflexive info =
    Set.isEmpty info.superfluousForIrreflexivity


{-| Set of pairs that would have to be removed for the relation to be irreflexive
-}
superfluousForIrreflexivity : Rel -> Set Pair
superfluousForIrreflexivity (Rel rows) =
    indexedFoldr
        (\i row ->
            if Array.get i row |> Maybe.withDefault False then
                Set.insert ( i, i )

            else
                identity
        )
        Set.empty
        rows


explainIrreflexive : DerivedInfo -> ExplanationData
explainIrreflexive info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is irreflexive if ∀ x ∈ X: (x, x) ∉ R."

        diagonalPairs : Set Pair
        diagonalPairs =
            Set.fromList <| List.map (\i -> ( i, i )) info.domain
    in
    if Set.isEmpty info.superfluousForIrreflexivity then
        { greenHighlight = diagonalPairs
        , redHighlight = Set.empty
        , lines =
            [ "This relation is irreflexive."
            , definition
            , "Explanation: this relation has no elements of the form (x, x), so it is irreflexive."
            ]
        }

    else
        { greenHighlight = Set.diff diagonalPairs info.superfluousForIrreflexivity
        , redHighlight = info.superfluousForIrreflexivity
        , lines =
            [ "This relation is not irreflexive."
            , definition
            , explanationPrefix "irreflexive: ∃ x ∈ X: (x, x) ∈ R."
            , let
                extraneousCount =
                    Set.size info.superfluousForIrreflexivity

                ( isAre, elements_ ) =
                    isAreElements extraneousCount
              in
              "There "
                ++ isAre
                ++ " "
                ++ elements_
                ++ " of the form (x, x) which "
                ++ isAre
                ++ " in the relation: "
                ++ showPairSet info.superfluousForIrreflexivity
            , "By removing these extraneous pairs, we get a Reflexive Reduction (also called Irreflexive Kernel) of this relation,"
                ++ " which is the smallest (with respect to ⊆) relation on X that has the same reflexive closure as R."
            ]
        }


{-| ∀ x, y ∈ X: (x, y) ∈ R ⇒ (y, x) ∈ R
-}
isSymmetric : DerivedInfo -> Bool
isSymmetric info =
    Set.isEmpty info.missingForSymmetry


{-| Set of pairs that would have to be added for the relation to be symmetric
-}
missingForSymmetry : Rel -> Set Pair
missingForSymmetry rel =
    difference (pointwise xor rel (converse rel)) rel
        |> elements
        |> Set.fromList


explainSymmetric : DerivedInfo -> ExplanationData
explainSymmetric info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is symmetric if ∀ x, y ∈ X: (x, y) ∈ R ⇒ (y, x) ∈ R."
    in
    if Set.isEmpty info.missingForSymmetry then
        { greenHighlight = info.offDiagonalElements
        , redHighlight = Set.empty
        , lines =
            [ "This relation is symmetric."
            , definition
            , "Explanation: whenever there is an off-diagonal element (x, y), "
                ++ "it's \"mirror image\" (y, x) is also present, so the relation is symmetric."
            ]
        }

    else
        { greenHighlight = Set.map (\( x, y ) -> ( y, x )) info.missingForSymmetry
        , redHighlight = info.missingForSymmetry
        , lines =
            [ "This relation is not symmetric."
            , definition
            , explanationPrefix "symmetric: ∃ x, y ∈ X: (x, y) ∈ R ∧ (y, x) ∉ R."
            , let
                missingCount =
                    Set.size info.missingForSymmetry

                ( isAre, elements_ ) =
                    isAreElements missingCount
              in
              "There "
                ++ isAre
                ++ " "
                ++ elements_
                ++ " of the form (x, y) (highlighted in green), for which the corresponding "
                ++ "\"mirror image\" (y, x) (highlighted in red) "
                ++ isAre
                ++ " missing:"
            ]
                ++ List.map
                    (\( x, y ) ->
                        showPair ( y, x )
                            ++ " is present, but "
                            ++ showPair ( x, y )
                            ++ " is missing."
                    )
                    (Set.toList info.missingForSymmetry)
        }


{-| ∀ x, y ∈ X: (x, y) ∈ R ∧ (y, x) ∈ R ⇒ x = y
Or equivalently: ∀ x, y ∈ X: x ≠ y ∧ (x, y) ∈ R ⇒ (x, y) ∉ R
-}
isAntisymmetric : DerivedInfo -> Bool
isAntisymmetric info =
    Set.isEmpty info.superfuousForAntisymmetry


{-| Set of pairs that are problematic for antisymmetry.
-}
superfluousForAntisymmetry : Rel -> Set Pair
superfluousForAntisymmetry rel =
    let
        relMinusEye =
            difference rel (eye (size rel))
    in
    intersection relMinusEye (converse relMinusEye)
        |> elements
        |> Set.fromList


explainAntisymmetric : DerivedInfo -> ExplanationData
explainAntisymmetric info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is antisymmetric if ∀ x, y ∈ X: (x, y) ∈ R ∧ (y, x) ∈ R ⇒ x = y."
    in
    if Set.isEmpty info.superfuousForAntisymmetry then
        { greenHighlight = Set.empty
        , redHighlight = Set.empty
        , lines =
            [ "This relation is antisymmetric."
            , definition
            , "Explanation: whenever there is an off-diagonal element (x, y), "
                ++ "the corresponding \"mirror image\" element (y, x) must not be present."
            ]
                ++ List.map
                    (\( x, y ) ->
                        showPair ( x, y )
                            ++ " is present, so "
                            ++ showPair ( y, x )
                            ++ " must not be present. ✓"
                    )
                    (Set.toList info.offDiagonalElements)
        }

    else
        let
            belowDiagonalProblematic =
                Set.filter (\( x, y ) -> x > y) info.superfuousForAntisymmetry
        in
        { greenHighlight = Set.empty
        , redHighlight = info.superfuousForAntisymmetry
        , lines =
            [ "This relation is not antisymmetric."
            , definition
            , explanationPrefix "antisymmetric: ∃ x, y ∈ X: x ≠ y ∧ (x, y) ∈ R ∧ (y, x) ∈ R."
                ++ (let
                        problematicPairCount =
                            Set.size belowDiagonalProblematic

                        ( isAre, elements_ ) =
                            isAreElements problematicPairCount
                    in
                    "There "
                        ++ isAre
                        ++ " "
                        ++ elements_
                        ++ " of the form (x, y) for which the corresponding \"mirror image\""
                        ++ " (y, x) is also present:"
                   )
            ]
                ++ List.map
                    (\( x, y ) ->
                        "Both "
                            ++ showPair ( x, y )
                            ++ " and "
                            ++ showPair ( y, x )
                            ++ " are present. ✗"
                    )
                    (Set.toList belowDiagonalProblematic)
        }


{-| ∀ x, y ∈ X: (x, y) ∈ R ⇒ (y, x) ∉ R
-}
isAsymmetric : DerivedInfo -> Bool
isAsymmetric info =
    let
        ( onDiagSuperfluous, offDiagSuperfluous ) =
            info.superfluousForAsymmetry
    in
    Set.isEmpty onDiagSuperfluous && Set.isEmpty offDiagSuperfluous


superfluousForAsymmetry : Rel -> ( Set Pair, Set Pair )
superfluousForAsymmetry rel =
    intersection rel (converse rel)
        |> elements
        |> Set.fromList
        |> Set.partition (\( i, j ) -> i == j)


explainAsymmetric : DerivedInfo -> ExplanationData
explainAsymmetric info =
    let
        ( superfluousDiagonal, superfluousOffDiagonal ) =
            info.superfluousForAsymmetry

        definition =
            "Definition: a relation R ⊆ X ⨯ X is asymmetric if ∀ x, y ∈ X: (x, y) ∈ R ⇒ (y, x) ∉ R."
    in
    if Set.isEmpty superfluousDiagonal && Set.isEmpty superfluousOffDiagonal then
        let
            diagonalPairsList =
                List.map (\i -> ( i, i )) info.domain

            diagonalPairs =
                Set.fromList diagonalPairsList
        in
        { greenHighlight =
            Set.union info.offDiagonalElements
                -- Highlight that diagonal doesn't contain elements
                diagonalPairs
        , redHighlight = Set.empty
        , lines =
            [ "This relation is asymmetric."
            , definition
            , "Explanation: whenever there is an element (x, y), the element (y, x) must not be present."
                ++ " Notice the subtle difference from asymmetry, which permits elements of the form (x, x),"
                ++ " while asymmetry does not allow them."
            ]
                ++ List.map
                    (\( x, y ) ->
                        -- TODO these lists can get potentially long, so ellipsisize them somehow
                        showPair ( x, y )
                            ++ " is present, so "
                            ++ showPair ( y, x )
                            ++ " must not be present. ✓"
                    )
                    (Set.toList info.offDiagonalElements)
                ++ List.map
                    (\( x, y ) ->
                        showPair ( x, y )
                            ++ " is not present. ✓"
                    )
                    diagonalPairsList
        }

    else
        { greenHighlight = Set.empty
        , redHighlight = Set.union superfluousDiagonal superfluousOffDiagonal
        , lines =
            [ "This relation is not asymmetric."
            , definition
            , explanationPrefix "asymmetric: ∃ x, y ∈ X: (x, y) ∈ R ∧ (y, x) ∈ R. "
                ++ "Note that this also applies to elements of the form (x, x), "
                ++ "whose presence breaks asymmetry."
            ]
                ++ (let
                        offDiagCount =
                            Set.size superfluousOffDiagonal

                        ( isAre, elements_ ) =
                            isAreElements (offDiagCount // 2)
                    in
                    if offDiagCount > 0 then
                        ("There "
                            ++ isAre
                            ++ " "
                            ++ elements_
                            ++ " of the form (x, y) for which the corresponding \"mirror image\""
                            ++ " (y, x) is also present:"
                        )
                            :: List.map
                                (\( x, y ) ->
                                    "Both "
                                        ++ showPair ( x, y )
                                        ++ " and "
                                        ++ showPair ( y, x )
                                        ++ " are present. ✗"
                                )
                                (let
                                    belowDiagonalProblematic =
                                        Set.filter (\( x, y ) -> x > y) superfluousOffDiagonal
                                 in
                                 Set.toList belowDiagonalProblematic
                                )

                    else
                        []
                   )
                ++ (let
                        diagonalCount =
                            Set.size superfluousDiagonal

                        ( isAre, elements_ ) =
                            isAreElements diagonalCount
                    in
                    if diagonalCount > 0 then
                        ("There "
                            ++ isAre
                            ++ " "
                            ++ elements_
                            ++ " of the form (x, x):"
                        )
                            :: List.map
                                (\( x, y ) -> showPair ( x, y ) ++ " is present. ✗")
                                (Set.toList superfluousDiagonal)

                    else
                        []
                   )
        }


{-| ∀ x, y, z ∈ X: (x, y) ∈ R ∧ (y, z) ∈ R ⇒ (x, z) ∈ R
-}
isTransitive : DerivedInfo -> Bool
isTransitive info =
    Set.isEmpty <| Tuple.first info.missingForTransitivity


missingForTransitivity : Rel -> ( Set Pair, List TransitiveClosureStep )
missingForTransitivity rel =
    let
        ( trClosure, closureSteps ) =
            transitiveClosureWithSteps rel
    in
    ( difference trClosure rel
        |> elements
        |> Set.fromList
    , closureSteps
    )


explainTransitive : DerivedInfo -> ExplanationData
explainTransitive info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is transitive if ∀ x, y, z ∈ X: (x, y) ∈ R ∧ (y, z) ∈ R ⇒ (x, z) ∈ R."

        ( missingPairs, closureSteps ) =
            info.missingForTransitivity
    in
    if Set.isEmpty missingPairs then
        { greenHighlight =
            -- TODO what to highlight? Everything, or only those whose absence could break transitivity
            -- (e.g. elems that would be removed by tr. reduction)
            Set.empty
        , redHighlight = Set.empty
        , lines =
            [ "This relation is transitive."
            , definition
            ]
        }

    else
        { greenHighlight = Set.empty
        , redHighlight = missingPairs
        , lines =
            [ "This relation is not transitive."
            , definition
            , explanationPrefix "transitive: ∃ x, y, z ∈ X: (x, y) ∈ R ∧ (y, z) ∈ R ∧ (x, z) ∉ R."
            , "Here is what we need to do to make the relation transitive."
            ]
                ++ (closureSteps
                        |> List.groupWhile (\a b -> a.distance == b.distance)
                        |> List.andThen
                            (\( first, others ) ->
                                (case ( first.distance, List.isEmpty others ) of
                                    ( 2, True ) ->
                                        "We need to add the following missing pair:"

                                    ( 2, False ) ->
                                        "We need to add the following missing pairs:"

                                    ( _, True ) ->
                                        "After adding those, we get another pair of the form (x, y) and (y, z) for which (x, z) is missing:"

                                    ( _, False ) ->
                                        "After adding those, we get new pairs of the form (x, y) and (y, z) for which (x, z) is missing:"
                                )
                                    :: List.map
                                        (\{ from, through, to } ->
                                            showPair ( from, through )
                                                ++ " and "
                                                ++ showPair ( through, to )
                                                ++ " are present, but "
                                                ++ showPair ( from, to )
                                                ++ " is missing. ✗"
                                        )
                                        (first :: others)
                            )
                        |> (\explanationRows ->
                                explanationRows
                                    ++ [ "After adding those, the relation becomes transitive." ]
                           )
                   )
        }


{-| ∀ x, y ∈ X: x ≠ y ⇒ (x, y) ∈ R ∨ (y, x) ∈ R
-}
isConnected : DerivedInfo -> Bool
isConnected info =
    Set.isEmpty info.missingForConnectedness


explainConnected : DerivedInfo -> ExplanationData
explainConnected info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is connected if ∀ x, y ∈ X: x ≠ y ⇒ (x, y) ∈ R ∨ (y, x) ∈ R."
    in
    if Set.isEmpty info.missingForConnectedness then
        { greenHighlight = Set.empty
        , redHighlight = Set.empty
        , lines =
            [ "This relation is connected."
            , definition
            ]
        }

    else
        { greenHighlight = Set.empty
        , redHighlight = info.missingForConnectedness
        , lines =
            [ "This relation is not connected."
            , definition
            , explanationPrefix "connected: ∃ x, y ∈ X: x ≠ y ∧ (x, y) ∉ R ∧ (y, x) ∉ R."
            , "These pairs of elements are problematic: "
            ]
                ++ (Set.toList info.missingForConnectedness
                        |> List.filter (\( a, b ) -> a > b)
                        |> List.map (\( x, y ) -> "Both " ++ showPair ( x, y ) ++ " and " ++ showPair ( y, x ) ++ " are missing. ✗")
                   )
                ++ [ "We would have to add at least one of each to make the relation connected." ]
        }


{-| A binary relation is acyclic if it contains no cycles.
Equivalently, its transitive closure is antisymmetric.
-}
mkAcyclic : Rel -> Result (List Int) Acyclic
mkAcyclic rel =
    case findCycle rel of
        Just cycle ->
            Err cycle

        Nothing ->
            Ok (Acyclic rel)


isLattice : DerivedInfo -> Bool
isLattice info =
    case info.acyclicInfo of
        Ok acInfo ->
            case acInfo.posetInfo of
                Just posetInfo ->
                    isPosetLattice posetInfo

                Nothing ->
                    False

        Err _ ->
            False


isPosetLattice : PosetInfo -> Bool
isPosetLattice { meetTable, joinTable } =
    Result.map2 (\_ _ -> True) meetTable joinTable
        |> Result.withDefault False


type JoinOrMeetResult
    = -- join (meet) of 2 elements is the greater (lesser) of the two
      ComparableJoin Int
    | -- join (meet) of 2 elements is not one of the two being joined (met)
      NonTrivialJoin Int


type JoinOrMeetError
    = NoCommonBounds
    | MultipleBounds (Set Int)


type alias JoinOrMeetErrors =
    { noCommonBounds : Set Pair
    , multipleBounds : Dict Pair (Set Int)
    , almostJoinOrMeetTable : Array (Array (Result JoinOrMeetError JoinOrMeetResult))
    }


{-| Calculate a Dict that maps each element of the underlying set of the relation
to the set of all elements that are greater than or equal to it.

Given Acyclic relation, we can calculate upper sets of all elements efficiently,
by starting at the end of topological order.

-}
getUpperSetDict : Acyclic -> Dict Int (Set Int)
getUpperSetDict ((Acyclic (Rel rows)) as acyclic) =
    topologicalSort acyclic
        |> List.foldr
            (\n accDict ->
                let
                    upperN =
                        Array.get n rows
                            |> Maybe.withDefault Array.empty
                            |> indexedFoldr
                                (\i bool ->
                                    if bool then
                                        case Dict.get i accDict of
                                            Just upperI ->
                                                Set.union upperI

                                            Nothing ->
                                                identity

                                    else
                                        identity
                                )
                                (Set.singleton n)
                in
                Dict.insert n upperN accDict
            )
            Dict.empty


joinAttempt : Poset -> Result JoinOrMeetErrors (Array (Array JoinOrMeetResult))
joinAttempt (Poset ((Rel rows) as rel)) =
    -- TODO this is very naive, inefficient implementation. Find a way to make it less so.
    let
        -- Safe, because every poset is acyclic
        acyclic =
            Acyclic rel

        upperSetDict =
            getUpperSetDict acyclic

        (Rel coveringRelRows) =
            reflexiveReduction <| transitiveReduction acyclic

        tryJoin : Int -> Int -> Result JoinOrMeetError JoinOrMeetResult
        tryJoin i j =
            if unsafeGet i j rows then
                -- if i <= j then j is their join
                Ok (ComparableJoin j)

            else if unsafeGet j i rows then
                -- if j <= i then i is their join
                Ok (ComparableJoin i)

            else
                -- i and j not comparable - intersect their upper sets
                let
                    upperIupperJIntersection =
                        Dict.get i upperSetDict
                            |> Maybe.andThen
                                (\upperI ->
                                    Dict.get j upperSetDict
                                        |> Maybe.map (Set.intersect upperI)
                                )
                            |> Maybe.withDefault Set.empty
                in
                if Set.isEmpty upperIupperJIntersection then
                    Err NoCommonBounds

                else
                    -- non-empty upper set intersection
                    let
                        elems =
                            Set.toList upperIupperJIntersection

                        minimalElems =
                            List.filter
                                (\colIdx ->
                                    -- Minimal elements have no incoming edges
                                    -- = all values within subrectangle whose rows and column indices
                                    -- are determined by the indices in the upper set intersection are False
                                    List.all (\rowIdx -> unsafeGet rowIdx colIdx coveringRelRows == False) elems
                                )
                                elems
                    in
                    case minimalElems of
                        [] ->
                            -- Shouldn't happen
                            Err NoCommonBounds

                        [ x ] ->
                            -- unique minimum -> it's the join
                            Ok (NonTrivialJoin x)

                        _ ->
                            Err (MultipleBounds (Set.fromList minimalElems))

        n =
            size rel

        rawResult =
            Array.initialize n
                (\i ->
                    Array.initialize n
                        (\j -> tryJoin i j)
                )

        ( noCommonUpperBounds, noUniqueUpperBounds ) =
            indexedFoldr
                (\i row acc ->
                    indexedFoldr
                        (\j cell ( noUps, noUniqs ) ->
                            case cell of
                                Ok _ ->
                                    ( noUps, noUniqs )

                                Err e ->
                                    case e of
                                        NoCommonBounds ->
                                            ( Set.insert ( i, j ) noUps, noUniqs )

                                        MultipleBounds s ->
                                            ( noUps, Dict.insert ( i, j ) s noUniqs )
                        )
                        acc
                        row
                )
                ( Set.empty, Dict.empty )
                rawResult
    in
    if Set.isEmpty noCommonUpperBounds && Dict.isEmpty noUniqueUpperBounds then
        Ok
            (Array.map
                (Array.map
                    (\res ->
                        case res of
                            Ok r ->
                                r

                            -- This is safe, as we know there are no join errors based on the above check
                            Err _ ->
                                ComparableJoin 0
                    )
                )
                rawResult
            )

    else
        Err
            { noCommonBounds = noCommonUpperBounds
            , multipleBounds = noUniqueUpperBounds
            , almostJoinOrMeetTable = rawResult
            }


meetAttempt : Poset -> Result JoinOrMeetErrors (Array (Array JoinOrMeetResult))
meetAttempt (Poset rel) =
    joinAttempt (Poset (converse rel))


{-| TODO this should really accept something like Covering or Poset
-}
minMaxElements : Rel -> ( Set Int, Set Int )
minMaxElements ((Rel rows) as rel) =
    let
        (Rel opRows) =
            converse rel

        rowsWithOutdegree0 =
            indexedFoldr
                (\i row acc ->
                    if arrayOr row then
                        acc

                    else
                        Set.insert i acc
                )
                Set.empty

        minimal =
            rowsWithOutdegree0 opRows

        maximal =
            rowsWithOutdegree0 rows
    in
    ( minimal, maximal )


type alias JoinMeetConfig =
    { opSymbol : String -- ∨ or ∧
    , opName : String -- join or meet
    , boundName : String -- upper or lower bound
    , minMax : String -- minimal or maximal
    }


viewJoinInfo : Result JoinOrMeetErrors (Array (Array JoinOrMeetResult)) -> List (Html msg)
viewJoinInfo =
    viewJoinOrMeetInfo joinConfig


viewMeetInfo : Result JoinOrMeetErrors (Array (Array JoinOrMeetResult)) -> List (Html msg)
viewMeetInfo =
    viewJoinOrMeetInfo meetConfig


joinConfig : JoinMeetConfig
joinConfig =
    { opSymbol = "∨", opName = "join", boundName = "upper", minMax = "minimal" }


meetConfig : JoinMeetConfig
meetConfig =
    { opSymbol = "∧", opName = "meet", boundName = "lower", minMax = "maximal" }


viewJoinOrMeetInfo : JoinMeetConfig -> Result JoinOrMeetErrors (Array (Array JoinOrMeetResult)) -> List (Html msg)
viewJoinOrMeetInfo ({ opSymbol, opName, boundName, minMax } as config) result =
    case result of
        Ok opData ->
            [ Html.text <| "The " ++ opSymbol ++ " (" ++ opName ++ ") operation is well defined."
            , binaryOpTable opSymbol (viewOpResultTableCell config) opData
            ]

        Err joinErrors ->
            let
                noBoundsExplanation =
                    if Set.isEmpty joinErrors.noCommonBounds then
                        []

                    else
                        [ Html.div []
                            [ Html.text <|
                                "The following pairs don't have a common "
                                    ++ boundName
                                    ++ " bound: "
                                    ++ showPairSet joinErrors.noCommonBounds
                            ]
                        ]

                multiBoundsExplanation =
                    if Dict.isEmpty joinErrors.multipleBounds then
                        []

                    else
                        [ Html.div []
                            [ Html.text <| "The following pairs have multiple " ++ minMax ++ " " ++ boundName ++ " bounds: "
                            , Html.ul [] <|
                                List.map
                                    (\( p, multiBounds ) ->
                                        Html.li []
                                            [ Html.text <| showPair p ++ " -> " ++ showIntSet multiBounds ]
                                    )
                                    (Dict.toList joinErrors.multipleBounds)
                            ]
                        ]
            in
            Html.text ("The " ++ opSymbol ++ " (" ++ opName ++ ") operation is not well defined. ")
                :: (noBoundsExplanation
                        ++ multiBoundsExplanation
                        ++ [ binaryOpTable opSymbol (viewOpErrorOrResultTableCell config) joinErrors.almostJoinOrMeetTable ]
                   )


binaryOpTable : String -> (a -> Html msg) -> Array (Array a) -> Html msg
binaryOpTable opSymbol viewCell opTable =
    let
        n =
            Array.length opTable
    in
    Html.table []
        (Html.thead []
            (Html.th [] [ Html.text opSymbol ]
                :: List.map (\i -> Html.th [] [ Html.text (String.fromInt i) ])
                    (List.range 0 (n - 1))
            )
            :: List.indexedMap
                (\i row ->
                    Html.tr []
                        (Html.th [] [ Html.text (String.fromInt i) ]
                            :: List.map
                                (\cell -> viewCell cell)
                                (Array.toList row)
                        )
                )
                (Array.toList opTable)
        )


viewOpResultTableCell : JoinMeetConfig -> JoinOrMeetResult -> Html msg
viewOpResultTableCell { opName } cell =
    Html.td []
        [ case cell of
            ComparableJoin k ->
                Html.text <| String.fromInt k

            NonTrivialJoin k ->
                Html.b [ A.title <| "Non-trivial " ++ opName ++ " result (different from both operands)" ] [ Html.text <| String.fromInt k ]
        ]


viewOpErrorOrResultTableCell : JoinMeetConfig -> Result JoinOrMeetError JoinOrMeetResult -> Html msg
viewOpErrorOrResultTableCell config res =
    case res of
        Ok r ->
            viewOpResultTableCell config r

        Err e ->
            case e of
                NoCommonBounds ->
                    Html.td
                        [ A.style "background-color" "salmon"
                        , A.title <| "These two elements don't have any common " ++ config.boundName ++ " bound"
                        ]
                        [ Html.text "?" ]

                MultipleBounds bounds ->
                    Html.td
                        [ A.style "background-color" "orange"
                        , A.title <| "Multiple " ++ config.minMax ++ " " ++ config.boundName ++ " bounds: " ++ showIntSet bounds
                        ]
                        [ Html.text "!!" ]


isAcyclic : DerivedInfo -> Bool
isAcyclic info =
    case info.acyclicInfo of
        Ok _ ->
            True

        Err _ ->
            False


findCycle : Rel -> Maybe (List Int)
findCycle (Rel rows) =
    let
        maxIndex =
            Array.length rows - 1

        adjacentNodes : Int -> List Int
        adjacentNodes i =
            Array.get i rows
                |> Maybe.withDefault Array.empty
                |> (\row ->
                        let
                            ( _, adjacent ) =
                                Array.foldr
                                    (\bool ( j, acc ) ->
                                        ( j - 1
                                        , if bool && i /= j then
                                            j :: acc

                                          else
                                            acc
                                        )
                                    )
                                    ( maxIndex, [] )
                                    row
                        in
                        adjacent
                   )

        dfs : Int -> Maybe (List Int)
        dfs start =
            let
                loop : List ( Int, List Int ) -> Set Int -> Maybe (List Int)
                loop stack visited =
                    case stack of
                        [] ->
                            Nothing

                        ( i, path ) :: rest ->
                            if Set.member i visited then
                                if List.member i path then
                                    Just (i :: List.reverse (List.takeWhile ((/=) i) path))

                                else
                                    loop rest visited

                            else
                                loop
                                    (List.map
                                        (\adj -> ( adj, i :: path ))
                                        (adjacentNodes i)
                                        ++ rest
                                    )
                                    (Set.insert i visited)
            in
            loop [ ( start, [] ) ] Set.empty
    in
    List.range 0 maxIndex
        |> List.findMap dfs


explainAcyclic : DerivedInfo -> ExplanationData
explainAcyclic info =
    let
        definition =
            "Definition: relation is acyclic if it contains no cycles."
                ++ " Cycle is a sequence of at least 2 elements x1, x2, ..., xn ∈ X: (x1, x2) ∈ R ∧ (x2, x3) ∈ R ∧ ... ∧ (xn, x1) ∈ R."
    in
    case info.acyclicInfo of
        Ok _ ->
            { greenHighlight = Set.empty
            , redHighlight = Set.empty
            , lines =
                [ "This relation is acyclic."
                , definition
                , "Explanation: this relation doesn't contain any cycles, so it is acyclic."
                ]
            }

        Err cycle ->
            let
                elemsToPairs xs =
                    case xs of
                        [] ->
                            []

                        fst :: rest ->
                            List.map2 Tuple.pair xs (rest ++ [ fst ])

                cyclePairs =
                    elemsToPairs cycle
            in
            { redHighlight = Set.fromList cyclePairs
            , greenHighlight = Set.empty
            , lines =
                [ "This relation is not acyclic."
                , definition
                , explanationPrefix "acyclic: ∃ x1, x2, ..., xn ∈ X: (x1, x2) ∈ R ∧ (x2, x3) ∈ R ∧ ... ∧ (xn, x1) ∈ R."
                , "The following pairs form a cycle: " ++ showPairListAsSet cyclePairs
                , "so the cycle consists of this sequence of elements: " ++ showIntListAsList cycle
                ]
            }


missingForConnectedness : Rel -> Set Pair
missingForConnectedness rel =
    difference (complement (union rel (converse rel))) (eye (size rel))
        |> elements
        |> Set.fromList


isFunctional : DerivedInfo -> Bool
isFunctional info =
    Set.isEmpty info.superfluousForFunctional


{-| Return all pairs that live within rows with more than one True element
-}
superfluousForFunctional : Rel -> Set Pair
superfluousForFunctional (Rel rows) =
    Array.toIndexedList rows
        |> List.foldl
            (\( i, row ) acc ->
                case Array.toIndexedList row |> List.filter Tuple.second |> List.map Tuple.first of
                    [] ->
                        acc

                    [ _ ] ->
                        acc

                    more ->
                        List.map (\j -> ( i, j )) more |> Set.fromList |> Set.union acc
            )
            Set.empty


explainFunctional : DerivedInfo -> ExplanationData
explainFunctional info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is functional (also called right-unique or univalent) if "
                ++ "∀ x, y, z ∈ X: (x, y) ∈ R ∧ (x, z) ∈ R ⇒ y = z"
    in
    if Set.isEmpty info.superfluousForFunctional then
        { redHighlight = Set.empty
        , greenHighlight = Set.empty
        , lines =
            [ "This relation is functional."
            , definition
            , "Intuitively, this means that each x ∈ X is in relation with at most one y ∈ X."
            ]
        }

    else
        let
            xsWithMoreThanOneY =
                Set.map Tuple.first info.superfluousForFunctional

            ( isAre, elements_ ) =
                isAreElements <| Set.size xsWithMoreThanOneY

            xsToYs =
                Dict.toList <|
                    Set.foldl
                        (\( x, y ) ->
                            Dict.update x
                                (\maybeYs ->
                                    case maybeYs of
                                        Just ys ->
                                            Just (ys ++ [ y ])

                                        Nothing ->
                                            Just [ y ]
                                )
                        )
                        Dict.empty
                        info.superfluousForFunctional
        in
        { redHighlight = info.superfluousForFunctional
        , greenHighlight = Set.empty
        , lines =
            [ "This relation is not functional."
            , definition
            , explanationPrefix "functional: ∃ x, y, z ∈ X: (x, y) ∈ R ∧ (x, z) ∈ R ∧ y ≠ z."
            , "There " ++ isAre ++ " " ++ elements_ ++ " which " ++ isAre ++ " in relation with more than one y ∈ X:"
            ]
                ++ List.map
                    (\( x, ys ) ->
                        String.fromInt x
                            ++ " is in relation with "
                            ++ String.fromInt (List.length ys)
                            ++ " elements: "
                            ++ showIntListAsSet ys
                    )
                    xsToYs
        }


isLeftTotal : DerivedInfo -> Bool
isLeftTotal info =
    Set.isEmpty info.emptyRowIndices


superfluousAndMissingForFunction : Rel -> ( Set Pair, Set Int )
superfluousAndMissingForFunction (Rel rows) =
    Array.toIndexedList rows
        |> List.foldl
            (\( i, row ) ( sup, mis ) ->
                case Array.toIndexedList row |> List.filter Tuple.second |> List.map Tuple.first of
                    [] ->
                        -- mark whole row of pairs as missing
                        ( sup
                        , Set.insert i mis
                        )

                    [ _ ] ->
                        ( sup, mis )

                    more ->
                        ( List.map (\j -> ( i, j )) more
                            |> Set.fromList
                            |> Set.union sup
                        , mis
                        )
            )
            ( Set.empty, Set.empty )


explainLeftTotal : DerivedInfo -> ExplanationData
explainLeftTotal info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is left-total if ∀ x ∈ X: ∃ y ∈ X: (x, y) ∈ R."
    in
    if Set.isEmpty info.emptyRowIndices then
        { greenHighlight = Set.empty
        , redHighlight = Set.empty
        , lines =
            [ "This relation is left-total."
            , definition
            , "Intuitively, this means that every x ∈ X needs to be in relation with at least y ∈ X, which is the case."
            ]
        }

    else
        { greenHighlight = Set.empty
        , redHighlight =
            Set.toList info.emptyRowIndices
                |> List.andThen (\i -> List.map (Tuple.pair i) info.domain)
                |> Set.fromList
        , lines =
            [ "This relation is not left-total."
            , definition
            , explanationPrefix "left-total: ∃ x ∈ X: ∀ y ∈ X: (x, y) ∉ R."
            ]
                ++ (let
                        ( isAre, elements_ ) =
                            isAreElements <| Set.size info.emptyRowIndices
                    in
                    [ "There "
                        ++ isAre
                        ++ " "
                        ++ elements_
                        ++ " which "
                        ++ isAre
                        ++ " not in relation with any elements y ∈ X:"
                        ++ showIntListAsSet (Set.toList info.emptyRowIndices)
                    ]
                   )
        }


isFunctionRel : Rel -> Bool
isFunctionRel r =
    let
        ( sup, mis ) =
            superfluousAndMissingForFunction r
    in
    Set.isEmpty sup && Set.isEmpty mis


isBijectiveFunction : Rel -> Bool
isBijectiveFunction rel =
    isFunctionRel rel && isFunctionRel (converse rel)


isDerangement : Rel -> Bool
isDerangement rel =
    isBijectiveFunction rel && Set.isEmpty (superfluousForIrreflexivity rel)


isInvolution : Rel -> Bool
isInvolution rel =
    isFunctionRel rel && rel == converse rel


isPartialOrder : DerivedInfo -> Bool
isPartialOrder info =
    isReflexive info && isAntisymmetric info && isTransitive info


isTotalOrder : DerivedInfo -> Bool
isTotalOrder info =
    isPartialOrder info && isConnected info



-- CLOSURES


{-| <https://en.wikipedia.org/wiki/Reflexive_closure>
-}
reflexiveClosure : Rel -> Rel
reflexiveClosure rel =
    union rel (eye (size rel))


reflexiveReduction : Rel -> Rel
reflexiveReduction rel =
    difference rel (eye (size rel))


{-| <https://en.wikipedia.org/wiki/Symmetric_closure>
-}
symmetricClosure : Rel -> Rel
symmetricClosure rel =
    union rel (converse rel)


{-| <https://en.wikipedia.org/wiki/Transitive_closure>

This uses modified version of Floyd-Warshell algorithm

-}
transitiveClosure : Rel -> Rel
transitiveClosure ((Rel rows) as rel) =
    let
        n =
            Array.length rows
    in
    Rel <|
        List.foldl
            (\k reachable ->
                Array.initialize n
                    (\i ->
                        Array.initialize n
                            (\j ->
                                -- j was already reachable from i
                                unsafeGet i j reachable
                                    || -- or there is a path from i to j and from k to j
                                       (unsafeGet i k reachable && unsafeGet k j reachable)
                            )
                    )
            )
            rows
            (domain rel)


{-| Based on Theorem 3 from <https://www.cs.tufts.edu/comp/150FP/archive/al-aho/transitive-reduction.pdf>
TODO the paper also has a way to generate canonical TR of graphs with cycles
-}
transitiveReduction : Acyclic -> Rel
transitiveReduction (Acyclic rel) =
    let
        relNoLoops =
            reflexiveReduction rel

        relNoLoopsTC =
            transitiveClosure relNoLoops

        composed =
            compose relNoLoops relNoLoopsTC
    in
    difference rel composed


type Distance
    = Infinite
    | Finite Int


type alias TransitiveClosureStep =
    { from : Int
    , through : Int
    , to : Int
    , -- Number of pairs of the original relation
      -- that need to be traversed to go from "from" to "to"
      distance : Int
    }


addDist : Distance -> Distance -> Distance
addDist d1 d2 =
    case d1 of
        Infinite ->
            Infinite

        Finite n ->
            case d2 of
                Infinite ->
                    Infinite

                Finite m ->
                    Finite (n + m)


{-| Uses modified Floyd-Warshall algorithm.
-}
transitiveClosureWithSteps : Rel -> ( Rel, List TransitiveClosureStep )
transitiveClosureWithSteps ((Rel rows) as rel) =
    let
        n =
            Array.length rows

        toDistance : Bool -> Distance
        toDistance b =
            if b then
                Finite 1

            else
                Infinite

        isFinite : Distance -> Bool
        isFinite d =
            case d of
                Infinite ->
                    False

                Finite _ ->
                    True

        initDistances : Array (Array Distance)
        initDistances =
            Array.map (Array.map toDistance) rows

        lookup : Int -> Int -> Array (Array Distance) -> Distance
        lookup i j arr =
            Array.get i arr
                |> Maybe.andThen (Array.get j)
                |> Maybe.withDefault Infinite
    in
    List.foldl
        (\k ( distances, added ) ->
            let
                ( newDistances, newAdded ) =
                    Array.unzip <|
                        Array.map Array.unzip <|
                            Array.initialize n
                                (\i ->
                                    Array.initialize n
                                        (\j ->
                                            case
                                                ( lookup i j distances
                                                , addDist (lookup i k distances) (lookup k j distances)
                                                )
                                            of
                                                ( Infinite, Finite x ) ->
                                                    -- going through k gives us new edge  that wasn't in the original rel
                                                    ( Finite x, Just { from = i, through = k, to = j, distance = x } )

                                                ( Finite x, Finite y ) ->
                                                    if y < x then
                                                        -- going through k gives us shorter path than paths going through elements < k
                                                        ( Finite y, Just { from = i, through = k, to = j, distance = x } )

                                                    else
                                                        ( Finite x, Nothing )

                                                ( Infinite, Infinite ) ->
                                                    ( Infinite, Nothing )

                                                ( Finite x, Infinite ) ->
                                                    ( Finite x, Nothing )
                                        )
                                )

                newAdded2 =
                    Array.toList newAdded
                        |> List.andThen
                            (\row ->
                                Array.toList row |> List.filterMap identity
                            )
            in
            ( newDistances, newAdded2 ++ added )
        )
        ( initDistances, [] )
        (domain rel)
        |> Tuple.mapBoth
            (Array.map (Array.map isFinite) >> Rel)
            (List.sortBy (\trStep -> ( trStep.distance, trStep.from, trStep.to )))


{-| Strongly Connected Components.
Algorithm adapted from <https://hackage.haskell.org/package/containers-0.7/docs/src/Data.Graph.html#scc>
-}
scc : Rel -> List (List Int)
scc rel =
    List.map treeToList <| dfsFromNodes rel <| List.reverse <| postorder <| converse rel


type Tree a
    = Tree a (List (Tree a))


postorderTree : Tree a -> List a -> List a
postorderTree (Tree a ts) =
    postorderForest ts << (::) a


postorderForest : List (Tree a) -> List a -> List a
postorderForest ts =
    List.foldr (<<) identity <| List.map postorderTree ts


postorder : Rel -> List Int
postorder rel =
    postorderForest (spanningForest rel) []


topologicalSort : Acyclic -> List Int
topologicalSort (Acyclic rel) =
    List.reverse <| postorder rel


treeToList : Tree a -> List a
treeToList tree =
    treeToListAcc [ tree ] []


treeToListAcc : List (Tree a) -> List a -> List a
treeToListAcc stack accumulator =
    case stack of
        [] ->
            List.reverse accumulator

        (Tree a ts) :: rest ->
            treeToListAcc (List.append ts rest) (a :: accumulator)


spanningForest : Rel -> List (Tree Int)
spanningForest rel =
    dfsFromNodes rel (domain rel)


dfsFromNodes : Rel -> List Int -> List (Tree Int)
dfsFromNodes (Rel rows) startNodes =
    let
        maxIndex =
            Array.length rows - 1

        adjacentNodes : Int -> List Int
        adjacentNodes i =
            Array.get i rows
                |> Maybe.withDefault Array.empty
                |> (\row ->
                        let
                            ( _, adjacent ) =
                                Array.foldr
                                    (\bool ( j, acc ) ->
                                        ( j - 1
                                        , if bool && i /= j then
                                            j :: acc

                                          else
                                            acc
                                        )
                                    )
                                    ( maxIndex, [] )
                                    row
                        in
                        adjacent
                   )

        go : List Int -> Set Int -> ( List (Tree Int), Set Int )
        go toVisit visited =
            -- elm-review: IGNORE TCO
            case toVisit of
                [] ->
                    ( [], visited )

                v :: vs ->
                    if Set.member v visited then
                        go vs visited

                    else
                        let
                            visited1 =
                                Set.insert v visited

                            ( trees2, visited2 ) =
                                go (adjacentNodes v) visited1

                            ( trees3, visited3 ) =
                                go vs visited2
                        in
                        ( Tree v trees2 :: trees3, visited3 )

        ( trees, _ ) =
            go startNodes Set.empty
    in
    trees



-- GENERATORS


genBool : Float -> Generator Bool
genBool trueProb =
    Random.weighted ( trueProb, True ) [ ( 1 - trueProb, False ) ]


genRelation : Float -> Int -> Generator Rel
genRelation trueProb n =
    genBool trueProb
        |> Random.list n
        |> Random.list n
        |> Random.map (List.map Array.fromList >> Array.fromList >> Rel)


genReflexiveRelation : Float -> Int -> Random.Generator Rel
genReflexiveRelation =
    genWithDiagonal True


genIrreflexiveRelation : Float -> Int -> Random.Generator Rel
genIrreflexiveRelation =
    genWithDiagonal False


genWithDiagonal : Bool -> Float -> Int -> Random.Generator Rel
genWithDiagonal diag trueProb n =
    List.range 0 (n - 1)
        |> Random.traverse
            (\i ->
                List.range 0 (n - 1)
                    |> Random.traverse
                        (\j ->
                            if j == i then
                                Random.constant diag

                            else
                                genBool trueProb
                        )
                    |> Random.map Array.fromList
            )
        |> Random.map (Array.fromList >> Rel)


genSymmetricRelation : Float -> Int -> Random.Generator Rel
genSymmetricRelation trueProb n =
    List.range 0 (n - 1)
        |> Random.traverse
            (\i ->
                List.range 0 (n - 1)
                    |> Random.traverse
                        (\j ->
                            -- Generate only upper triangle and do symmetric closure
                            if j >= i then
                                genBool trueProb

                            else
                                Random.constant False
                        )
                    |> Random.map Array.fromList
            )
        |> Random.map (Array.fromList >> Rel >> symmetricClosure)


genAntisymmetricRelation : Float -> Int -> Random.Generator Rel
genAntisymmetricRelation trueProb n =
    -- Generate indices of all pairs within upper triangle, including diagonal
    List.range 0 (n - 1)
        |> List.andThen
            (\i ->
                List.range i (n - 1)
                    |> List.map (\j -> ( i, j ))
            )
        |> genAsymAntisymHelp trueProb n


genAsymmetricRelation : Float -> Int -> Random.Generator Rel
genAsymmetricRelation trueProb n =
    -- Generate indices of all pairs within upper triangle, excluding diagonal
    List.range 0 (n - 2)
        |> List.andThen
            (\i ->
                List.range (i + 1) (n - 1)
                    |> List.map (\j -> ( i, j ))
            )
        |> genAsymAntisymHelp trueProb n


{-| Logic shared by asymmetric and antisymmetric rel. generators
-}
genAsymAntisymHelp : Float -> Int -> List Pair -> Generator Rel
genAsymAntisymHelp trueProb n pairs =
    pairs
        |> Random.traverse
            (\( i, j ) ->
                -- Pick some of the pairs for inclusion in rel with trueProb
                genBool trueProb
                    |> Random.andThen
                        (\include ->
                            if include then
                                -- Go through picked ones and swap them with 50% probability
                                genBool 0.5
                                    |> Random.map
                                        (\swap ->
                                            Just <|
                                                if swap then
                                                    ( i, j )

                                                else
                                                    ( j, i )
                                        )

                            else
                                Random.constant Nothing
                        )
            )
        |> Random.map (List.filterMap identity >> fromElements n)


genConnectedRelation : Float -> Int -> Generator Rel
genConnectedRelation trueProb n =
    -- Generate indices of all pairs within upper triangle, including diagonal
    List.range 0 (n - 1)
        |> List.andThen
            (\i ->
                List.range i (n - 1)
                    |> List.map (\j -> ( i, j ))
            )
        |> Random.traverse
            (\( i, j ) ->
                -- Elements on diagonal are not "mandatory" - generate them with trueProb
                if i == j then
                    genBool trueProb
                        |> Random.map
                            (\include ->
                                if include then
                                    [ ( i, j ) ]

                                else
                                    []
                            )

                else
                    -- For off-diagnoal elements one has to be there for connectedness,
                    -- the other can be generated with trueProb.
                    genBool trueProb
                        |> Random.andThen
                            (\includeBoth ->
                                if includeBoth then
                                    Random.constant [ ( i, j ), ( j, i ) ]

                                else
                                    genBool 0.5
                                        |> Random.map
                                            (\swap ->
                                                [ if swap then
                                                    ( j, i )

                                                  else
                                                    ( i, j )
                                                ]
                                            )
                            )
            )
        |> Random.map (List.concat >> fromElements n)


genFunctionalRelation : Float -> Int -> Generator Rel
genFunctionalRelation trueProb n =
    -- trueProb determines how likely we are to generate single element in row i
    genBool trueProb
        |> Random.andThen
            (\includei ->
                if includei then
                    Random.map (\i -> Array.initialize n (\j -> j == i)) <| Random.int 0 (n - 1)

                else
                    Random.constant <| Array.repeat n False
            )
        |> Random.list n
        |> Random.map (Array.fromList >> Rel)


genLeftTotal : Float -> Int -> Generator Rel
genLeftTotal trueProb n =
    -- One element has to be there for totality
    Random.map ((::) True)
        -- Plus remaining n-1 elements with trueProb
        (Random.sequence (List.repeat (n - 1) (genBool trueProb)))
        |> Random.map Array.fromList
        |> Random.andThen Random.Array.shuffle
        |> Random.list n
        |> Random.map (Array.fromList >> Rel)


genBijectiveFunction : Int -> Generator Rel
genBijectiveFunction n =
    Array.initialize n identity
        |> Random.Array.shuffle
        |> Random.map
            (\shuffledArr ->
                Array.map (\indexOfTrue -> Array.initialize n (\i -> i == indexOfTrue)) shuffledArr
                    |> Rel
            )


genInvolution : Int -> Generator Rel
genInvolution n =
    let
        go : List Int -> Int -> List ( Int, Int ) -> Generator (List ( Int, Int ))
        go choices x generatedPairs =
            -- elm-review: IGNORE TCO
            Random.List.choose choices
                |> Random.andThen
                    (\( my, restChoices ) ->
                        case my of
                            Just y ->
                                let
                                    newChoices =
                                        List.filter (\c -> c /= x && c /= y) restChoices

                                    nextX =
                                        List.minimum newChoices |> Maybe.withDefault 0

                                    newGeneratedPairs =
                                        if x == y then
                                            -- fixed point
                                            ( x, y ) :: generatedPairs

                                        else
                                            -- cycle of length 2
                                            ( x, y ) :: ( y, x ) :: generatedPairs
                                in
                                go newChoices nextX newGeneratedPairs

                            Nothing ->
                                Random.constant generatedPairs
                    )

        mkRel : List ( Int, Int ) -> Rel
        mkRel pairs =
            Rel <|
                Array.initialize n <|
                    \i ->
                        Array.initialize n <|
                            \j -> List.member ( i, j ) pairs
    in
    go (List.range 0 (n - 1)) 0 []
        |> Random.map mkRel


genTotalOrder : Int -> Generator Rel
genTotalOrder n =
    List.range 0 (n - 1)
        |> Random.List.shuffle
        |> Random.map
            (\xs ->
                List.zip xs (List.drop 1 xs)
                    |> fromElements n
                    |> reflexiveClosure
                    |> transitiveClosure
            )


{-| Based on <https://mathematica.stackexchange.com/questions/608/how-to-generate-random-directed-acyclic-graphs/2266#answer-613>
"graph is acyclic if and only if there exists a vertex ordering which makes the adjacency matrix triangular"
-}
genAcyclic : Float -> Int -> Generator Rel
genAcyclic trueProb n =
    -- 1. Generate random upper triangular matrix (including diagonal)
    let
        upperTriangularGen : Generator (List Pair)
        upperTriangularGen =
            List.range 0 (n - 1)
                |> List.andThen
                    (\i ->
                        List.range i (n - 1)
                            |> List.map (\j -> ( i, j ))
                    )
                |> Random.traverse
                    (\( i, j ) ->
                        genBool trueProb
                            |> Random.map
                                (\include ->
                                    if include then
                                        [ ( i, j ) ]

                                    else
                                        []
                                )
                    )
                |> Random.map List.concat

        -- 2. Generate random permutation of indices
        permutationGen =
            Array.initialize n identity |> Random.Array.shuffle
    in
    Random.map2
        (\upperTriangularElements permutation ->
            -- 3. Apply the permutation to all the elements
            List.map
                (\( i, j ) ->
                    ( Array.get i permutation |> Maybe.withDefault i
                    , Array.get j permutation |> Maybe.withDefault j
                    )
                )
                upperTriangularElements
                |> fromElements n
        )
        upperTriangularGen
        permutationGen



-- FCA - Formal Concept Analysis
-- Here we consider `Rel` as a special-case representation of a formal context,
-- with first element of each pair representing objects and second representing attributes.


{-| This is like '' operator corresponding to composition `objectsSharingAllAttributes >> attributesSharedByAllObjects`
-}
attributeSetClosure : Rel -> Set Int -> Set Int
attributeSetClosure (Rel rows) attributes =
    let
        initSharedAttributes : List Bool
        initSharedAttributes =
            List.repeat (Array.length rows) True
    in
    Array.foldl
        (\row commonAttrs ->
            if
                -- if this row has all the input attributes, AND it with accumulator
                Set.foldl
                    (\attrIdx acc ->
                        Maybe.withDefault False (Array.get attrIdx row) && acc
                    )
                    True
                    attributes
            then
                List.map2 (&&) (Array.toList row) commonAttrs

            else
                commonAttrs
        )
        initSharedAttributes
        rows
        |> List.indexedMap
            (\attrIdx b ->
                if b then
                    Just attrIdx

                else
                    Nothing
            )
        |> List.filterMap identity
        |> Set.fromList


{-| Second derivation operator, based on:
B′ = {g ∈ G | (g,m) ∈ I for all m ∈ B}

Input: set representing attributes
Output: set representing objects that have all input attributes

-}
objectsSharingAllAttributes : Rel -> Set Int -> Set Int
objectsSharingAllAttributes (Rel rows) attributes =
    Array.toIndexedList rows
        |> List.filterMap
            (\( i, row ) ->
                if
                    Set.foldl
                        (\attrIdx acc ->
                            Maybe.withDefault False (Array.get attrIdx row) && acc
                        )
                        True
                        attributes
                then
                    Just i

                else
                    Nothing
            )
        |> Set.fromList


{-| List all atttribute closures in lectic order.
Algorithm described in <https://www.youtube.com/watch?v=rs3qSLEQYuM&list=PLISEtDmihMo2wvgHrsdQhV6AeUsqu-Cum&index=27>
-}
listAttributeClosures : Rel -> List (Set Int)
listAttributeClosures rel =
    let
        nextClosure : Set Int -> Maybe ( Set Int, Set Int )
        nextClosure attrs =
            let
                go : Set Int -> Int -> Maybe (Set Int)
                go attrs1 i =
                    if i < 0 then
                        Nothing

                    else if Set.member i attrs1 then
                        go (Set.remove i attrs1) (i - 1)

                    else
                        let
                            b =
                                -- (atrrs1 u {m})'
                                attributeSetClosure rel <| Set.insert i attrs1

                            bMinusAPrime =
                                Set.diff b attrs1
                        in
                        -- Does bMinusAPrime contain any attribute smaller then i?
                        if Set.foldl (\el acc -> el < i || acc) False bMinusAPrime then
                            go attrs1 (i - 1)

                        else
                            Just b
            in
            go attrs (size rel - 1) |> Maybe.map (\a -> ( a, a ))

        emptySetClosure =
            attributeSetClosure rel Set.empty
    in
    emptySetClosure :: List.unfoldr nextClosure emptySetClosure


applyPermutation : Permutation -> Rel -> Rel
applyPermutation p rel =
    elements rel
        |> List.map
            (\( i, j ) ->
                ( Permutation.get i p |> Maybe.withDefault i
                , Permutation.get j p |> Maybe.withDefault j
                )
            )
        |> fromElements (size rel)


mkPermutation : Rel -> Maybe Permutation
mkPermutation rel =
    if isBijectiveFunction rel then
        elements rel
            |> List.sortBy Tuple.first
            |> List.map Tuple.second
            |> Permutation.fromListUnsafe
            |> Just

    else
        Nothing



-- VIEW


view : Config msg -> Rel -> Highlight -> Html msg
view config ((Rel rows) as rel) highlight =
    let
        addBgColor : Int -> Int -> List (Attribute msg) -> List (Attribute msg)
        addBgColor i j =
            case highlight of
                Explanation _ explanation ->
                    (::)
                        (A.style "background-color" <|
                            if Set.member ( i, j ) explanation.redHighlight then
                                "salmon"

                            else if Set.member ( i, j ) explanation.greenHighlight then
                                "lightgreen"

                            else
                                "white"
                        )

                Pairs pairSet ->
                    (::)
                        (A.style "background-color" <|
                            if Set.member ( i, j ) pairSet then
                                "lightgreen"

                            else
                                "white"
                        )

                NoHighlight ->
                    identity
    in
    -- TODO it's not clear that cells should be clicked
    -- Need something like "click cells to toggle them / add/remove elements to/from relation"
    Html.div [ A.id "rel" ]
        [ Html.table []
            [ Html.thead []
                [ Html.tr [] <|
                    Html.th [] [{- empty top-left corner -}]
                        :: List.map headerCell (domain rel)
                ]
            , Html.tbody [] <|
                Array.toList <|
                    Array.indexedMap
                        (\i row ->
                            Html.tr [] <|
                                headerCell i
                                    :: (Array.toList <|
                                            Array.indexedMap
                                                (\j cell ->
                                                    Html.td
                                                        (addBgColor i j [ E.onClick <| config.toggle i j ])
                                                        [ Html.text <|
                                                            if cell then
                                                                "✓"

                                                            else
                                                                ""
                                                        ]
                                                )
                                                row
                                       )
                        )
                        rows
            ]
        ]


headerCell : Int -> Html msg
headerCell i =
    Html.th [] [ Html.text (String.fromInt i) ]


showElements : Rel -> String
showElements =
    elements >> showPairListAsSet


showIntSet : Set Int -> String
showIntSet =
    Set.toList
        >> showIntListAsSet


showPairSet : Set Pair -> String
showPairSet pairs =
    Set.toList pairs |> List.sort |> showPairListAsSet


showPairListAsSet : List Pair -> String
showPairListAsSet pairs =
    List.map showPair pairs
        |> String.join ", "
        |> (\s -> "{" ++ s ++ "}")


showIntListAsSet : List Int -> String
showIntListAsSet list =
    List.map String.fromInt list
        |> String.join ", "
        |> (\s -> "{" ++ s ++ "}")


showIntListAsList : List Int -> String
showIntListAsList list =
    List.map String.fromInt list
        |> String.join ", "
        |> (\s -> "[" ++ s ++ "]")


showPair : Pair -> String
showPair ( i, j ) =
    "(" ++ String.fromInt i ++ "," ++ String.fromInt j ++ ")"


isAreElements : Int -> ( String, String )
isAreElements count =
    if count == 1 then
        ( "is", "one element" )

    else
        ( "are", String.fromInt count ++ " elements" )


relationGraphToDotSource : Rel -> { highlightSccs : Bool, showArrowheads : Bool } -> String
relationGraphToDotSource rel { highlightSccs, showArrowheads } =
    let
        -- Compute SCCs even when not highlighting them.
        -- We need to keep the same node order in both highlighted/unhighlighted case
        -- to prevent graph layout changes when switching between them
        ( individualNodes, multiNodeComponents ) =
            scc rel
                |> List.partition (\component -> List.length component == 1)
                |> Tuple.mapFirst List.concat

        needsHighlight =
            highlightSccs && not (List.isEmpty multiNodeComponents)

        nodeLines =
            if needsHighlight then
                let
                    colors =
                        -- TODO this short list of colors should be enough for now, as 10 element set can have at most 5 SCCs
                        [ "lightgreen", "lightblue", "orange", "orchid", "khaki", "tan", "tomato" ]
                in
                List.map String.fromInt individualNodes
                    ++ ("node[style=filled]"
                            :: List.concat
                                (List.map2
                                    (\color nodes ->
                                        ("node[fillcolor=" ++ color ++ "]")
                                            :: List.map String.fromInt nodes
                                    )
                                    colors
                                    multiNodeComponents
                                )
                       )

            else
                individualNodes
                    ++ List.concat multiNodeComponents
                    |> List.map String.fromInt

        edgesLines =
            elements rel
                |> List.map (\( i, j ) -> String.fromInt i ++ "->" ++ String.fromInt j)

        edgeAttrs =
            if showArrowheads then
                "edge[arrowsize=0.5]"

            else
                "edge[arrowhead=none]"
    in
    String.join ";" <|
        "digraph G{graph[rankdir=BT;splines=true;overlap=false]"
            :: "node[shape=circle;width=0.3;fixedsize=true]"
            :: edgeAttrs
            :: (nodeLines ++ edgesLines)
            ++ [ "}" ]


relationBipartiteGraphDotSource : Rel -> String
relationBipartiteGraphDotSource rel =
    let
        domainNodes =
            domainCodomain "d" "0"

        codomainNodes =
            domainCodomain "c" "2"

        domainCodomain domCodomIdentifier xCoord =
            List.map
                (\i ->
                    let
                        yCoord =
                            -0.75 * toFloat i
                    in
                    domCodomIdentifier
                        ++ String.fromInt i
                        ++ ("[pos=\"" ++ xCoord ++ "," ++ String.fromFloat yCoord ++ "!\";")
                        ++ ("label=\"" ++ String.fromInt i ++ "\"]")
                )
                (domain rel)

        edges =
            List.map
                (\( i, j ) ->
                    "d"
                        ++ String.fromInt i
                        ++ "->c"
                        ++ String.fromInt j
                )
                (elements rel)
    in
    -- Based on https://observablehq.com/@magjac/placing-graphviz-nodes-in-fixed-positions
    String.join ";" <|
        "digraph G{node[shape=circle;width=0.3;fixedsize=true]"
            :: "edge[arrowsize=0.5]"
            :: (domainNodes ++ codomainNodes ++ edges ++ [ "}" ])


errorInBoxDot : String -> String
errorInBoxDot msg =
    "digraph G{node[shape=box];\"" ++ msg ++ "\"}"


hasseDiagramToDotSource : AcyclicInfo -> String
hasseDiagramToDotSource { coveringRelation } =
    relationGraphToDotSource
        coveringRelation
        { highlightSccs = False, showArrowheads = False }


conceptLatticeToDotSource : List ( Set Int, Set Int ) -> Bool -> Bool -> String
conceptLatticeToDotSource attributeClosures showExtents showIntents =
    let
        formalConcepts : List ( BitSet, BitSet )
        formalConcepts =
            List.map (Tuple.mapBoth BitSet.fromSet BitSet.fromSet) attributeClosures

        closureCount =
            List.length formalConcepts
    in
    if closureCount > 64 then
        errorInBoxDot "The concept lattice has too many nodes (>64) to be rendered."

    else
        let
            ( nodeAttributes, renderNode ) =
                if showExtents && showIntents then
                    ( "node[shape=record;width=0;height=0;margin=0;color=gray]"
                    , \( objSet, attrSet ) ->
                        BitSet.showInt attrSet ++ "[label=\"{" ++ BitSet.showSet True attrSet ++ "|" ++ BitSet.showSet True objSet ++ "}\"]"
                    )

                else if showExtents then
                    ( "node[shape=box;margin=0;width=0;height=0;color=gray]"
                    , \( objSet, attrSet ) ->
                        BitSet.showInt attrSet ++ "[label=\"" ++ BitSet.showSet False objSet ++ "\"]"
                    )

                else if showIntents then
                    ( "node[shape=box;margin=0;width=0;height=0;color=gray]"
                    , \( _, attrSet ) ->
                        BitSet.showInt attrSet ++ "[label=\"" ++ BitSet.showSet False attrSet ++ "\"]"
                    )

                else
                    ( "node[shape=point;width=0.1;color=black;fillcolor=none]"
                    , \( _, attrSet ) ->
                        BitSet.showInt attrSet ++ "[label=\"" ++ BitSet.showSet False attrSet ++ "\"]"
                    )

            nodes =
                List.map
                    renderNode
                    formalConcepts

            relElementToAttrBitSet : Array BitSet
            relElementToAttrBitSet =
                Array.fromList <| List.map Tuple.second formalConcepts

            getBitSet : Int -> BitSet
            getBitSet i =
                Array.get i relElementToAttrBitSet |> Maybe.withDefault BitSet.empty

            -- Represent ⊆ order relation on closure sets as Rel, so we can run transitive reduction on it
            latticeOrderRelation =
                -- Safe, because set containment is guaranteed to be poset and thus acyclic
                Acyclic <|
                    Rel <|
                        Array.initialize closureCount
                            (\i ->
                                Array.initialize closureCount
                                    (\j ->
                                        let
                                            a =
                                                getBitSet i

                                            b =
                                                getBitSet j
                                        in
                                        BitSet.isSubset a b && a /= b
                                    )
                            )

            latticeCoverRelationEdges =
                transitiveReduction latticeOrderRelation
                    |> elements
                    |> List.map
                        (\( i, j ) ->
                            BitSet.showInt (getBitSet i)
                                ++ "->"
                                ++ BitSet.showInt (getBitSet j)
                        )
        in
        String.join ";" <|
            "digraph G{rankdir=BT"
                :: nodeAttributes
                :: "edge[arrowhead=none]"
                :: (nodes ++ latticeCoverRelationEdges ++ [ "}" ])



-- Internal helpers


pointwise : (Bool -> Bool -> Bool) -> Rel -> Rel -> Rel
pointwise op (Rel r1) (Rel r2) =
    Rel <| Array.map2 (Array.map2 op) r1 r2


unsafeGet : Int -> Int -> Array (Array Bool) -> Bool
unsafeGet i j rows =
    Array.get i rows
        |> Maybe.andThen (Array.get j)
        |> Maybe.withDefault False


arrayOr : Array Bool -> Bool
arrayOr =
    Array.foldl (||) False


indexedFoldr : (Int -> a -> b -> b) -> b -> Array a -> b
indexedFoldr f initB arr =
    let
        maxIndex =
            Array.length arr - 1

        ( _, finalB ) =
            Array.foldr
                (\a ( i, b ) ->
                    ( i - 1, f i a b )
                )
                ( maxIndex, initB )
                arr
    in
    finalB



{- Unicode corner: ⇒ ∈ ∉ ⊆ ∀ ∃ ∧ ∨ ≠ ∅ -}
{- DEAD CODE, might come in handy later
   {-| Is the first relation subset of the 2nd?
   -}
   isSubsetOf : Rel -> Rel -> Bool
   isSubsetOf (Rel rowsA) (Rel rowsB) =
       arrayAnd <|
           Array.map2
               (\rowA rowB ->
                   arrayAnd <|
                       Array.map2
                           -- Implication: whenever A is true, B must be as well
                           (\cellA cellB -> not cellA || cellB)
                           rowA
                           rowB
               )
               rowsA
               rowsB

   arrayAnd : Array Bool -> Bool
   arrayAnd =
       Array.foldl (&&) True
-}
