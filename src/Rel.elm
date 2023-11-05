module Rel exposing
    ( Acyclic
    , Config
    , DerivedInfo
    , Explanation
    , Pair
    , Rel
    , complement
    , converse
    , deriveInfo
    , domain
    , empty
    , explainAntisymmetric
    , explainAsymmetric
    , explainFunction
    , explainIrreflexive
    , explainReflexive
    , explainRelation
    , explainSymmetric
    , explainTransitive
    , findCycle
    , genBijectiveFunction
    , genFunction
    , genPartialFunction
    , genReflexiveRelation
    , genRelation
    , isAntisymmetric
    , isAsymmetric
    , isFunction
    , isIrreflexive
    , isReflexive
    , isSymmetric
    , isTransitive
    , missingForConnectedness
    , reflexiveClosure
    , reflexiveReduction
    , resize
    , scc
    , showElements
    , showIntList
    , showPair
    , showPairList
    , showPairSet
    , size
    , superfluousForPartialFunction
    , symmetricClosure
    , toDotSource
    , toggle
    , topologicalSort
    , transitiveClosure
    , view, genIrreflexiveRelation
    )

import Array exposing (Array)
import Array.Extra as Array
import Html exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import List
import List.Extra as List
import Random exposing (Generator)
import Random.Array as Array
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


{-| An element of a relation.
For relation on set of size n, the are n^2 possible elements:
{(a,b) | a in {0..n-1}, b in {0..n-1}}
-}
type alias Pair =
    ( Int, Int )


type alias Explanation =
    { greenHighlight : Set Pair
    , redHighlight : Set Pair
    , lines : List String
    }


type alias DerivedInfo =
    { relSize : Int
    , domain : List Int
    , offDiagonalElements : Set Pair
    , onDiagonalElements : Set Pair
    , missingForReflexivity : Set Pair
    , superfluousForIrreflexivity : Set Pair
    , missingForSymmetry : Set Pair
    , superfuousForAntisymmetry : Set Pair
    , superfluousForAsymmetry : ( Set Pair, Set Pair )
    , missingForTransitivity : ( Set Pair, List TransitiveClosureStep )
    , superfluousForFunction : Set Pair
    , emptyRowIndices : Set Int
    , isConnected : Bool
    , isAcyclic : Bool
    , isPartialFunction : Bool
    , isBijectiveFunction : Bool
    , isDerangement : Bool
    , isInvolution : Bool
    , acyclic : Maybe Acyclic
    }


deriveInfo : Rel -> DerivedInfo
deriveInfo rel =
    let
        ( onDiagonalElements, offDiagonalElements ) =
            onAndOffDiagnoalElements rel

        ( superfluousForFunction, emptyRowIndices ) =
            superfluousAndMissingForFunction rel

        maybeAcyclic =
            mkAcyclic rel
    in
    { relSize = size rel
    , domain = domain rel
    , onDiagonalElements = onDiagonalElements
    , offDiagonalElements = offDiagonalElements
    , missingForReflexivity = missingForReflexivity rel
    , superfluousForIrreflexivity = superfluousForIrreflexivity rel
    , missingForSymmetry = missingForSymmetry rel
    , superfuousForAntisymmetry = superfluousForAntisymmetry rel
    , superfluousForAsymmetry = superfluousForAsymmetry rel
    , missingForTransitivity = missingForTransitivity rel
    , isConnected = isConnected rel
    , isPartialFunction = isPartialFunction rel
    , superfluousForFunction = superfluousForFunction
    , emptyRowIndices = emptyRowIndices
    , isBijectiveFunction = isBijectiveFunction rel
    , isDerangement = isDerangement rel
    , isInvolution = isInvolution rel
    , acyclic = maybeAcyclic
    , isAcyclic =
        case maybeAcyclic of
            Just _ ->
                True

            Nothing ->
                False
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
                                --  (+ corresponds to OR, * corresponds to AND)
                                arrayOr <| Array.map2 (&&) rowi colj
                            )
                            (Array.get i rows)
                            (Array.get j cols)
                            |> Maybe.withDefault False
                    )
            )


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


explainRelation : DerivedInfo -> Explanation
explainRelation info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is any subset of cartesian product X ⨯ X."
    in
    { greenHighlight = Set.union info.offDiagonalElements info.onDiagonalElements
    , redHighlight = Set.empty
    , lines =
        [ "This is a relation on X."
        , definition
        , "There are no special properties that relation must satisfy. It's just a set of pairs ☺."
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
    Array.toIndexedList rows
        |> List.foldr
            (\( i, row ) ->
                if Array.get i row |> Maybe.withDefault False then
                    identity

                else
                    Set.insert ( i, i )
            )
            Set.empty


explainReflexive : DerivedInfo -> Explanation
explainReflexive info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is reflexive if ∀ x ∈ X: (x, x) ∈ R."
    in
    if Set.isEmpty info.missingForReflexivity then
        { greenHighlight = Set.fromList <| List.map (\i -> ( i, i )) info.domain
        , redHighlight = Set.empty
        , lines =
            [ "This relation is reflexive."
            , definition
            , "Explanation: this relation contains all elements of the form (x, x), so it is reflexive."
            ]
        }

    else
        { greenHighlight = Set.empty
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
    Array.toIndexedList rows
        |> List.foldr
            (\( i, row ) ->
                if Array.get i row |> Maybe.withDefault False then
                    Set.insert ( i, i )

                else
                    identity
            )
            Set.empty


explainIrreflexive : DerivedInfo -> Explanation
explainIrreflexive info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is irreflexive if ∀ x ∈ X: (x, x) ∉ R."
    in
    if Set.isEmpty info.superfluousForIrreflexivity then
        { greenHighlight = Set.fromList <| List.map (\i -> ( i, i )) info.domain
        , redHighlight = Set.empty
        , lines =
            [ "This relation is irreflexive."
            , definition
            , "Explanation: this relation has no elements of the form (x, x), so it is irreflexive."
            ]
        }

    else
        { greenHighlight = Set.empty
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


explainSymmetric : DerivedInfo -> Explanation
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
                ++ (let
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
                   )
            ]
                ++ List.map (\( x, y ) -> showPair ( y, x ) ++ " is present, but " ++ showPair ( x, y ) ++ " is missing.")
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


explainAntisymmetric : DerivedInfo -> Explanation
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


explainAsymmetric : DerivedInfo -> Explanation
explainAsymmetric info =
    let
        ( superfluousDiagonal, superfluousOffDiagonal ) =
            info.superfluousForAsymmetry

        definition =
            "Definition: a relation R ⊆ X ⨯ X is asymmetric if ∀ x, y ∈ X: (x, y) ∈ R ⇒ (y, x) ∉ R."
    in
    if Set.isEmpty superfluousDiagonal && Set.isEmpty superfluousOffDiagonal then
        let
            allDiagonalSquares =
                Set.fromList <| List.map (\i -> ( i, i )) info.domain
        in
        { greenHighlight =
            Set.union info.offDiagonalElements
                -- Highlight that diagonal doesn't contain elements
                allDiagonalSquares
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
                    (Set.toList allDiagonalSquares)
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


explainTransitive : DerivedInfo -> Explanation
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

            -- TODO explanation
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


{-| a/=b ⇒ aRb or bRa
-}
isConnected : Rel -> Bool
isConnected (Rel rows) =
    let
        maxIndex =
            Array.length rows - 1
    in
    arrayAnd <|
        Array.indexedMap
            (\i row ->
                List.range (i + 1) maxIndex
                    |> List.all
                        (\j ->
                            Maybe.withDefault False (Array.get j row) || unsafeGet j i rows
                        )
            )
            rows


{-| A binary relation is acyclic if it contains no cycles.
Equivalently, its transitive closure is antisymmetric.
-}
isAcyclic : Rel -> Bool
isAcyclic rel =
    Set.isEmpty <| superfluousForAntisymmetry <| transitiveClosure rel


mkAcyclic : Rel -> Maybe Acyclic
mkAcyclic rel =
    if isAcyclic rel then
        Just (Acyclic rel)

    else
        Nothing


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


missingForConnectedness : Rel -> Set Pair
missingForConnectedness rel =
    difference (complement (union rel (converse rel))) (eye (size rel))
        |> elements
        |> Set.fromList


isPartialFunction : Rel -> Bool
isPartialFunction (Rel rows) =
    arrayAnd <|
        Array.map
            (\row ->
                let
                    trueCountInRow =
                        Array.foldl
                            (\elem elemCount ->
                                if elem then
                                    elemCount + 1

                                else
                                    elemCount
                            )
                            0
                            row
                in
                trueCountInRow == 0 || trueCountInRow == 1
            )
            rows


{-| Return all pairs that live within rows with more than one True element
-}
superfluousForPartialFunction : Rel -> Set Pair
superfluousForPartialFunction (Rel rows) =
    -- TODO ugly. It would be great to have indexed fold on arrays
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


isFunction : DerivedInfo -> Bool
isFunction info =
    Set.isEmpty info.superfluousForFunction && Set.isEmpty info.emptyRowIndices


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


explainFunction : DerivedInfo -> Explanation
explainFunction info =
    let
        definition =
            "Definition: a relation R ⊆ X ⨯ X is a function if it is both: "
                ++ "univalent (∀ x, y, z ∈ X: (x, y) ∈ R (x, z) ∈ R ⇒ y = z) and "
                ++ "total (∀ x ∈ X: ∃ y ∈ X: (x, y) ∈ R)."
    in
    if Set.isEmpty info.superfluousForFunction && Set.isEmpty info.emptyRowIndices then
        { greenHighlight = Set.empty
        , redHighlight = Set.empty
        , lines =
            [ "This relation is a function."
            , definition
            , "In words this means that every x ∈ X needs to be in relation with exactly one y ∈ X, which is the case."
            ]
        }

    else
        { greenHighlight = Set.empty
        , redHighlight =
            Set.union info.superfluousForFunction
                (Set.toList info.emptyRowIndices
                    |> List.andThen
                        (\i -> List.map (Tuple.pair i) info.domain)
                    |> Set.fromList
                )
        , lines =
            [ "This relation is not a function."
            , definition
            ]
                ++ (if not (Set.isEmpty info.superfluousForFunction) then
                        "It is not univalent, because there are some x ∈ X that map to more than one y ∈ X:"
                            :: (Set.toList info.superfluousForFunction
                                    |> List.gatherEqualsBy Tuple.first
                                    |> List.map (\( ( x, y ), rest ) -> String.fromInt x ++ " is in relation with " ++ String.fromInt (1 + List.length rest) ++ " elements: " ++ showPairList (( x, y ) :: rest))
                               )

                    else
                        []
                   )
                ++ (if not (Set.isEmpty info.emptyRowIndices) then
                        "It is not total, because there are some x ∈ X that are not in relation with any y ∈ X:"
                            :: (Set.toList info.emptyRowIndices
                                    |> List.map (\i -> String.fromInt i ++ " is not in relation with any element.")
                               )

                    else
                        []
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
spanningForest ((Rel rows) as rel) =
    dfsFromNodes (Rel rows) (domain rel)


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
genReflexiveRelation trueProb n =
    genRelation trueProb n
        -- TODO this way doesn't generate all reflexive relations with equal probability.
        -- We should set all diagonal entries to True and only generate the rest.
        |> Random.map reflexiveClosure


genIrreflexiveRelation : Float -> Int -> Random.Generator Rel
genIrreflexiveRelation trueProb n =
    genRelation trueProb n
        -- TODO this way doesn't generate all reflexive relations with equal probability.
        -- We should set all diagonal entries to True and only generate the rest.
        |> Random.map reflexiveReduction


genPartialFunction : Int -> Generator Rel
genPartialFunction n =
    -- Like genFunction except we geneerate 1 extra element (n) which
    -- leads to Array.initialize not setting any element in given row to True
    Random.int 0 n
        |> Random.map (\i -> Array.initialize n (\j -> j == i))
        |> Random.list n
        |> Random.map (Array.fromList >> Rel)


genFunction : Int -> Generator Rel
genFunction n =
    Random.int 0 (n - 1)
        |> Random.map (\i -> Array.initialize n (\j -> j == i))
        |> Random.list n
        |> Random.map (Array.fromList >> Rel)


genBijectiveFunction : Int -> Generator Rel
genBijectiveFunction n =
    Array.initialize n identity
        |> Array.shuffle
        |> Random.map
            (\shuffledArr ->
                Array.map (\indexOfTrue -> Array.initialize n (\i -> i == indexOfTrue)) shuffledArr
                    |> Rel
            )



-- VIEW


view : Config msg -> Rel -> Maybe Explanation -> Html msg
view config ((Rel rows) as rel) mExplanation =
    let
        addBgColor : Int -> Int -> List (Attribute msg) -> List (Attribute msg)
        addBgColor i j =
            case mExplanation of
                Just explanation ->
                    (::)
                        (A.style "background-color" <|
                            if Set.member ( i, j ) explanation.redHighlight then
                                "salmon"

                            else if Set.member ( i, j ) explanation.greenHighlight then
                                "lightgreen"

                            else
                                "white"
                        )

                Nothing ->
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
    elements >> showPairList


showPairSet : Set Pair -> String
showPairSet pairs =
    Set.toList pairs |> List.sort |> showPairList


showPairList : List Pair -> String
showPairList pairs =
    List.map showPair pairs
        |> String.join ", "
        |> (\s -> "{" ++ s ++ "}")


showIntList : List Int -> String
showIntList list =
    List.map String.fromInt list
        |> String.join ", "
        |> (\s -> "{" ++ s ++ "}")


showPair : Pair -> String
showPair ( i, j ) =
    "(" ++ String.fromInt i ++ "," ++ String.fromInt j ++ ")"


isAreElements : Int -> ( String, String )
isAreElements count =
    if count == 1 then
        ( "is", "one element" )

    else
        ( "are", String.fromInt count ++ " elements" )


toDotSource : Rel -> String
toDotSource rel =
    let
        nodeLines =
            domain rel
                |> List.map String.fromInt

        edgesLines =
            elements rel
                |> List.map (\( i, j ) -> String.fromInt i ++ "->" ++ String.fromInt j)
    in
    "digraph G {"
        ++ String.join ";" (nodeLines ++ edgesLines)
        ++ "}"



-- Internal helpers


pointwise : (Bool -> Bool -> Bool) -> Rel -> Rel -> Rel
pointwise op (Rel r1) (Rel r2) =
    Rel <| Array.map2 (Array.map2 op) r1 r2


unsafeGet : Int -> Int -> Array (Array Bool) -> Bool
unsafeGet i j rows =
    Array.get i rows
        |> Maybe.andThen (Array.get j)
        |> Maybe.withDefault False


arrayAnd : Array Bool -> Bool
arrayAnd =
    Array.foldl (&&) True


arrayOr : Array Bool -> Bool
arrayOr =
    Array.foldl (||) False



{- Unicode corner: ⇒ ∈ ∉ ⊆ ∀ ∃ ∧ ∨ ≠ -}
