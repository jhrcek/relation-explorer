module Rel exposing
    ( Config
    , Pair
    , Rel
    , complement
    , converse
    , empty
    , findCycle
    , genBijectiveFunction
    , genFunction
    , genPartialFunction
    , genReflexiveRelation
    , genRelation
    , isAcyclic
    , isAntisymmetric
    , isAsymmetric
    , isBijectiveFunction
    , isConnected
    , isFunction
    , isIrreflexive
    , isPartialFunction
    , isReflexive
    , isSymmetric
    , isTransitive
    , missingForConnectedness
    , missingForReflexivity
    , missingForSymmetry
    , reflexiveClosure
    , resize
    , showElements
    , showPair
    , showPairList
    , showPairSet
    , size
    , superfluousAndMissingForFunction
    , superfluousForAntisymmetry
    , superfluousForAsymmetry
    , superfluousForIrreflexivity
    , superfluousForPartialFunction
    , symmetricClosure
    , toggle
    , transitiveClosure
    , view
    )

import Array exposing (Array)
import Array.Extra as Array
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import List
import Random exposing (Generator)
import Random.Array as Array
import Set exposing (Set)


{-| A homogenous relation
-}
type Rel
    = Rel (Array (Array Bool))


{-| An element of a relation.
For relation on set of size n, the are n^2 possible elements:
{(a,b) | a in {1..n}, b in {1..n}}
-}
type alias Pair =
    ( Int, Int )


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
isSubsetOf (Rel relA) (Rel relB) =
    listAnd <|
        List.map2
            (\rowA rowB ->
                listAnd <|
                    List.map2
                        -- Implication: whenever A is true, B must be as well
                        (\cellA cellB -> not cellA || cellB)
                        (Array.toList rowA)
                        (Array.toList rowB)
            )
            (Array.toList relA)
            (Array.toList relB)


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


{-| aRa
-}
isReflexive : Rel -> Bool
isReflexive (Rel rows) =
    arrayAnd <|
        -- All elements on the diagonal must be True
        Array.indexedMap (\i row -> Array.get i row |> Maybe.withDefault False) rows


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


{-| not aRa
-}
isIrreflexive : Rel -> Bool
isIrreflexive (Rel rows) =
    arrayAnd <|
        -- All elements on the diagonal must be False
        Array.indexedMap (\i row -> Array.get i row |> Maybe.withDefault False |> not) rows


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


{-| aRb => bRa
-}
isSymmetric : Rel -> Bool
isSymmetric rel =
    rel == converse rel


{-| Set of pairs that would have to be added for the relation to be symmetric
-}
missingForSymmetry : Rel -> Set Pair
missingForSymmetry rel =
    difference (pointwise xor rel (converse rel)) rel
        |> elements
        |> Set.fromList


{-| Set of pairs that are problematic.
-- TODO this is not satisfactory, because it doesn't mean that all "problematic" elements must be removed
-- to get antisymetry. Only one of each "mirror image" pair would be enough to remove to get antisymmetry.
-- It would be nice to "pair up" the pairs to have "mirror images" that cause trouble for antisymmetry.
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


superfluousForAsymmetry : Rel -> ( Set Pair, Set Pair )
superfluousForAsymmetry rel =
    intersection rel (converse rel)
        |> elements
        |> Set.fromList
        |> Set.partition (\( i, j ) -> i == j)


{-| aRb and bRa => a == b
Or equivalently: a/=b and aRb => not bRa
-}
isAntisymmetric : Rel -> Bool
isAntisymmetric (Rel rows) =
    let
        maxIdx =
            Array.length rows - 1
    in
    arrayAnd <|
        Array.indexedMap
            (\i row ->
                List.range (i + 1) maxIdx
                    |> List.all
                        (\j ->
                            if Maybe.withDefault False (Array.get j row) then
                                not <| unsafeGet j i rows

                            else
                                True
                        )
            )
            rows


{-| aRb => not bRa
-}
isAsymmetric : Rel -> Bool
isAsymmetric rel =
    isAntisymmetric rel && isIrreflexive rel


isTransitive : Rel -> Bool
isTransitive rel =
    isSubsetOf (compose rel rel) rel


{-| a/=b => aRb or bRa
-}
isConnected : Rel -> Bool
isConnected (Rel rows) =
    let
        maxIdx =
            Array.length rows - 1
    in
    arrayAnd <|
        Array.indexedMap
            (\i row ->
                List.range (i + 1) maxIdx
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
    let
        ( relTran, _ ) =
            transitiveClosure rel
    in
    isAntisymmetric relTran


findCycle : Rel -> Maybe (List Int)
findCycle (Rel rows) =
    let
        n =
            Array.length rows

        adjacentNodes : Int -> List Int
        adjacentNodes i =
            Array.get i rows
                |> Maybe.withDefault Array.empty
                |> Array.toIndexedList
                |> List.filter Tuple.second
                |> List.map Tuple.first
                |> List.filter (\j -> i /= j)

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
                                    Just (takeWhile ((/=) i) path ++ [ i ])

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
    List.range 0 (n - 1)
        |> List.filterMap dfs
        |> List.head
        |> Maybe.map List.reverse


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


isFunction : Rel -> Bool
isFunction (Rel rows) =
    arrayAnd <|
        Array.map
            (\row ->
                1
                    -- There's exactly one element in each row
                    == Array.foldl
                        (\elem elemCount ->
                            if elem then
                                elemCount + 1

                            else
                                elemCount
                        )
                        0
                        row
            )
            rows


superfluousAndMissingForFunction : Rel -> ( Set Pair, Set Pair )
superfluousAndMissingForFunction ((Rel rows) as rel) =
    Array.toIndexedList rows
        |> List.foldl
            (\( i, row ) ( sup, mis ) ->
                case Array.toIndexedList row |> List.filter Tuple.second |> List.map Tuple.first of
                    [] ->
                        -- mark whole row of pairs as missing
                        ( sup
                        , List.range 0 (size rel)
                            |> List.map (Tuple.pair i)
                            |> Set.fromList
                            |> Set.union mis
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


isBijectiveFunction : Rel -> Bool
isBijectiveFunction rel =
    isFunction rel && isFunction (converse rel)



-- CLOSURES


{-| <https://en.wikipedia.org/wiki/Reflexive_closure>
-}
reflexiveClosure : Rel -> Rel
reflexiveClosure (Rel rows) =
    Rel <|
        Array.indexedMap
            (\i row ->
                Array.indexedMap
                    (\j cell ->
                        if i == j then
                            True

                        else
                            cell
                    )
                    row
            )
            rows


{-| <https://en.wikipedia.org/wiki/Symmetric_closure>
-}
symmetricClosure : Rel -> Rel
symmetricClosure rel =
    union rel (converse rel)


{-| <https://en.wikipedia.org/wiki/Transitive_closure>

Returns a non-empty list of relations.
The last one is the input, each successive one is the composition of the previous relation with the original relation.
The first one is the transitive closure.

-}
transitiveClosure : Rel -> ( Rel, List Rel )
transitiveClosure rel =
    let
        -- TODO this is brute force. See if there's more efficient way
        transitiveHelp ( r, history ) =
            if isTransitive r then
                ( r, history )

            else
                transitiveHelp ( union r (compose r rel), r :: history )
    in
    transitiveHelp ( rel, [] )



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


view : Config msg -> Rel -> Set Pair -> Html msg
view config (Rel rows) highlight =
    let
        relSize =
            Array.length rows
    in
    -- TODO it's not clear that cells should be clicked
    -- Need something like "click cells to toggle them / add/remove elements to/from relation"
    Html.table []
        [ Html.thead []
            [ Html.tr [] <|
                Html.th [] [{- empty top-left corner -}]
                    :: List.map headerCell (List.range 1 relSize)
            ]
        , Html.tbody [] <|
            Array.toList <|
                Array.indexedMap
                    (\i row ->
                        Html.tr [] <|
                            headerCell (i + 1)
                                :: (Array.toList <|
                                        Array.indexedMap
                                            (\j cell ->
                                                Html.td
                                                    [ E.onClick <| config.toggle i j
                                                    , A.style "background-color" <|
                                                        if Set.member ( i, j ) highlight then
                                                            "tomato"

                                                        else
                                                            "white"
                                                    ]
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
    pairs
        |> List.map showPair
        |> String.join ", "
        |> (\s -> "{" ++ s ++ "}")


{-| Internally pairs are 0-based for easier array indexing.
But we're showing them as 1-based to the user.
-}
showPair : Pair -> String
showPair ( i, j ) =
    "(" ++ String.fromInt (i + 1) ++ "," ++ String.fromInt (j + 1) ++ ")"



-- Internal helpers


pointwise : (Bool -> Bool -> Bool) -> Rel -> Rel -> Rel
pointwise op (Rel r1) (Rel r2) =
    Rel <| Array.map2 (Array.map2 op) r1 r2


unsafeGet : Int -> Int -> Array (Array Bool) -> Bool
unsafeGet i j rows =
    Array.get i rows
        |> Maybe.andThen (Array.get j)
        |> Maybe.withDefault False


listAnd : List Bool -> Bool
listAnd =
    List.all identity


arrayAnd : Array Bool -> Bool
arrayAnd =
    Array.foldl (&&) True


arrayOr : Array Bool -> Bool
arrayOr =
    Array.foldl (||) False


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate =
    let
        takeWhileMemo memo list =
            case list of
                [] ->
                    List.reverse memo

                x :: xs ->
                    if predicate x then
                        takeWhileMemo (x :: memo) xs

                    else
                        List.reverse memo
    in
    takeWhileMemo []
