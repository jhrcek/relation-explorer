module Rel exposing
    ( Config
    , Pair
    , Rel
    , complement
    , converse
    , empty
    , genBijectiveFunction
    , genFunction
    , genReflexiveRelation
    , genRelation
    , isAntisymmetric
    , isAsymmetric
    , isBijectiveFunction
    , isConnected
    , isFunction
    , isIrreflexive
    , isReflexive
    , isSymmetric
    , isTransitive
    , missingForReflexivity
    , missingForSymmetry
    , reflexiveClosure
    , resize
    , showElements
    , showPairSet
    , size
    , superfluousForIrreflexivity
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
                                List.map2 (&&) (Array.toList rowi) (Array.toList colj)
                                    |> listOr
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


listOr : List Bool -> Bool
listOr =
    List.any identity


arrayAnd : Array Bool -> Bool
arrayAnd =
    Array.foldl (&&) True
