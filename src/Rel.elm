module Rel exposing
    ( Config
    , Rel
    , complement
    , converse
    , empty
    , isAntisymmetric
    , isReflexive
    , isSymmetric
    , isTransitive
    , reflexiveClosure
    , resize
    , size
    , symmetricClosure
    , toggle
    , view
    )

import Array exposing (Array)
import Array.Extra as Array
import Html exposing (Html)
import Html.Events as E
import List



-- TODO generate random Rel
-- TODO a way to clean existing Rel


{-| A homogenous relation
-}
type Rel
    = Rel (Array (Array Bool))


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


union : Rel -> Rel -> Rel
union (Rel rowsA) (Rel rowsB) =
    Rel <| Array.map2 (Array.map2 (||)) rowsA rowsB


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



-- TODO maybe also return a list of cells that are missing for it to be reflexive


isReflexive : Rel -> Bool
isReflexive (Rel rows) =
    arrayAnd <|
        Array.indexedMap (\i row -> Array.get i row |> Maybe.withDefault False) rows


isSymmetric : Rel -> Bool
isSymmetric rel =
    rel == converse rel


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
                    -- If iRj with i /= j then jRi must not hold
                    |> List.all
                        (\j ->
                            if Maybe.withDefault False (Array.get j row) then
                                not <| unsafeGet j i rows

                            else
                                True
                        )
            )
            rows


isTransitive : Rel -> Bool
isTransitive rel =
    isSubsetOf (compose rel rel) rel



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



-- VIEW


view : Config msg -> Rel -> Html msg
view config (Rel rows) =
    let
        relSize =
            Array.length rows
    in
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
                                                Html.td [ E.onClick <| config.toggle i j ]
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



-- Internal helpers


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
