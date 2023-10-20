module Rel exposing
    ( Config
    , Rel
    , empty
    , isAntisymmetric
    , isReflexive
    , isSymmetric
    , resize
    , size
    , toggle
    , view
    )

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (rows)
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


empty : Int -> Rel
empty n =
    Rel <| Array.repeat n <| Array.repeat n False


{-| Preserve already toggled cells of the original
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


transpose : Rel -> Rel
transpose (Rel rows) =
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


size : Rel -> Int
size (Rel rows) =
    Array.length rows


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
    Array.foldl (&&) True <|
        Array.indexedMap (\i row -> Array.get i row |> Maybe.withDefault False) rows


isSymmetric : Rel -> Bool
isSymmetric rel =
    rel == transpose rel


isAntisymmetric : Rel -> Bool
isAntisymmetric (Rel rows) =
    let
        maxIdx =
            Array.length rows - 1
    in
    Array.foldl (&&) True <|
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


unsafeGet : Int -> Int -> Array (Array Bool) -> Bool
unsafeGet i j rows =
    Array.get i rows
        |> Maybe.andThen (Array.get j)
        |> Maybe.withDefault False



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
