module BitSet exposing
    ( BitSet
    , empty
    , fromSet
    , isSubset
    , showInt
    , showSet
    )

import Bitwise as B
import Set exposing (Set)


{-| Efficient representation of set of numbers numbers from 0 to 31 (inclusive)
-}
type BitSet
    = BitSet Int


empty : BitSet
empty =
    BitSet 0


maxElement : Int
maxElement =
    31


showSet : Bool -> BitSet -> String
showSet escapeCurly (BitSet set) =
    let
        go : Int -> Int -> List String -> List String
        go i acc resultStack =
            if i <= 0 then
                List.reverse resultStack

            else
                go (i // 2)
                    (acc + 1)
                    (if modBy 2 i == 1 then
                        String.fromInt acc :: resultStack

                     else
                        resultStack
                    )
    in
    case go set 0 [] of
        [] ->
            "∅"

        nonEmpty ->
            let
                elems =
                    String.join "," nonEmpty
            in
            -- This is needed because I'm using graphviz shape=record, which
            -- uses {} to render things vertically within a node.
            if escapeCurly then
                "\\{" ++ elems ++ "\\}"

            else
                "{" ++ elems ++ "}"


showInt : BitSet -> String
showInt (BitSet set) =
    String.fromInt set


fromSet : Set Int -> BitSet
fromSet set =
    BitSet <|
        Set.foldl
            (\i acc ->
                if 0 <= i && i <= maxElement then
                    acc + 2 ^ i

                else
                    acc
            )
            0
            set


{-| isSubset a b <=> a is subset of b
-}
isSubset : BitSet -> BitSet -> Bool
isSubset (BitSet a) (BitSet b) =
    B.and a b == a



{-
   intersect : BitSet -> }BitSet -> BitSet
   intersect (BitSet a) (BitSet b) =
       BitSet (B.and a b)


   union : BitSet -> BitSet -> BitSet
   union (BitSet a) (BitSet b) =
       BitSet (B.or a b)


   member : Int -> BitSet -> Bool
   member i (BitSet set) =
       if i > maxElement then
           False

       else
           B.and set (2 ^ i) /= 0
-}
