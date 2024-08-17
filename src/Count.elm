module Count exposing
    ( antisymmetric
    , asymmetric
    , connected
    , derangement
    , involution
    , lattice
    , leftTotal
    , partialFunctions
    , poset
    , reflexiveOrIrreflexive
    , rel
    , symmetric
    , totalOrderOrPermutation
    , transitive
    )

import Natural as Nat exposing (Natural)


{-| 2 ^ (n \* n)
<https://oeis.org/A002416>
-}
rel : Int -> Natural
rel n =
    let
        n2 =
            -- safe, for n <= 10
            Nat.fromSafeInt (n * n)
    in
    Nat.exp Nat.two n2


{-| 2 ^ (n \* (n - 1))
<https://oeis.org/A053763>
-}
reflexiveOrIrreflexive : Int -> Natural
reflexiveOrIrreflexive n =
    Nat.exp Nat.two (Nat.fromSafeInt (n * (n - 1)))


{-| 2 ^ n \* 3 ^ (n \* (n - 1) // 2)
<https://oeis.org/A083667>
-}
antisymmetric : Int -> Natural
antisymmetric n =
    let
        nn =
            Nat.fromSafeInt n

        diagOpposingCount =
            Nat.fromSafeInt (n * (n - 1) // 2)
    in
    Nat.mul
        -- Anything goes on diagonal
        (Nat.exp Nat.two nn)
        -- For each pair of diagonally opposing elems we can have 1) neither 2) under diag 3) above diag, but not both
        (Nat.exp Nat.three diagOpposingCount)


{-| 3 ^ (n \* (n - 1) // 2)
<https://oeis.org/A047656>
-}
asymmetric : Int -> Natural
asymmetric n =
    let
        diagOpposingCount =
            Nat.fromSafeInt (n * (n - 1) // 2)
    in
    -- For each pair of diagonally opposing elems we can have 1) neither 2) under diag 3) above diag, but not both
    Nat.exp Nat.three diagOpposingCount


connected : Int -> Natural
connected n =
    let
        nn =
            Nat.fromSafeInt n

        diagOpposingCount =
            Nat.fromSafeInt (n * (n - 1) // 2)
    in
    Nat.mul
        -- Anything goes on diagonal
        (Nat.exp Nat.two nn)
        -- For each pair of diagonally opposing elems we can have 1) under diag 2) over diag 3) both, but not neither
        (Nat.exp Nat.three diagOpposingCount)


{-| 2 ^ (n \* (n + 1) // 2)
NO OEIS
-}
symmetric : Int -> Natural
symmetric n =
    let
        nn =
            Nat.fromSafeInt (n * (n + 1) // 2)
    in
    Nat.exp Nat.two nn


{-| n!
<https://oeis.org/A000142>
-}
totalOrderOrPermutation : Int -> Natural
totalOrderOrPermutation n =
    List.range 1 n
        |> List.map Nat.fromSafeInt
        |> List.foldl Nat.mul Nat.one


{-| Recursive formula from Euler: a(n) = (n-1)\*(a(n-1) + a(n-2))
<https://oeis.org/A000166>
-}
derangement : Int -> Natural
derangement n =
    let
        deraHelp cnt nMinus2 nMinus1 =
            if cnt == n then
                nMinus2

            else
                deraHelp (cnt + 1) nMinus1 ((cnt + 1) * (nMinus1 + nMinus2))
    in
    -- safe, as derangement 10 = 1334961
    Nat.fromSafeInt <| deraHelp 1 0 1


{-| Recursive formula: a(n) = a(n-1) + (n-1)\*a(n-2)
<https://oeis.org/A000085>
-}
involution : Int -> Natural
involution n =
    let
        invoHelp cnt nMinus2 nMinus1 =
            if cnt == n then
                nMinus2

            else
                invoHelp (cnt + 1) nMinus1 (nMinus1 + (cnt + 1) * nMinus2)
    in
    -- safe, as involution 10 = 9496
    Nat.fromSafeInt <| invoHelp 1 1 2


{-| (n+1)^n
<https://oeis.org/A000169>
-}
partialFunctions : Int -> Natural
partialFunctions n =
    Nat.exp (Nat.fromSafeInt (n + 1)) (Nat.fromSafeInt n)


{-| (2 ^ n - 1) ^ n
<https://oeis.org/A055601>
-}
leftTotal : Int -> Natural
leftTotal n =
    Nat.exp (Nat.fromSafeInt (2 ^ n - 1)) (Nat.fromSafeInt n)


{-| No formula known
<https://oeis.org/A006905>
-}
transitive : Int -> Natural
transitive n =
    Nat.fromSafeInt <|
        case n of
            0 ->
                1

            1 ->
                2

            2 ->
                13

            3 ->
                171

            4 ->
                3994

            5 ->
                154303

            6 ->
                9415189

            7 ->
                878222530

            8 ->
                122207703623

            9 ->
                24890747921947

            --  safe, Nat.maxSafeInt = 9007199254740991
            10 ->
                7307450299510288

            _ ->
                0


{-| No formula known
<https://oeis.org/A006966>
-}
lattice : Int -> Natural
lattice n =
    Nat.fromSafeInt <|
        case n of
            0 ->
                1

            1 ->
                1

            2 ->
                1

            3 ->
                1

            4 ->
                2

            5 ->
                5

            6 ->
                15

            7 ->
                53

            8 ->
                222

            9 ->
                1078

            10 ->
                5994

            _ ->
                0


{-| No formula known
<https://oeis.org/A001035>
-}
poset : Int -> Natural
poset n =
    Nat.fromSafeInt <|
        case n of
            0 ->
                1

            1 ->
                1

            2 ->
                3

            3 ->
                19

            4 ->
                219

            5 ->
                4231

            6 ->
                130023

            7 ->
                6129859

            8 ->
                431723379

            9 ->
                44511042511

            10 ->
                6611065248783

            _ ->
                0
