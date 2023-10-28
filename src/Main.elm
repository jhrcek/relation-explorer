module Main exposing (main)

import Array
import Html exposing (Html)


main : Html a
main =
    let
        rows =
            Array.empty

        adjacentNodes : Int -> List Int
        adjacentNodes i =
            Array.get i rows
                |> Maybe.withDefault Array.empty
                |> (Array.foldr
                        (\bool ( j, acc ) ->
                            ( j - 1
                            , if bool && i /= j then
                                j :: acc

                              else
                                acc
                            )
                        )
                        ( 0, [] )
                        >> Tuple.second
                   )
    in
    Html.text "reproducer"
