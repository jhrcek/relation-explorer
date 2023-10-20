module Rel exposing
    ( Config
    , Rel
    , empty
    , resize
    , size
    , toggle
    , view
    )

import Array exposing (Array)
import Html exposing (Html)
import Html.Events as E



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


resize : Int -> Rel -> Rel
resize n (Rel _) =
    -- TODO make is so that stuff is copied from the original rel
    empty n


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
                    :: List.map
                        (\i -> Html.th [{- col header -}] [ Html.text <| String.fromInt i ])
                        (List.range 1 relSize)
            ]
        , Html.tbody [] <|
            Array.toList <|
                Array.indexedMap
                    (\i row ->
                        Html.tr [] <|
                            Html.th [{- row header -}] [ Html.text <| String.fromInt <| i + 1 ]
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
