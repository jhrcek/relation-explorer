module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Random
import Rel exposing (Rel)
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { rel : Rel
    , explanation : Maybe Explanation

    -- Number between 0 and 1, indicating how likely it is that random generators
    -- will produce a True value (and thus generate an element of a relation)
    , trueProb : Float
    }


type alias Explanation =
    { -- TODO use at least 2 different colors: one for missing, one for superfluous
      highlight : Set Rel.Pair
    , textLines : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initSize =
            4
    in
    ( { rel = Rel.empty initSize
      , explanation = Nothing
      , trueProb = 0.2
      }
    , Cmd.none
    )


type Msg
    = SetRelSize Int
    | SetTrueProb Float
    | ToggleRel Int Int
    | DoReflexiveClosure
    | DoSymmetricClosure
    | DoTransitiveClosure
    | DoComplement
    | DoConverse
    | MakeEmpty
      -- Random generation
    | GenRel
    | GenReflexive
    | GenPartialFunction
    | GenFunction
    | GenBijectiveFunction
    | GotRandom Rel
      -- Explanations
    | HideExplanations
    | ExplainWhyNotReflexive
    | ExplainWhyNotIrreflexive
    | ExplainWhyNotSymmetric
    | ExplainWhyNotAntisymmetric
    | ExplainWhyNotAsymmetric
    | ExplainWhyNotConnected
    | ExplainWhyNotPartialFunction
    | ExplainWhyNotFunction
    | ExplainWhyNotAcyclic
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRelSize newSize ->
            let
                safeSize =
                    clamp 1 10 newSize
            in
            pure { model | rel = Rel.resize safeSize model.rel }

        SetTrueProb newProb ->
            let
                safeProb =
                    clamp 0.0 1.0 newProb
            in
            pure { model | trueProb = safeProb }

        ToggleRel i j ->
            pure { model | rel = Rel.toggle i j model.rel }

        DoReflexiveClosure ->
            pure { model | rel = Rel.reflexiveClosure model.rel }

        DoSymmetricClosure ->
            pure { model | rel = Rel.symmetricClosure model.rel }

        DoTransitiveClosure ->
            let
                ( transitiveRel, _ ) =
                    Rel.transitiveClosure model.rel
            in
            pure { model | rel = transitiveRel }

        DoComplement ->
            pure { model | rel = Rel.complement model.rel }

        DoConverse ->
            pure { model | rel = Rel.converse model.rel }

        MakeEmpty ->
            pure { model | rel = Rel.empty <| Rel.size model.rel }

        GenRel ->
            ( model
            , Random.generate GotRandom <|
                Rel.genRelation model.trueProb (Rel.size model.rel)
            )

        GenReflexive ->
            ( model
            , Random.generate GotRandom <|
                Rel.genReflexiveRelation model.trueProb (Rel.size model.rel)
            )

        GenPartialFunction ->
            ( model
            , Random.generate GotRandom <|
                Rel.genPartialFunction (Rel.size model.rel)
            )

        GenFunction ->
            ( model
            , Random.generate GotRandom <|
                Rel.genFunction (Rel.size model.rel)
            )

        GenBijectiveFunction ->
            ( model
            , Random.generate GotRandom <|
                Rel.genBijectiveFunction (Rel.size model.rel)
            )

        GotRandom rel ->
            pure { model | rel = rel }

        HideExplanations ->
            pure { model | explanation = Nothing }

        ExplainWhyNotReflexive ->
            let
                missing =
                    Rel.missingForReflexivity model.rel
            in
            pure
                { model
                    | explanation =
                        Just
                            { highlight = missing
                            , textLines =
                                [ "This relation is not reflexive."
                                , "To be reflexive it would need to conain all elements of the form (x,x)."
                                , "But the following elements are missing: " ++ Rel.showPairSet missing
                                ]
                            }
                }

        ExplainWhyNotIrreflexive ->
            let
                superfluous =
                    Rel.superfluousForIrreflexivity model.rel
            in
            pure
                { model
                    | explanation =
                        Just
                            { highlight = superfluous
                            , textLines =
                                [ "This relation is not irreflexive."
                                , "To make it irreflexive we would have to remove all elements of the form (x,x)."
                                , "The following elements would have to be removed: " ++ Rel.showPairSet superfluous
                                ]
                            }
                }

        ExplainWhyNotSymmetric ->
            let
                missing =
                    Rel.missingForSymmetry model.rel
            in
            pure
                { model
                    | explanation =
                        Just
                            { highlight = missing
                            , textLines =
                                [ "This relation is not symmetric."
                                , "To be symmetric, the relation has to contain element (y,x) whenever it contains an element (x,y)."

                                -- TODO I'd like the explanation to be more granular, to make it clearer.
                                -- something like: "since we have (1,2), we alson need (2,1) ..."
                                -- TODO also it would be nice to highlight the potentially "superfluous" elements
                                , "The following elements would have to be added to satisfy that condition: " ++ Rel.showPairSet missing
                                ]
                            }
                }

        ExplainWhyNotAntisymmetric ->
            let
                superfluous =
                    Rel.superfluousForAntisymmetry model.rel

                problematicPairs =
                    renderMirrorImagePairs superfluous
            in
            pure
                { model
                    | explanation =
                        Just
                            { highlight = superfluous
                            , textLines =
                                [ "This relation is not antisymmetric."
                                , "To be antisymmetric, the relation must not contain both (a,b) and (b,a), such that a ≠ b."

                                -- TODO pluralize properly based on the number of pairs
                                , "But this relation contains such pair(s): " ++ problematicPairs
                                , "One (or both) of each such pair would have to be removed to make the relation antisymmetric."
                                ]
                            }
                }

        ExplainWhyNotAsymmetric ->
            let
                ( diagonalPairs, offDiagonalPairs ) =
                    Rel.superfluousForAsymmetry model.rel
            in
            pure
                { model
                    | explanation =
                        Just
                            { highlight = Set.union offDiagonalPairs diagonalPairs
                            , textLines =
                                [ "This relation is not asymmetric."
                                , "To be asymmetric, the relation must not contain (b,a) when it contains (a,b)."

                                -- TODO pluralize properly based on the number of pairs
                                -- TODO also either of these could be empty, in which case the respective sentence should be omitted
                                , "To make it asymmetric we would have to remove the following pairs of the form (a,a): " ++ Rel.showPairSet diagonalPairs
                                , "Moreover we'd also have to remove at least of each of the following pairs: " ++ renderMirrorImagePairs offDiagonalPairs
                                ]
                            }
                }

        ExplainWhyNotConnected ->
            let
                missing =
                    Rel.missingForConnectedness model.rel

                problematicPairs =
                    renderMirrorImagePairs missing
            in
            pure
                { model
                    | explanation =
                        Just
                            { highlight = missing
                            , textLines =
                                [ "This relation is not connected."
                                , "To be connected, it would have to contain at least one of the elements (a,b) or (b,a), for each a ≠ b."

                                -- TODO pluralize properly based on the number of pairs
                                , "These are the problematic pair(s): " ++ problematicPairs
                                , "We would have to add at least one of each such pair to make the relation connected."
                                ]
                            }
                }

        ExplainWhyNotPartialFunction ->
            let
                superfluous =
                    Rel.superfluousForPartialFunction model.rel
            in
            pure
                { model
                    | explanation =
                        Just
                            { highlight = superfluous
                            , textLines =
                                [ "This relation is not a partial function."
                                , "To be partial function there should be at most one pair (a,b) for each a ∈ X."

                                -- TODO pluralize properly
                                , "But some a's we have multiple pairs(s): " ++ Rel.showPairSet superfluous
                                , "We would have to remove some of these so that each row has at most one pair."
                                ]
                            }
                }

        ExplainWhyNotFunction ->
            let
                -- TODO candidate for color distinction between superfluous and missing
                ( superfluous, missing ) =
                    Rel.superfluousAndMissingForFunction model.rel
            in
            pure
                { model
                    | explanation =
                        Just
                            { highlight = Set.union superfluous missing
                            , textLines =
                                [ "This relation is not a function."
                                , "To be a function there should be exactly one pair (a,b) for each a ∈ X."

                                -- TODO pluralize properly
                                , if Set.size superfluous > 0 then
                                    "But in some row sthere's multipe pairs for one a: " ++ Rel.showPairSet superfluous

                                  else
                                    ""
                                , let
                                    rowIndicesWithMissingPairs =
                                        Set.map Tuple.first missing
                                  in
                                  if Set.size rowIndicesWithMissingPairs > 0 then
                                    "In some rows there's no pair, namely for a ∈ {"
                                        ++ String.join ", "
                                            (List.map (\i -> String.fromInt (i + 1)) <| Set.toList rowIndicesWithMissingPairs)
                                        ++ "}"

                                  else
                                    ""
                                , "We would have to remove some of these so that each row has at most one pair."
                                ]
                            }
                }

        ExplainWhyNotAcyclic ->
            let
                elemsToPairs xs =
                    case xs of
                        [] ->
                            []

                        fst :: rest ->
                            List.map2 Tuple.pair xs (rest ++ [ fst ])
            in
            pure
                { model
                    | explanation =
                        Maybe.map
                            (\cycleElems ->
                                let
                                    cyclePairs =
                                        elemsToPairs cycleElems
                                in
                                { highlight = Set.fromList cyclePairs
                                , textLines =
                                    [ "This relation is not acyclic."
                                    , "These the following pairs form a cycle: " ++ Rel.showPairList cyclePairs
                                    , "so the cycle consists of these elements: {"
                                        ++ String.join ", "
                                            (List.map (String.fromInt << (+) 1) cycleElems)
                                        ++ "}"
                                    ]
                                }
                            )
                        <|
                            Rel.findCycle model.rel
                }

        NoOp ->
            pure model


{-| select subset of pairs above diagonal and render them as pairs of pairs connected by "<->"
-}
renderMirrorImagePairs : Set Rel.Pair -> String
renderMirrorImagePairs pairs =
    Set.toList pairs
        |> List.filter (\( a, b ) -> a > b)
        |> List.map (\( a, b ) -> Rel.showPair ( a, b ) ++ " <-> " ++ Rel.showPair ( b, a ))
        |> String.join ", "


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


relConfig : Rel.Config Msg
relConfig =
    { toggle = ToggleRel }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.node "style" [] [ Html.text style ]
        , Html.div [ A.id "top-container" ]
            [ Html.div [ A.id "rel-and-controls" ]
                [ sizeInputView model.rel
                , let
                    highlight =
                        case model.explanation of
                            Nothing ->
                                Set.empty

                            Just exp ->
                                exp.highlight
                  in
                  Rel.view relConfig model.rel highlight
                , elementaryPropertiesView model.rel
                , miscControls model.trueProb
                ]
            , Html.div [ A.id "explanation" ]
                [ Html.div []
                    [ Html.div []
                        [ Html.text <|
                            "X = {"
                                ++ (String.join ", " <| List.map String.fromInt <| List.range 1 (Rel.size model.rel))
                                ++ "}"
                        ]
                    , Html.div []
                        [ Html.text <|
                            "R ⊆ X ⨯ X, R = "
                                ++ Rel.showElements model.rel
                        ]
                    , case model.explanation of
                        Just exp ->
                            Html.div [] <| List.map (\line -> Html.div [] [ Html.text line ]) exp.textLines

                        Nothing ->
                            Html.text ""
                    ]
                ]
            ]
        ]


type alias PropertyConfig msg =
    { propertyName : String
    , wikiLink : String
    , hasProperty : Rel -> Bool
    , closureButton : Maybe msg
    , genRandom : Maybe msg
    , onHoverExplanation : Maybe msg
    }


propertyConfigs : List (PropertyConfig Msg)
propertyConfigs =
    [ { propertyName = "Relation"
      , wikiLink = "https://en.wikipedia.org/wiki/Relation_(mathematics)"
      , hasProperty = always True
      , closureButton = Nothing
      , genRandom = Just GenRel
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Reflexive"
      , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation"
      , hasProperty = Rel.isReflexive
      , closureButton = Just DoReflexiveClosure
      , genRandom = Just GenReflexive
      , onHoverExplanation = Just ExplainWhyNotReflexive
      }
    , { propertyName = "Irreflexive"
      , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation#Irreflexivity"
      , hasProperty = Rel.isIrreflexive
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainWhyNotIrreflexive
      }
    , { propertyName = "Symmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Symmetric_relation"
      , hasProperty = Rel.isSymmetric
      , closureButton = Just DoSymmetricClosure
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainWhyNotSymmetric
      }
    , { propertyName = "Antisymmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Antisymmetric_relation"
      , hasProperty = Rel.isAntisymmetric
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainWhyNotAntisymmetric
      }
    , { propertyName = "Asymmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Asymmetric_relation"
      , hasProperty = Rel.isAsymmetric
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainWhyNotAsymmetric
      }
    , { propertyName = "Transitive"
      , wikiLink = "https://en.wikipedia.org/wiki/Transitive_relation"
      , hasProperty = Rel.isTransitive
      , closureButton = Just DoTransitiveClosure
      , genRandom = Nothing

      -- TODO explain why not transitive
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Connected"
      , wikiLink = "https://en.wikipedia.org/wiki/Connected_relation"
      , hasProperty = Rel.isConnected
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainWhyNotConnected
      }
    , { propertyName = "Acyclic"
      , wikiLink = "https://en.wikipedia.org/wiki/Glossary_of_order_theory#A"
      , hasProperty = Rel.isAcyclic
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainWhyNotAcyclic
      }
    , { propertyName = "Partial function"
      , wikiLink = "https://en.wikipedia.org/wiki/Partial_function"
      , hasProperty = Rel.isPartialFunction
      , closureButton = Nothing
      , genRandom = Just GenPartialFunction
      , onHoverExplanation = Just ExplainWhyNotPartialFunction
      }
    , { propertyName = "Function"
      , wikiLink = "https://en.wikipedia.org/wiki/Function_(mathematics)"
      , hasProperty = Rel.isFunction
      , closureButton = Nothing
      , genRandom = Just GenFunction
      , onHoverExplanation = Just ExplainWhyNotFunction
      }
    , { propertyName = "Bijection"
      , wikiLink = "https://en.wikipedia.org/wiki/Bijection"
      , hasProperty = Rel.isBijectiveFunction
      , closureButton = Nothing
      , genRandom = Just GenBijectiveFunction

      -- TODO explain why not bijective function
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Involution"
      , wikiLink = "https://en.wikipedia.org/wiki/Involution_(mathematics)"
      , hasProperty = Rel.isInvolution
      , closureButton = Nothing
      , genRandom = Nothing

      -- TODO explain why not involution
      , onHoverExplanation = Nothing
      }
    ]


elementaryPropertiesView : Rel -> Html Msg
elementaryPropertiesView rel =
    let
        row : PropertyConfig Msg -> Html Msg
        row { propertyName, wikiLink, hasProperty, closureButton, genRandom, onHoverExplanation } =
            let
                hasProp =
                    hasProperty rel
            in
            Html.tr []
                [ Html.td [] [ blankLink wikiLink propertyName ]
                , Html.td [] [ yesNo (Maybe.withDefault NoOp onHoverExplanation) hasProp ]
                , Html.td [] <|
                    case closureButton of
                        Just closureMsg ->
                            [ Html.button [ E.onClick closureMsg, A.disabled hasProp ] [ Html.text "Closure" ] ]

                        Nothing ->
                            []
                , Html.td [] <|
                    case genRandom of
                        Just genMsg ->
                            [ Html.button [ E.onClick genMsg ] [ Html.text "⚄" ] ]

                        Nothing ->
                            []
                ]
    in
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Property name" ]
                , Html.th [] [ Html.text "Does R have this property?" ]
                , Html.th [] [ Html.text "Closure" ]
                , Html.th [] [ Html.text "Generate" ]
                ]
            ]
        , Html.tbody [] <| List.map row propertyConfigs
        ]


blankLink : String -> String -> Html msg
blankLink href text =
    Html.a [ A.href href, A.target "_blank" ] [ Html.text text ]


yesNo : Msg -> Bool -> Html Msg
yesNo onHover b =
    if b then
        Html.text "Yes"

    else
        Html.span
            [ E.onMouseEnter onHover
            , E.onMouseLeave HideExplanations
            ]
            [ Html.text "No - ⓘ" ]


sizeInputView : Rel -> Html Msg
sizeInputView rel =
    let
        relSize =
            Rel.size rel
    in
    Html.div []
        [ Html.label []
            [ Html.span [ A.id "size-label" ]
                [ Html.text <| "|X| = " ++ String.fromInt relSize ]
            , Html.input
                [ A.type_ "range"
                , A.min "1"
                , A.max "10"
                , E.onInput <| SetRelSize << Maybe.withDefault 3 << String.toInt
                , A.value <| String.fromInt relSize
                ]
                []
            ]
        ]


setTrueProbView : Float -> Html Msg
setTrueProbView trueProb =
    Html.div []
        [ Html.label []
            [ Html.span [ A.id "size-label" ]
                [ Html.text <| "P(True) = " ++ String.fromFloat trueProb ]
            , Html.input
                [ A.type_ "range"
                , A.min "0"
                , A.max "1"
                , A.step "0.01"
                , E.onInput <| SetTrueProb << Maybe.withDefault 0.5 << String.toFloat
                , A.value <| String.fromFloat trueProb
                ]
                []
            ]
        ]


miscControls : Float -> Html Msg
miscControls trueProb =
    Html.div []
        [ Html.h4 [] [ Html.text "Operations" ]
        , -- TODO think about how to decompose this hodpodge of controls
          Html.button [ E.onClick MakeEmpty, A.title "Empty relation" ] [ Html.text "∅" ]
        , Html.button [ E.onClick DoComplement ] [ Html.text "Complement" ]
        , Html.button [ E.onClick DoConverse ] [ Html.text "Converse" ]
        , setTrueProbView trueProb
        ]


style : String
style =
    """
table {
    border-collapse: collapse;
}

th, td {
    border: 1px solid black;
    width: 20px;
    height: 20px;
    padding: 0;
    line-height: 20px; /* This ensures the text is vertically centered */
    text-align: center;
}

thead th {
    background-color: #f2f2f2;
}

a {
    text-decoration: none;
    color: sienna;
}

a:hover {
    text-decoration: underline;
}

a:visited {
    color: sienna;
}

#size-label {
    display: inline-block;
    width: 60px;
    text-align: right;
}

#top-container {
    display: flex;
    gap: 20px;
    padding: 20px;

    #explanation {
        max-width: 40%;
    }
}
#rel {
    padding-top: 20px;
    height: 250px;
    width: 250px;
}
"""
