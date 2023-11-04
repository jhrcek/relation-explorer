module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Ports
import Random exposing (Generator)
import Rel exposing (DerivedInfo, Explanation, Rel)
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
    , history : List Rel
    , derivedInfo : DerivedInfo
    , explanation : Maybe Explanation

    -- Number between 0 and 1, indicating how likely it is that random generators
    -- will produce a True value (and thus generate an element of a relation)
    , trueProb : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initSize =
            4

        initRel =
            Rel.empty initSize
    in
    ( { rel = initRel
      , history = []
      , derivedInfo = Rel.deriveInfo initRel
      , explanation = Nothing
      , trueProb = 0.2
      }
    , renderGraph initRel
    )


renderGraph : Rel -> Cmd msg
renderGraph =
    Rel.toDotSource >> Ports.renderDot


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
    | ExplainReflexive
    | ExplainIrreflexive
    | ExplainSymmetric
    | ExplainAntisymmetric
    | ExplainAsymmetric
    | ExplainTransitive
    | ExplainWhyNotConnected
    | ExplainWhyNotPartialFunction
    | ExplainFunction
    | ExplainWhyNotAcyclic
    | UndoHistory
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTrueProb newProb ->
            let
                safeProb =
                    clamp 0.0 1.0 newProb
            in
            pure { model | trueProb = safeProb }

        SetRelSize newSize ->
            let
                safeSize =
                    clamp 1 10 newSize
            in
            updateRel (Rel.resize safeSize) model

        ToggleRel i j ->
            updateRel (Rel.toggle i j) model

        DoReflexiveClosure ->
            updateRel Rel.reflexiveClosure model

        DoSymmetricClosure ->
            updateRel Rel.symmetricClosure model

        DoTransitiveClosure ->
            updateRel Rel.transitiveClosure model

        DoComplement ->
            updateRel Rel.complement model

        DoConverse ->
            updateRel Rel.converse model

        MakeEmpty ->
            updateRel (Rel.empty << Rel.size) model

        GotRandom rel ->
            updateRel (always rel) model

        GenRel ->
            generateRel (Rel.genRelation model.trueProb) model

        GenReflexive ->
            generateRel (Rel.genReflexiveRelation model.trueProb) model

        GenPartialFunction ->
            generateRel Rel.genPartialFunction model

        GenFunction ->
            generateRel Rel.genFunction model

        GenBijectiveFunction ->
            generateRel Rel.genBijectiveFunction model

        HideExplanations ->
            pure { model | explanation = Nothing }

        ExplainReflexive ->
            pure { model | explanation = Just <| Rel.explainReflexive model.derivedInfo }

        ExplainIrreflexive ->
            pure { model | explanation = Just <| Rel.explainIrreflexive model.derivedInfo }

        ExplainSymmetric ->
            pure { model | explanation = Just <| Rel.explainSymmetric model.derivedInfo }

        ExplainAntisymmetric ->
            pure { model | explanation = Just <| Rel.explainAntisymmetric model.derivedInfo }

        ExplainAsymmetric ->
            pure { model | explanation = Just <| Rel.explainAsymmetric model.derivedInfo }

        ExplainTransitive ->
            pure { model | explanation = Just <| Rel.explainTransitive model.derivedInfo }

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
                            { redHighlight = missing
                            , greenHighlight = Set.empty
                            , lines =
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
                            { redHighlight = superfluous
                            , greenHighlight = Set.empty
                            , lines =
                                [ "This relation is not a partial function."
                                , "To be partial function there should be at most one pair (a,b) for each a ∈ X."

                                -- TODO pluralize properly
                                , "But some a's we have multiple pairs(s): " ++ Rel.showPairSet superfluous
                                , "We would have to remove some of these so that each row has at most one pair."
                                ]
                            }
                }

        ExplainFunction ->
            pure { model | explanation = Just <| Rel.explainFunction model.derivedInfo }

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
                                { redHighlight = Set.fromList cyclePairs
                                , greenHighlight = Set.empty
                                , lines =
                                    [ "This relation is not acyclic."
                                    , "The following pairs form a cycle: " ++ Rel.showPairList cyclePairs
                                    , "so the cycle consists of these elements: " ++ Rel.showIntList cycleElems
                                    ]
                                }
                            )
                        <|
                            Rel.findCycle model.rel
                }

        UndoHistory ->
            undoHistory model

        NoOp ->
            pure model


updateRel : (Rel -> Rel) -> Model -> ( Model, Cmd Msg )
updateRel f model =
    let
        newRel =
            f model.rel
    in
    ( { model
        | rel = newRel
        , history = model.rel :: model.history
        , derivedInfo = Rel.deriveInfo newRel
      }
    , renderGraph newRel
    )


undoHistory : Model -> ( Model, Cmd Msg )
undoHistory model =
    case model.history of
        [] ->
            ( model, Cmd.none )

        prevRel :: rest ->
            ( { model
                | rel = prevRel
                , history = rest
                , derivedInfo = Rel.deriveInfo prevRel
              }
            , renderGraph prevRel
            )


generateRel : (Int -> Generator Rel) -> Model -> ( Model, Cmd Msg )
generateRel gen model =
    ( model
    , Random.generate GotRandom <| gen model.derivedInfo.relSize
    )


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


relConfig : Rel.Config Msg
relConfig =
    { toggle = ToggleRel }


{-| select subset of pairs above diagonal and render them as pairs of pairs connected by "<->"
-}
renderMirrorImagePairs : Set Rel.Pair -> String
renderMirrorImagePairs pairs =
    Set.toList pairs
        |> List.filter (\( a, b ) -> a > b)
        |> List.map (\( a, b ) -> Rel.showPair ( a, b ) ++ " <-> " ++ Rel.showPair ( b, a ))
        |> String.join ", "


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ A.id "top-container" ]
            [ Html.div [ A.id "rel-and-controls" ]
                [ sizeInputView model.derivedInfo.relSize
                , Rel.view relConfig model.rel model.explanation
                , elementaryPropertiesView model.derivedInfo
                , miscControls model.trueProb
                ]
            , Html.div [ A.id "explanation" ]
                [ Html.div []
                    [ Html.div []
                        [ Html.text <| "X = " ++ Rel.showIntList (Rel.domain model.rel)
                        ]
                    , Html.div []
                        [ Html.text <| "R ⊆ X ⨯ X, R = " ++ Rel.showElements model.rel
                        ]
                    , Html.div []
                        [ Html.text <|
                            "Strongly connected components: "
                                ++ String.join ", "
                                    (List.map Rel.showIntList <|
                                        Rel.scc model.rel
                                    )
                        ]
                    , case model.explanation of
                        Just exp ->
                            Html.div [] <| List.map (\line -> Html.div [] [ Html.text line ]) exp.lines

                        Nothing ->
                            Html.text ""
                    ]
                ]
            , Html.div [ A.id "graph" ]
                [{- This is where viz.js renders svg graph -}]
            ]
        ]


type alias PropertyConfig msg =
    { propertyName : String
    , wikiLink : String
    , hasProperty : DerivedInfo -> Bool
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

      -- TODO explain that any subset of cartesian product is a relation
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Reflexive"
      , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation"
      , hasProperty = Rel.isReflexive
      , closureButton = Just DoReflexiveClosure
      , genRandom = Just GenReflexive
      , onHoverExplanation = Just ExplainReflexive
      }
    , { propertyName = "Irreflexive"
      , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation#Irreflexivity"
      , hasProperty = Rel.isIrreflexive
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainIrreflexive
      }
    , { propertyName = "Symmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Symmetric_relation"
      , hasProperty = Rel.isSymmetric
      , closureButton = Just DoSymmetricClosure
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainSymmetric
      }
    , { propertyName = "Antisymmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Antisymmetric_relation"
      , hasProperty = Rel.isAntisymmetric
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainAntisymmetric
      }
    , { propertyName = "Asymmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Asymmetric_relation"
      , hasProperty = Rel.isAsymmetric
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainAsymmetric
      }
    , { propertyName = "Transitive"
      , wikiLink = "https://en.wikipedia.org/wiki/Transitive_relation"
      , hasProperty = Rel.isTransitive
      , closureButton = Just DoTransitiveClosure
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainTransitive
      }
    , { propertyName = "Connected"
      , wikiLink = "https://en.wikipedia.org/wiki/Connected_relation"
      , hasProperty = .isConnected
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainWhyNotConnected
      }
    , { propertyName = "Acyclic"
      , wikiLink = "https://en.wikipedia.org/wiki/Glossary_of_order_theory#A"
      , hasProperty = .isAcyclic
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainWhyNotAcyclic
      }
    , { propertyName = "Partial function"
      , wikiLink = "https://en.wikipedia.org/wiki/Partial_function"
      , hasProperty = .isPartialFunction
      , closureButton = Nothing
      , genRandom = Just GenPartialFunction
      , onHoverExplanation = Just ExplainWhyNotPartialFunction
      }
    , { propertyName = "Function"
      , wikiLink = "https://en.wikipedia.org/wiki/Function_(mathematics)"
      , hasProperty = Rel.isFunction
      , closureButton = Nothing
      , genRandom = Just GenFunction
      , onHoverExplanation = Just ExplainFunction
      }
    , { propertyName = "Bijection (Permutation)"
      , wikiLink = "https://en.wikipedia.org/wiki/Bijection"
      , hasProperty = .isBijectiveFunction
      , closureButton = Nothing
      , genRandom = Just GenBijectiveFunction

      -- TODO explain why not bijective function
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Derangement"
      , wikiLink = "https://en.wikipedia.org/wiki/Derangement"
      , hasProperty = .isDerangement
      , closureButton = Nothing
      , genRandom = Nothing

      -- TODO explain why not Derangement
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Involution"
      , wikiLink = "https://en.wikipedia.org/wiki/Involution_(mathematics)"
      , hasProperty = .isInvolution
      , closureButton = Nothing
      , genRandom = Nothing

      -- TODO explain why not involution
      , onHoverExplanation = Nothing
      }
    ]


elementaryPropertiesView : DerivedInfo -> Html Msg
elementaryPropertiesView derivedInfo =
    let
        row : PropertyConfig Msg -> Html Msg
        row { propertyName, wikiLink, hasProperty, closureButton, genRandom, onHoverExplanation } =
            let
                hasProp =
                    hasProperty derivedInfo
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
    Html.span
        [ E.onMouseEnter onHover
        , E.onMouseLeave HideExplanations
        ]
        [ Html.text <|
            (if b then
                "Yes"

             else
                "No"
            )
                ++ " - ⓘ"
        ]


sizeInputView : Int -> Html Msg
sizeInputView relSize =
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
        , Html.button [ E.onClick UndoHistory, A.title "Undo previous edits" ] [ Html.text "Undo" ]
        , -- TODO think about how to decompose this hodpodge of controls
          Html.button [ E.onClick MakeEmpty, A.title "Empty relation" ] [ Html.text "∅" ]
        , Html.button [ E.onClick DoComplement ] [ Html.text "Complement" ]
        , Html.button [ E.onClick DoConverse ] [ Html.text "Converse" ]
        , setTrueProbView trueProb
        ]
