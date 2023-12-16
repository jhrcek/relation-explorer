module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Natural
import Ports
import Random exposing (Generator)
import Rel exposing (DerivedInfo, Highlight(..), Rel)
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
    , highlight : Highlight
    , highlightSccs : Bool
    , showIntents : Bool
    , showExtents : Bool
    , graphMode : GraphMode
    , -- Number between 0 and 1, indicating how likely it is that random generators
      -- will produce a True value (and thus generate an element of a relation)-}
      trueProb : Float
    }


type GraphMode
    = RelationGraph
    | BipartiteGraph
    | HasseDiagram
    | ConceptLattice


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initSize =
            4

        initRel =
            Rel.empty initSize

        initModel =
            { rel = initRel
            , history = []
            , derivedInfo = Rel.deriveInfo initRel
            , highlight = NoHighlight
            , trueProb = 0.2
            , graphMode = RelationGraph
            , highlightSccs = True
            , showIntents = True
            , showExtents = True
            }
    in
    updateGraph initModel


updateGraph : Model -> ( Model, Cmd Msg )
updateGraph model =
    ( model, renderGraph model )


renderGraph :
    { r
        | rel : Rel
        , derivedInfo : DerivedInfo
        , graphMode : GraphMode
        , highlightSccs : Bool
        , showExtents : Bool
        , showIntents : Bool
    }
    -> Cmd msg
renderGraph { rel, derivedInfo, graphMode, highlightSccs, showExtents, showIntents } =
    Ports.renderDot <|
        case graphMode of
            RelationGraph ->
                { engine =
                    if Rel.isAcyclic derivedInfo then
                        "dot"

                    else
                        "neato"
                , dotSource = Rel.relationGraphToDotSource rel { highlightSccs = highlightSccs, showArrowheads = True }
                }

            BipartiteGraph ->
                { engine = "neato"
                , dotSource = Rel.relationBipartiteGraphDotSource rel
                }

            HasseDiagram ->
                { engine = "dot"
                , dotSource =
                    case derivedInfo.acyclicInfo of
                        Ok acyclicInfo ->
                            Rel.hasseDiagramToDotSource acyclicInfo

                        Err _ ->
                            Rel.errorInBoxDot "Cannot draw Hasse diagram for a relation that contains cycles."
                }

            ConceptLattice ->
                { engine = "dot"
                , dotSource = Rel.conceptLatticeToDotSource derivedInfo.formalConcepts showExtents showIntents
                }


type Msg
    = SetRelSize Int
    | SetTrueProb Float
    | ToggleRel Int Int
    | ToggleGraphMode GraphMode
    | ToggleHighlightSccs
    | ToggleShowExtents
    | ToggleShowIntents
    | PreviousRelation
    | NextRelation
    | DoReflexiveClosure
    | DoReflexiveReduction
    | DoSymmetricClosure
    | DoTransitiveClosure
    | DoTransitiveReduction
    | DoComplement
    | DoConverse
    | MakeEmpty
      -- Random generation
    | GenRel
    | GenReflexive
    | GenIrreflexive
    | GenSymmetric
    | GenAntisymmetric
    | GenAsymmetric
    | GenConnected
    | GenFunctional
    | GenLeftTotal
    | GenBijectiveFunction
    | GenInvolution
    | GenTotalOrder
    | GotRandom Rel
      -- Explanations
    | ExplainRelation
    | ExplainReflexive
    | ExplainIrreflexive
    | ExplainSymmetric
    | ExplainAntisymmetric
    | ExplainAsymmetric
    | ExplainTransitive
    | ExplainAcyclic
    | ExplainConnected
    | ExplainFunctional
    | ExplainLeftTotal
    | HighlightConcept (Set Int)
    | ClearHighlight
    | UndoHistory


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

        ToggleGraphMode newGraphMode ->
            updateGraph { model | graphMode = newGraphMode }

        ToggleHighlightSccs ->
            updateGraph { model | highlightSccs = not model.highlightSccs }

        ToggleShowExtents ->
            updateGraph { model | showExtents = not model.showExtents }

        ToggleShowIntents ->
            updateGraph { model | showIntents = not model.showIntents }

        PreviousRelation ->
            updateRel Rel.prev model

        NextRelation ->
            updateRel Rel.next model

        DoReflexiveClosure ->
            updateRel Rel.reflexiveClosure model

        DoReflexiveReduction ->
            updateRel Rel.reflexiveReduction model

        DoSymmetricClosure ->
            updateRel Rel.symmetricClosure model

        DoTransitiveClosure ->
            updateRel Rel.transitiveClosure model

        DoTransitiveReduction ->
            case model.derivedInfo.acyclicInfo of
                Ok acyclicInfo ->
                    updateRel (\_ -> acyclicInfo.transitivelyReduced) model

                Err _ ->
                    pure model

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

        GenIrreflexive ->
            generateRel (Rel.genIrreflexiveRelation model.trueProb) model

        GenSymmetric ->
            generateRel (Rel.genSymmetricRelation model.trueProb) model

        GenAntisymmetric ->
            generateRel (Rel.genAntisymmetricRelation model.trueProb) model

        GenAsymmetric ->
            generateRel (Rel.genAsymmetricRelation model.trueProb) model

        GenConnected ->
            generateRel (Rel.genConnectedRelation model.trueProb) model

        GenFunctional ->
            generateRel (Rel.genFunctionalRelation model.trueProb) model

        GenLeftTotal ->
            generateRel (Rel.genLeftTotal model.trueProb) model

        GenBijectiveFunction ->
            generateRel Rel.genBijectiveFunction model

        GenInvolution ->
            generateRel Rel.genInvolution model

        GenTotalOrder ->
            generateRel Rel.genTotalOrder model

        ExplainRelation ->
            pure { model | highlight = Explanation <| Rel.explainRelation model.derivedInfo }

        ExplainReflexive ->
            pure { model | highlight = Explanation <| Rel.explainReflexive model.derivedInfo }

        ExplainIrreflexive ->
            pure { model | highlight = Explanation <| Rel.explainIrreflexive model.derivedInfo }

        ExplainSymmetric ->
            pure { model | highlight = Explanation <| Rel.explainSymmetric model.derivedInfo }

        ExplainAntisymmetric ->
            pure { model | highlight = Explanation <| Rel.explainAntisymmetric model.derivedInfo }

        ExplainAsymmetric ->
            pure { model | highlight = Explanation <| Rel.explainAsymmetric model.derivedInfo }

        ExplainTransitive ->
            pure { model | highlight = Explanation <| Rel.explainTransitive model.derivedInfo }

        ExplainAcyclic ->
            pure { model | highlight = Explanation <| Rel.explainAcyclic model.derivedInfo }

        ExplainFunctional ->
            pure { model | highlight = Explanation <| Rel.explainFunctional model.derivedInfo }

        ExplainConnected ->
            pure { model | highlight = Explanation <| Rel.explainConnected model.derivedInfo }

        ExplainLeftTotal ->
            pure { model | highlight = Explanation <| Rel.explainLeftTotal model.derivedInfo }

        HighlightConcept attrClosure ->
            let
                objects =
                    Rel.objectsSharingAllAttributes model.rel attrClosure

                prod =
                    Rel.cartesianProduct objects attrClosure
            in
            pure { model | highlight = Pairs prod }

        ClearHighlight ->
            pure { model | highlight = NoHighlight }

        UndoHistory ->
            undoHistory model


updateRel : (Rel -> Rel) -> Model -> ( Model, Cmd Msg )
updateRel f model =
    let
        newRel =
            f model.rel

        newDerivedInfo =
            Rel.deriveInfo newRel

        newModel =
            { model
                | rel = newRel
                , derivedInfo = newDerivedInfo
                , history = model.rel :: model.history
            }
    in
    updateGraph newModel


undoHistory : Model -> ( Model, Cmd Msg )
undoHistory model =
    case model.history of
        [] ->
            ( model, Cmd.none )

        prevRel :: rest ->
            let
                prevDerivedInfo =
                    Rel.deriveInfo prevRel

                newModel =
                    { model
                        | rel = prevRel
                        , derivedInfo = prevDerivedInfo
                        , history = rest
                    }
            in
            updateGraph newModel


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


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ A.id "top-container" ]
            [ Html.div [ A.id "rel-and-controls" ]
                [ sizeInputView model.derivedInfo.relSize
                , Rel.view relConfig model.rel model.highlight
                , pager model.rel
                , elementaryPropertiesView model.derivedInfo
                , miscControls model.trueProb
                ]
            , Html.div [ A.id "explanation" ]
                [ Html.div []
                    [ Html.div []
                        [ Html.text <| "X = " ++ Rel.showIntListAsSet (Rel.domain model.rel) ]
                    , Html.div []
                        [ Html.text <| "R ⊆ X ⨯ X, R = " ++ Rel.showElements model.rel ]
                    , Html.div []
                        [ Html.text <|
                            -- TODO highlight SCCs with the same  color as in the graph
                            "Strongly connected components: "
                                ++ String.join ", "
                                    (List.map Rel.showIntListAsSet <|
                                        Rel.scc model.rel
                                    )
                        ]
                    , Html.div []
                        [ Html.text "FCA"
                        , Html.div [ A.class "indent" ] <|
                            let
                                concepts =
                                    model.derivedInfo.formalConcepts
                            in
                            [ Html.text <|
                                "formal concepts (total "
                                    ++ String.fromInt (List.length concepts)
                                    ++ "): "
                            , Html.ul [] <|
                                List.map
                                    (\( objects, attrs ) ->
                                        Html.li
                                            [ E.onMouseEnter (HighlightConcept attrs)
                                            , E.onMouseLeave ClearHighlight
                                            ]
                                            [ Html.text <| "(" ++ Rel.showIntSet objects ++ "," ++ Rel.showIntSet attrs ++ ")" ]
                                    )
                                    concepts
                            ]
                        ]
                    , case model.highlight of
                        Explanation exp ->
                            Html.div [] <| List.map (\line -> Html.div [] [ Html.text line ]) exp.lines

                        Pairs _ ->
                            Html.text ""

                        NoHighlight ->
                            Html.text ""
                    ]
                ]
            , Html.div []
                [ graphControls model
                , Html.div [ A.id "graph" ]
                    [{- This is where viz.js renders svg graph -}]
                ]
            ]
        ]


type alias PropertyConfig msg =
    { propertyName : String
    , wikiLink : String
    , hasProperty : DerivedInfo -> Bool
    , buttons : List (ButtonConfig msg)
    , genRandom : Maybe msg
    , onHoverExplanation : Maybe msg
    }


type alias ButtonConfig msg =
    { label : String
    , message : msg
    , disabled : DerivedInfo -> Bool
    }


propertyConfigs : List (PropertyConfig Msg)
propertyConfigs =
    [ { propertyName = "Relation"
      , wikiLink = "https://en.wikipedia.org/wiki/Relation_(mathematics)"
      , hasProperty = always True
      , buttons = []
      , genRandom = Just GenRel
      , onHoverExplanation = Just ExplainRelation
      }
    , { propertyName = "Reflexive"
      , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation"
      , hasProperty = Rel.isReflexive
      , buttons = [ ButtonConfig "Closure" DoReflexiveClosure Rel.isReflexive ]
      , genRandom = Just GenReflexive
      , onHoverExplanation = Just ExplainReflexive
      }
    , { propertyName = "Irreflexive"
      , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation#Irreflexivity"
      , hasProperty = Rel.isIrreflexive
      , buttons = [ ButtonConfig "Reduction" DoReflexiveReduction Rel.isIrreflexive ]
      , genRandom = Just GenIrreflexive
      , onHoverExplanation = Just ExplainIrreflexive
      }
    , { propertyName = "Symmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Symmetric_relation"
      , hasProperty = Rel.isSymmetric
      , buttons = [ ButtonConfig "Closure" DoSymmetricClosure Rel.isSymmetric ]
      , genRandom = Just GenSymmetric
      , onHoverExplanation = Just ExplainSymmetric
      }
    , { propertyName = "Antisymmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Antisymmetric_relation"
      , hasProperty = Rel.isAntisymmetric
      , buttons = []
      , genRandom = Just GenAntisymmetric
      , onHoverExplanation = Just ExplainAntisymmetric
      }
    , { propertyName = "Asymmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Asymmetric_relation"
      , hasProperty = Rel.isAsymmetric
      , buttons = []
      , genRandom = Just GenAsymmetric
      , onHoverExplanation = Just ExplainAsymmetric
      }
    , { propertyName = "Transitive"
      , wikiLink = "https://en.wikipedia.org/wiki/Transitive_relation"
      , hasProperty = Rel.isTransitive
      , buttons = [ ButtonConfig "Closure" DoTransitiveClosure Rel.isTransitive ]
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainTransitive
      }
    , { propertyName = "Connected"
      , wikiLink = "https://en.wikipedia.org/wiki/Connected_relation"
      , hasProperty = Rel.isConnected
      , buttons = []
      , genRandom = Just GenConnected
      , onHoverExplanation = Just ExplainConnected
      }
    , { propertyName = "Acyclic"
      , wikiLink = "https://en.wikipedia.org/wiki/Glossary_of_order_theory#A"
      , hasProperty = Rel.isAcyclic
      , buttons =
            [ ButtonConfig "Transitive Reduction"
                DoTransitiveReduction
                (\info ->
                    case info.acyclicInfo of
                        Ok acyclicInfo ->
                            -- Disable when TR wouldn't remove any edges
                            Set.isEmpty acyclicInfo.redundantTransitiveEdges

                        Err _ ->
                            -- Disable for rels with cycles, for which there's no unique TR
                            True
                )
            ]
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainAcyclic
      }
    , { propertyName = "Functional"
      , wikiLink = "https://en.wikipedia.org/wiki/Binary_relation#Special_types_of_binary_relations"
      , hasProperty = Rel.isFunctional
      , buttons = []
      , genRandom = Just GenFunctional
      , onHoverExplanation = Just ExplainFunctional
      }
    , { propertyName = "(Left-)Total"
      , wikiLink = "https://en.wikipedia.org/wiki/Total_relation"
      , hasProperty = Rel.isLeftTotal
      , buttons = []
      , genRandom = Just GenLeftTotal
      , onHoverExplanation = Just ExplainLeftTotal
      }
    , { propertyName = "Bijection (Permutation)"
      , wikiLink = "https://en.wikipedia.org/wiki/Bijection"
      , hasProperty = .isBijectiveFunction
      , buttons = []
      , genRandom = Just GenBijectiveFunction

      -- TODO explain why not bijective function
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Derangement"
      , wikiLink = "https://en.wikipedia.org/wiki/Derangement"
      , hasProperty = .isDerangement
      , buttons = []

      -- Get some inspiration from this paper https://epubs.siam.org/doi/pdf/10.1137/1.9781611972986.7
      , genRandom = Nothing

      -- TODO explain why not Derangement
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Involution"
      , wikiLink = "https://en.wikipedia.org/wiki/Involution_(mathematics)"
      , hasProperty = .isInvolution
      , buttons = []
      , genRandom = Just GenInvolution

      -- TODO explain why not involution
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Partial Order"
      , wikiLink = "https://en.wikipedia.org/wiki/Partially_ordered_set"
      , hasProperty = Rel.isPartialOrder
      , buttons = []

      -- TODO generate partial order
      , genRandom = Nothing

      -- TODO explain why not total oder
      , onHoverExplanation = Nothing
      }

    -- TODO have separate section for "composite" concepts
    , { propertyName = "Total Order"
      , wikiLink = "https://en.wikipedia.org/wiki/Total_order"
      , hasProperty = Rel.isTotalOrder
      , buttons = []
      , genRandom = Just GenTotalOrder

      -- TODO explain why not total oder
      , onHoverExplanation = Nothing
      }
    ]


elementaryPropertiesView : DerivedInfo -> Html Msg
elementaryPropertiesView derivedInfo =
    let
        row : PropertyConfig Msg -> Html Msg
        row { propertyName, wikiLink, hasProperty, buttons, genRandom, onHoverExplanation } =
            let
                hasProp =
                    hasProperty derivedInfo
            in
            Html.tr []
                [ Html.td [] [ blankLink wikiLink propertyName ]
                , Html.td [] [ yesNo onHoverExplanation hasProp ]
                , Html.td [] <|
                    List.map
                        (\butCfg ->
                            Html.button
                                [ E.onClick butCfg.message
                                , A.disabled <| butCfg.disabled derivedInfo
                                ]
                                [ Html.text butCfg.label ]
                        )
                        buttons
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
                , Html.th [] [ Html.text "Operations" ]
                , Html.th [] [ Html.text "Generate" ]
                ]
            ]
        , Html.tbody [] <| List.map row propertyConfigs
        ]


blankLink : String -> String -> Html msg
blankLink href text =
    Html.a [ A.href href, A.target "_blank" ] [ Html.text text ]


yesNo : Maybe Msg -> Bool -> Html Msg
yesNo maybeOnHover b =
    let
        ( onHoverAttrs, appendInfoSymbol ) =
            case maybeOnHover of
                Nothing ->
                    ( [], identity )

                Just onHover ->
                    ( [ E.onMouseEnter onHover, E.onMouseLeave ClearHighlight ]
                    , \btnText -> btnText ++ " - ⓘ"
                    )
    in
    Html.span
        onHoverAttrs
        [ Html.text <|
            appendInfoSymbol <|
                if b then
                    "Yes"

                else
                    "No"
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


graphControls :
    { r
        | graphMode : GraphMode
        , highlightSccs : Bool
        , showExtents : Bool
        , showIntents : Bool
        , derivedInfo : DerivedInfo
    }
    -> Html Msg
graphControls { graphMode, highlightSccs, showExtents, showIntents, derivedInfo } =
    Html.div []
        [ Html.h4 [] [ Html.text "Graph Options" ]
        , Html.div []
            [ radio "Relation graph" graphMode RelationGraph True
            , Html.div [ A.class "indent" ]
                [ Html.label []
                    [ Html.input
                        [ A.type_ "checkbox"
                        , A.checked highlightSccs
                        , E.onClick ToggleHighlightSccs
                        , A.disabled (graphMode /= RelationGraph)
                        ]
                        []
                    , Html.text "Highlight SCCs "
                    , Html.span [ A.title "Strongly Connected Components" ] [ Html.text "ⓘ" ]
                    ]
                ]
            ]
        , Html.div []
            [ radio "Bipartite graph" graphMode BipartiteGraph True
            ]
        , Html.div []
            [ radio "Hasse diagram " graphMode HasseDiagram (Rel.isAcyclic derivedInfo)
            , Html.span [ A.title "Only available for acyclic relations" ] [ Html.text "ⓘ" ]
            ]
        , Html.div []
            [ radio "Concept lattice" graphMode ConceptLattice True
            , Html.div [ A.class "indent" ]
                [ Html.label []
                    [ Html.input
                        [ A.type_ "checkbox"
                        , A.checked showExtents
                        , E.onClick ToggleShowExtents
                        , A.disabled (graphMode /= ConceptLattice)
                        ]
                        []
                    , Html.text "Show extents"
                    ]
                , Html.label []
                    [ Html.input
                        [ A.type_ "checkbox"
                        , A.checked showIntents
                        , E.onClick ToggleShowIntents
                        , A.disabled (graphMode /= ConceptLattice)
                        ]
                        []
                    , Html.text "Show intents"
                    ]
                ]
            ]
        ]


pager : Rel -> Html Msg
pager rel =
    Html.div [ A.class "pager" ]
        [ Html.button [ E.onClick PreviousRelation, A.title "Previous relation" ] [ Html.text "<" ]
        , Html.span []
            [ Html.text <|
                " "
                    ++ Natural.toString (Rel.toRelIndex rel)
                    ++ " of "
                    ++ Natural.toString
                        (let
                            n =
                                Natural.fromSafeInt <| Rel.size rel

                            nSquared =
                                Natural.mul n n
                         in
                         Natural.exp Natural.two nSquared
                        )
                    ++ " "
            ]
        , Html.button [ E.onClick NextRelation, A.title "Next relation" ] [ Html.text ">" ]
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


radio : String -> GraphMode -> GraphMode -> Bool -> Html Msg
radio label currentGraphMode graphMode enabled =
    Html.label []
        [ Html.input
            [ A.type_ "radio"
            , A.name "graph-mode"
            , E.onClick (ToggleGraphMode graphMode)
            , A.checked (currentGraphMode == graphMode)
            , A.disabled (not enabled)
            ]
            []
        , Html.text label
        ]
