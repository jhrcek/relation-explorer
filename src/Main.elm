module Main exposing (main)

import Browser
import Count
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Natural exposing (Natural)
import Permutation exposing (Permutation)
import Ports
import Random exposing (Generator)
import Rel exposing (DerivedInfo, ExplainableProperty(..), Highlight(..), Rel)
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
    , permutationState : Permutation.State
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
            , permutationState = Permutation.init initSize
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
    | PermutationEdited String
    | ApplyPermutation Permutation
    | NextRelation
    | DoReflexiveClosure
    | DoReflexiveReduction
    | DoSymmetricClosure
    | DoTransitiveClosure
    | DoTransitiveReduction
    | DoCoveringRelation
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
    | GenAcyclic
    | GotRandom Rel
      -- Explanations
    | ShowExplanation ExplainableProperty
    | HideExplanation
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

        PermutationEdited str ->
            pure { model | permutationState = Permutation.update str (Rel.size model.rel) model.permutationState }

        ApplyPermutation permutation ->
            updateRel (Rel.applyPermutation permutation) { model | permutationState = Permutation.init (Rel.size model.rel) }

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

        DoCoveringRelation ->
            case model.derivedInfo.acyclicInfo of
                Ok acyclicInfo ->
                    case acyclicInfo.posetInfo of
                        Just _ ->
                            updateRel (\_ -> acyclicInfo.coveringRelation) model

                        Nothing ->
                            pure model

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

        GenAcyclic ->
            generateRel (Rel.genAcyclic model.trueProb) model

        ShowExplanation exProperty ->
            pure { model | highlight = Explanation exProperty <| Rel.getExplanationData exProperty model.derivedInfo }

        HideExplanation ->
            pure { model | highlight = NoHighlight }

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
                , highlight = updateHighlight newDerivedInfo model.highlight
                , permutationState = Permutation.updateSize (Rel.size newRel) model.permutationState
            }
    in
    updateGraph newModel


updateHighlight : DerivedInfo -> Highlight -> Highlight
updateHighlight derivedInfo highlight =
    case highlight of
        Explanation exProperty _ ->
            Explanation exProperty <| Rel.getExplanationData exProperty derivedInfo

        Pairs _ ->
            highlight

        NoHighlight ->
            highlight


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
                        , highlight = updateHighlight prevDerivedInfo model.highlight
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
                , elementaryPropertiesView model.derivedInfo model.highlight
                , miscControls model.trueProb
                ]
            , Html.div [ A.id "explanation" ]
                [ Html.h4 [] [ Html.text "Properties" ]
                , Html.details []
                    [ Html.summary [] [ Html.text "Set Theory" ]
                    , Html.div []
                        [ Html.text <| "Underlying Set: X = " ++ Rel.showIntListAsSet (Rel.domain model.rel) ]
                    , Html.div []
                        [ Html.text "R ⊆ X ⨯ X" ]
                    , Html.div []
                        [ Html.text <| "R = " ++ Rel.showElements model.rel ]
                    , Html.div []
                        [ Html.text <| "|R| = " ++ String.fromInt model.derivedInfo.elementCount ]
                    ]
                , Html.details []
                    [ Html.summary [] [ Html.text "Graph Theory" ]
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
                        [ Html.text <|
                            case model.derivedInfo.acyclicInfo of
                                Ok acInfo ->
                                    "The relation is acyclic. Acyclic relations can be sorted topologically. Example top. sort: "
                                        ++ Rel.showIntListAsList (Rel.topologicalSort acInfo.acyclic)

                                Err cycle ->
                                    "The relation contains cycle(s). Example cycle: "
                                        ++ Rel.showIntListAsList cycle
                        ]
                    ]
                , Html.details []
                    [ Html.summary [] [ Html.text "Group Theory" ]
                    , case model.derivedInfo.permutationInfo of
                        Just permutation ->
                            Html.div []
                                [ Html.text "This relation is a permutation."
                                , Html.div [ A.class "indent" ]
                                    [ Html.div [] [ Html.text <| "One-line notation: " ++ Permutation.showOneLineNotation permutation ]
                                    , Html.div [] [ Html.text <| "Cycle notation: " ++ Permutation.showCycles permutation ]
                                    , Html.div [] [ Html.text <| "Cycle type: " ++ Permutation.showCycleType permutation ]
                                    , Html.div [] [ Html.text <| "Number of permutations of the same cycle type: " ++ String.fromInt (Permutation.conjugacyClassSize permutation) ]
                                    , Html.div [] [ Html.text <| "Fixed points: " ++ Rel.showIntListAsSet (Permutation.fixedPoints permutation) ]
                                    , Html.div []
                                        [ Html.text <|
                                            "Parity: "
                                                ++ (if Permutation.isEven permutation then
                                                        "even"

                                                    else
                                                        "odd"
                                                   )
                                        ]
                                    , Html.div []
                                        [ Html.text <| "Order: " ++ String.fromInt (Permutation.order permutation)
                                        , Html.span [ A.title "Order is the smallest number representing how many times the permutation would have to be composed with itself to result in identity permutation" ]
                                            [ Html.text " ⓘ" ]
                                        ]
                                    ]
                                ]

                        Nothing ->
                            Html.text "This relation is not a permutation."
                    , Html.h4 [] [ Html.text "Operations" ]
                    , Html.div [] [ Html.text "Relabel relation elements by applying permutation group action" ]
                    , let
                        st =
                            model.permutationState

                        permButtonAttr =
                            case st.result of
                                Err _ ->
                                    A.disabled True

                                Ok perm ->
                                    E.onClick <| ApplyPermutation perm

                        permError =
                            case st.result of
                                Err e ->
                                    Html.div [ A.class "error" ] [ Html.text e ]

                                Ok _ ->
                                    Html.text ""
                      in
                      Html.div []
                        [ Html.label []
                            [ Html.text "Permutation "
                            , Html.input
                                [ E.onInput PermutationEdited
                                , A.placeholder "Cycle notation: (1 2 3) (4 5)"
                                , A.value st.rawInput
                                ]
                                []
                            ]
                        , Html.button [ permButtonAttr ] [ Html.text "Apply" ]
                        , Html.span [ A.title "Apply permutation p on elements of the set X (calculating new relation R' = p∘R∘p⁻¹)" ]
                            [ Html.text "ⓘ" ]

                        -- TODO next/previous button to enumerate all permutations
                        , permError
                        ]
                    ]
                , Html.details []
                    [ Html.summary [] [ Html.text "Formal Concept Analysis (FCA)" ]
                    , Html.div [] <|
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
                , Html.details []
                    [ Html.summary [] [ Html.text "Order Theory" ]
                    , Html.div []
                        [ case model.derivedInfo.acyclicInfo |> Result.toMaybe |> Maybe.andThen .posetInfo of
                            Just posetInfo ->
                                Html.div [] <|
                                    -- TODO explain why not partial order
                                    Html.div []
                                        [ Html.text "This relation is a partial order."
                                        , Html.div [ A.class "indent" ]
                                            [ Html.text <| "Minimal elements: " ++ Rel.showIntSet posetInfo.minimalElements ]
                                        , Html.div [ A.class "indent" ]
                                            [ Html.text <| "Maximal elements: " ++ Rel.showIntSet posetInfo.maximalElements ]
                                        ]
                                        :: Html.div []
                                            [ Html.text <|
                                                if Rel.isPosetLattice posetInfo then
                                                    "This relation is a lattice"

                                                else
                                                    "This relation is not a lattice"
                                            ]
                                        :: Rel.viewJoinInfo posetInfo.joinTable
                                        ++ Rel.viewMeetInfo posetInfo.meetTable

                            Nothing ->
                                Html.text "This relation is not a partial order."
                        ]
                    ]
                , case model.highlight of
                    Explanation _ exp ->
                        Html.div [ A.class "explanation" ] <|
                            Html.h4 [ A.class "explanation-title" ] [ Html.text "Explanation" ]
                                :: Html.button
                                    [ A.class "exlanation-close"
                                    , A.title "Close Explanation"
                                    , E.onClick HideExplanation
                                    ]
                                    [ Html.text "✕" ]
                                :: List.map (\line -> Html.div [] [ Html.text line ]) exp.lines

                    Pairs _ ->
                        Html.text ""

                    NoHighlight ->
                        Html.text ""
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
    , toggleExplanation : Maybe ExplainableProperty

    -- TODO maybe cache the result in model?
    , countFormula : Maybe (Int -> Natural)
    , oeisId : Maybe String
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
      , toggleExplanation = Just ExplainRelation
      , countFormula = Just Count.rel
      , oeisId = Just "A002416"
      }
    , { propertyName = "Reflexive"
      , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation"
      , hasProperty = Rel.isReflexive
      , buttons = [ ButtonConfig "Closure" DoReflexiveClosure Rel.isReflexive ]
      , genRandom = Just GenReflexive
      , toggleExplanation = Just ExplainReflexive
      , countFormula = Just Count.reflexiveOrIrreflexive
      , oeisId = Just "A053763"
      }
    , { propertyName = "Irreflexive"
      , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation#Irreflexivity"
      , hasProperty = Rel.isIrreflexive
      , buttons = [ ButtonConfig "Reduction" DoReflexiveReduction Rel.isIrreflexive ]
      , genRandom = Just GenIrreflexive
      , toggleExplanation = Just ExplainIrreflexive
      , countFormula = Just Count.reflexiveOrIrreflexive
      , oeisId = Just "A053763"
      }
    , { propertyName = "Symmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Symmetric_relation"
      , hasProperty = Rel.isSymmetric
      , buttons = [ ButtonConfig "Closure" DoSymmetricClosure Rel.isSymmetric ]
      , genRandom = Just GenSymmetric
      , toggleExplanation = Just ExplainSymmetric
      , countFormula = Just Count.symmetric
      , oeisId = Nothing
      }
    , { propertyName = "Antisymmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Antisymmetric_relation"
      , hasProperty = Rel.isAntisymmetric
      , buttons = []
      , genRandom = Just GenAntisymmetric
      , toggleExplanation = Just ExplainAntisymmetric
      , countFormula = Just Count.antisymmetric
      , oeisId = Just "A083667"
      }
    , { propertyName = "Asymmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Asymmetric_relation"
      , hasProperty = Rel.isAsymmetric
      , buttons = []
      , genRandom = Just GenAsymmetric
      , toggleExplanation = Just ExplainAsymmetric
      , countFormula = Just Count.asymmetric
      , oeisId = Just "A047656"
      }
    , { propertyName = "Transitive"
      , wikiLink = "https://en.wikipedia.org/wiki/Transitive_relation"
      , hasProperty = Rel.isTransitive
      , buttons = [ ButtonConfig "Closure" DoTransitiveClosure Rel.isTransitive ]
      , genRandom = Nothing
      , toggleExplanation = Just ExplainTransitive
      , countFormula = Just Count.transitive
      , oeisId = Just "A006905"
      }
    , { propertyName = "Connected"
      , wikiLink = "https://en.wikipedia.org/wiki/Connected_relation"
      , hasProperty = Rel.isConnected
      , buttons = []
      , genRandom = Just GenConnected
      , toggleExplanation = Just ExplainConnected
      , countFormula = Just Count.connected

      -- TODO connected OEIS?
      , oeisId = Nothing
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
      , genRandom = Just GenAcyclic
      , toggleExplanation = Just ExplainAcyclic

      -- TODO count acyclic
      , countFormula = Nothing

      -- TODO acyclic OEIS?
      , oeisId = Nothing
      }
    , { propertyName = "Functional"
      , wikiLink = "https://en.wikipedia.org/wiki/Binary_relation#Special_types_of_binary_relations"
      , hasProperty = Rel.isFunctional
      , buttons = []
      , genRandom = Just GenFunctional
      , toggleExplanation = Just ExplainFunctional
      , countFormula = Just Count.partialFunctions
      , oeisId = Just "A000169"
      }
    , { propertyName = "(Left-)Total"
      , wikiLink = "https://en.wikipedia.org/wiki/Total_relation"
      , hasProperty = Rel.isLeftTotal
      , buttons = []
      , genRandom = Just GenLeftTotal
      , toggleExplanation = Just ExplainLeftTotal
      , countFormula = Just Count.leftTotal
      , oeisId = Just "A055601"
      }
    , { propertyName = "Bijection (Permutation)"
      , wikiLink = "https://en.wikipedia.org/wiki/Bijection"
      , hasProperty = .isBijectiveFunction
      , buttons = []
      , genRandom = Just GenBijectiveFunction

      -- TODO explain why not bijective function
      , toggleExplanation = Nothing
      , countFormula = Just Count.totalOrderOrPermutation
      , oeisId = Just "A000142"
      }
    , { propertyName = "Derangement"
      , wikiLink = "https://en.wikipedia.org/wiki/Derangement"
      , hasProperty = .isDerangement
      , buttons = []

      -- TODO generate derangements. Find some inspiration from this paper https://epubs.siam.org/doi/pdf/10.1137/1.9781611972986.7
      , genRandom = Nothing

      -- TODO explain why not Derangement
      , toggleExplanation = Nothing
      , countFormula = Just Count.derangement
      , oeisId = Just "A000166"
      }
    , { propertyName = "Involution"
      , wikiLink = "https://en.wikipedia.org/wiki/Involution_(mathematics)"
      , hasProperty = .isInvolution
      , buttons = []
      , genRandom = Just GenInvolution

      -- TODO explain why not involution
      , toggleExplanation = Nothing
      , countFormula = Just Count.involution
      , oeisId = Just "A000085"
      }
    , { propertyName = "Partial Order"
      , wikiLink = "https://en.wikipedia.org/wiki/Partially_ordered_set"
      , hasProperty = Rel.isPartialOrder
      , buttons =
            [ ButtonConfig "Covering Relation"
                DoCoveringRelation
                (\info ->
                    case info.acyclicInfo of
                        Ok acyclicInfo ->
                            case acyclicInfo.posetInfo of
                                Just _ ->
                                    Set.isEmpty acyclicInfo.redundantTransitiveEdges && Set.isEmpty info.onDiagonalElements

                                Nothing ->
                                    True

                        Err _ ->
                            -- Disable for rels with cycles, for which there's no unique TR
                            True
                )
            ]

      -- TODO generate partial order
      , genRandom = Nothing

      -- TODO explain why not total oder
      , toggleExplanation = Nothing
      , countFormula = Just Count.poset
      , oeisId = Just "A001035"
      }
    , { propertyName = "Lattice"
      , wikiLink = "https://en.wikipedia.org/wiki/Lattice_(order)"
      , hasProperty = Rel.isLattice
      , buttons = []

      -- TODO generate lattice
      , genRandom = Nothing

      -- TODO explain why not lattice
      , toggleExplanation = Nothing
      , countFormula = Just Count.lattice
      , oeisId = Just "A006966"
      }

    -- TODO have separate section for "composite" concepts
    , { propertyName = "Total Order"
      , wikiLink = "https://en.wikipedia.org/wiki/Total_order"
      , hasProperty = Rel.isTotalOrder
      , buttons = []
      , genRandom = Just GenTotalOrder

      -- TODO explain why not total oder
      , toggleExplanation = Nothing
      , countFormula = Just Count.totalOrderOrPermutation
      , oeisId = Just "A000142"
      }
    ]


explainedProperty : Highlight -> Maybe ExplainableProperty
explainedProperty highlight =
    case highlight of
        Explanation explainableProperty _ ->
            Just explainableProperty

        Pairs _ ->
            Nothing

        NoHighlight ->
            Nothing


elementaryPropertiesView : DerivedInfo -> Highlight -> Html Msg
elementaryPropertiesView derivedInfo highlight =
    let
        activeProperty =
            explainedProperty highlight

        row : PropertyConfig Msg -> Html Msg
        row { propertyName, wikiLink, hasProperty, buttons, genRandom, toggleExplanation, countFormula, oeisId } =
            let
                hasProp =
                    hasProperty derivedInfo
            in
            Html.tr []
                [ Html.td [] [ blankLink wikiLink propertyName ]
                , Html.td [] [ yesNo hasProp ]
                , Html.td [] <|
                    case toggleExplanation of
                        Just thisProperty ->
                            let
                                ( onClick, label, title ) =
                                    case activeProperty of
                                        Just activeProp ->
                                            if activeProp == thisProperty then
                                                ( HideExplanation, "✕", "Hide explanation" )

                                            else
                                                ( ShowExplanation thisProperty, "ⓘ", "Show explanation" )

                                        Nothing ->
                                            ( ShowExplanation thisProperty, "ⓘ", "Show explanation" )
                            in
                            [ Html.button [ E.onClick onClick, A.title title ]
                                [ Html.text label ]
                            ]

                        Nothing ->
                            []
                , Html.td [] <|
                    case genRandom of
                        Just genMsg ->
                            [ Html.button [ E.onClick genMsg ] [ Html.text "⚄" ] ]

                        Nothing ->
                            []
                , Html.td [] <|
                    List.map
                        (\butCfg ->
                            Html.button
                                [ E.onClick butCfg.message
                                , A.disabled <| butCfg.disabled derivedInfo
                                , A.class "fill-cell-button"
                                ]
                                [ Html.text butCfg.label ]
                        )
                        buttons
                , Html.td []
                    [ case countFormula of
                        Just cnt ->
                            Html.text <| Natural.toString <| cnt derivedInfo.relSize

                        Nothing ->
                            Html.text "??"
                    ]
                , Html.td []
                    (case oeisId of
                        Just oid ->
                            [ Html.a [ A.href ("https://oeis.org/" ++ oid), A.target "_blank" ]
                                [ Html.text oid ]
                            ]

                        Nothing ->
                            []
                    )
                ]
    in
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Property name" ]
                , Html.th [] [ Html.text "Does R have this property?" ]
                , Html.th [] [ Html.text "Expl." ]
                , Html.th [] [ Html.text "Gen" ]
                , Html.th [] [ Html.text "Operations" ]
                , Html.th [] [ Html.text "Count" ]
                , Html.th [] [ Html.text "OEIS" ]
                ]
            ]
        , Html.tbody [] <| List.map row propertyConfigs
        ]


blankLink : String -> String -> Html msg
blankLink href text =
    Html.a [ A.href href, A.target "_blank" ] [ Html.text text ]


yesNo : Bool -> Html Msg
yesNo b =
    let
        ( color, label ) =
            if b then
                ( "green", "✔" )

            else
                ( "red", "✘" )
    in
    Html.span
        [ A.style "color" color ]
        [ Html.text label ]


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
                    ++ Natural.toString (Count.rel <| Rel.size rel)
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
