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
      -- will produce a True value (and thus generate an element of a relation)
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
    | DoEquivalenceClosure
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
    | GenInjective
    | GenSurjective
    | GenBijectiveFunction
    | GenDerangement
    | GenInvolution
    | GenTransitive
    | GenPartialOrder
    | GenTotalOrder
    | GenEquivalence
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

        DoEquivalenceClosure ->
            updateRel Rel.equivalenceClosure model

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

        GenInjective ->
            generateRel (Rel.genInjectiveRelation model.trueProb) model

        GenSurjective ->
            generateRel (Rel.genSurjectiveRelation model.trueProb) model

        GenBijectiveFunction ->
            generateRel Rel.genBijectiveFunction model

        GenDerangement ->
            generateRel Rel.genDerangement model

        GenInvolution ->
            generateRel Rel.genInvolution model

        GenTransitive ->
            generateRel (Rel.genTransitive model.trueProb) model

        GenPartialOrder ->
            generateRel (Rel.genPartialOrder model.trueProb) model

        GenTotalOrder ->
            generateRel Rel.genTotalOrder model

        GenEquivalence ->
            generateRel Rel.genEquivalence model

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
    in
    if newRel == model.rel then
        -- Don't pollute history with edits that didn't change anything
        pure model

    else
        setRel newRel { model | history = List.take maxHistory (model.rel :: model.history) }


maxHistory : Int
maxHistory =
    1000


{-| Replace the relation, recomputing everything that depends on it
-}
setRel : Rel -> Model -> ( Model, Cmd Msg )
setRel newRel model =
    let
        newDerivedInfo =
            Rel.deriveInfo newRel
    in
    updateGraph
        { model
            | rel = newRel
            , derivedInfo = newDerivedInfo
            , highlight = updateHighlight newDerivedInfo model.highlight
            , permutationState = Permutation.updateSize (Rel.size newRel) model.permutationState
        }


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
            setRel prevRel { model | history = rest }


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
        [ Html.header [ A.class "app-header" ]
            [ Html.h1 [] [ Html.text "Relation Explorer" ]
            , blankLink "https://github.com/jhrcek/relation-explorer" "GitHub"
            ]
        , Html.div [ A.class "layout" ]
            [ Html.div [ A.class "column" ]
                [ Html.section [ A.class "panel" ]
                    [ Html.h2 [] [ Html.text "Relation" ]
                    , sizeInputView model.derivedInfo.relSize
                    , Rel.view relConfig model.rel model.highlight
                    , pager model.rel
                    , miscControls
                    ]
                , Html.section [ A.class "panel" ]
                    [ Html.h2 [] [ Html.text "Properties" ]
                    , setTrueProbView model.trueProb
                    , elementaryPropertiesView model.derivedInfo model.highlight
                    ]
                ]
            , Html.div [ A.class "column" ]
                [ case model.highlight of
                    Explanation _ exp ->
                        Html.div [ A.class "explanation" ] <|
                            Html.h3 [ A.class "explanation-title" ] [ Html.text "Explanation" ]
                                :: Html.button
                                    [ A.class "explanation-close icon-button"
                                    , A.title "Close Explanation"
                                    , E.onClick HideExplanation
                                    ]
                                    [ Html.text "✕" ]
                                :: List.map (\line -> Html.div [] [ Html.text line ]) exp.lines

                    Pairs _ ->
                        Html.text ""

                    NoHighlight ->
                        Html.text ""
                , Html.section [ A.class "panel" ]
                    [ Html.h2 [] [ Html.text "Analysis" ]
                    , details "Set Theory"
                        [ Html.div []
                            [ Html.text <| "Underlying Set: X = " ++ Rel.showIntListAsSet (Rel.domain model.rel) ]
                        , Html.div []
                            [ Html.text "R ⊆ X ⨯ X" ]
                        , Html.div []
                            [ Html.text <| "R = " ++ Rel.showElements model.rel ]
                        , Html.div []
                            [ Html.text <| "|R| = " ++ String.fromInt model.derivedInfo.elementCount ]
                        , if Rel.isEquivalence model.derivedInfo then
                            let
                                classes =
                                    Rel.equivalenceClasses model.rel
                            in
                            Html.div []
                                [ Html.text "This relation is an equivalence relation."
                                , Html.div [ A.class "indent" ]
                                    [ Html.text "Equivalence classes:"
                                    , Html.ul [] <|
                                        List.map
                                            (\cls ->
                                                Html.li []
                                                    [ Html.text <|
                                                        String.join " = " (List.map (\x -> "[" ++ String.fromInt x ++ "]") cls)
                                                            ++ " = "
                                                            ++ Rel.showIntListAsSet cls
                                                    ]
                                            )
                                            classes
                                    ]
                                , Html.div [ A.class "indent" ]
                                    [ Html.text <|
                                        "Corresponding partition: X/R = {"
                                            ++ String.join ", " (List.map Rel.showIntListAsSet classes)
                                            ++ "}"
                                    ]
                                ]

                          else
                            Html.text "This relation is not an equivalence relation."
                        ]
                    , details "Graph Theory"
                        [ Html.div []
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
                    , details "Group Theory"
                        [ case model.derivedInfo.permutationInfo of
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
                                            , infoHint "Order is the smallest number representing how many times the permutation would have to be composed with itself to result in identity permutation"
                                            ]
                                        ]
                                    ]

                            Nothing ->
                                Html.text "This relation is not a permutation."
                        , Html.h3 [] [ Html.text "Relabel elements" ]
                        , Html.div [ A.class "muted" ] [ Html.text "Relabel relation elements by applying permutation group action" ]
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
                            [ Html.div [ A.class "control-row" ]
                                [ Html.input
                                    [ A.type_ "text"
                                    , E.onInput PermutationEdited
                                    , A.placeholder "Cycle notation: (1 2 3) (4 5)"
                                    , A.value st.rawInput
                                    ]
                                    []
                                , Html.button [ permButtonAttr ] [ Html.text "Apply" ]
                                , infoHint "Apply permutation p on elements of the set X (calculating new relation R' = p∘R∘p⁻¹)"

                                -- TODO next/previous button to enumerate all permutations
                                ]
                            , permError
                            ]
                        ]
                    , details "Formal Concept Analysis (FCA)"
                        [ Html.div [] <|
                            let
                                concepts =
                                    model.derivedInfo.formalConcepts
                            in
                            [ Html.text <|
                                "formal concepts (total "
                                    ++ String.fromInt (List.length concepts)
                                    ++ "): "
                            , Html.ul [ A.class "concept-list" ] <|
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
                    , details "Order Theory"
                        [ Html.div []
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
                    ]
                ]
            , Html.div [ A.class "column" ]
                [ Html.section [ A.class "panel" ]
                    [ Html.h2 [] [ Html.text "Graph" ]
                    , graphControls model
                    , Html.div [ A.id "graph" ]
                        [{- This is where viz.js renders svg graph -}]
                    ]
                ]
            ]
        ]


details : String -> List (Html msg) -> Html msg
details title body =
    Html.details []
        [ Html.summary [] [ Html.text title ]
        , Html.div [ A.class "details-body" ] body
        ]


infoHint : String -> Html msg
infoHint title =
    Html.span [ A.class "info", A.title title ] [ Html.text "i" ]


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
      , genRandom = Just GenTransitive
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
      , oeisId = Just "A083667"
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

      -- TODO acyclic OEIS? Don't just use A003024 (labeled DAGs): that counts DAGs without self-loops,
      -- but our isAcyclic ignores self-loops (findCycle skips the diagonal) and genAcyclic generates them,
      -- so the count should be 2^n * A003024(n). Either find OEIS sequence for that, or decide to disallow self-loops.
      , oeisId = Nothing
      }
    , { propertyName = "Functional"
      , wikiLink = "https://en.wikipedia.org/wiki/Binary_relation#Types_of_binary_relations"
      , hasProperty = Rel.isFunctional
      , buttons = []
      , genRandom = Just GenFunctional
      , toggleExplanation = Just ExplainFunctional
      , countFormula = Just Count.functionalOrInjective
      , oeisId = Just "A000169"
      }
    , { propertyName = "(Left-)Total"
      , wikiLink = "https://en.wikipedia.org/wiki/Total_relation"
      , hasProperty = Rel.isLeftTotal
      , buttons = []
      , genRandom = Just GenLeftTotal
      , toggleExplanation = Just ExplainLeftTotal
      , countFormula = Just Count.leftTotalOrSurjective
      , oeisId = Just "A055601"
      }
    , { propertyName = "Injective"
      , wikiLink = "https://en.wikipedia.org/wiki/Binary_relation#Types_of_binary_relations"
      , hasProperty = Rel.isInjective
      , buttons = []
      , genRandom = Just GenInjective
      , toggleExplanation = Just ExplainInjective
      , countFormula = Just Count.functionalOrInjective
      , oeisId = Just "A000169"
      }
    , { propertyName = "Surjective"
      , wikiLink = "https://en.wikipedia.org/wiki/Binary_relation#Types_of_binary_relations"
      , hasProperty = Rel.isSurjective
      , buttons = []
      , genRandom = Just GenSurjective
      , toggleExplanation = Just ExplainSurjective
      , countFormula = Just Count.leftTotalOrSurjective
      , oeisId = Just "A055601"
      }
    , { propertyName = "Bijection (Permutation)"
      , wikiLink = "https://en.wikipedia.org/wiki/Bijection"
      , hasProperty = Rel.isBijectiveFunction
      , buttons = []
      , genRandom = Just GenBijectiveFunction
      , toggleExplanation = Just ExplainBijective
      , countFormula = Just Count.totalOrderOrPermutation
      , oeisId = Just "A000142"
      }
    , { propertyName = "Derangement"
      , wikiLink = "https://en.wikipedia.org/wiki/Derangement"
      , hasProperty = Rel.isDerangement
      , buttons = []
      , genRandom = Just GenDerangement

      -- TODO explain why not Derangement
      , toggleExplanation = Nothing
      , countFormula = Just Count.derangement
      , oeisId = Just "A000166"
      }
    , { propertyName = "Involution"
      , wikiLink = "https://en.wikipedia.org/wiki/Involution_(mathematics)"
      , hasProperty = Rel.isInvolution
      , buttons = []
      , genRandom = Just GenInvolution

      -- TODO explain why not involution
      , toggleExplanation = Nothing
      , countFormula = Just Count.involution
      , oeisId = Just "A000085"
      }
    , { propertyName = "Equivalence"
      , wikiLink = "https://en.wikipedia.org/wiki/Equivalence_relation"
      , hasProperty = Rel.isEquivalence
      , buttons = [ ButtonConfig "Closure" DoEquivalenceClosure Rel.isEquivalence ]
      , genRandom = Just GenEquivalence
      , toggleExplanation = Just ExplainEquivalence
      , countFormula = Just Count.equivalence
      , oeisId = Just "A000110"
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
      , genRandom = Just GenPartialOrder

      -- TODO explain why not total order
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

      -- TODO explain why not total order
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
                , Html.td [ A.class "center" ] [ yesNo hasProp ]
                , Html.td [ A.class "center" ] <|
                    case toggleExplanation of
                        Just thisProperty ->
                            let
                                isActive =
                                    activeProperty == Just thisProperty

                                ( onClick, title ) =
                                    if isActive then
                                        ( HideExplanation, "Hide explanation" )

                                    else
                                        ( ShowExplanation thisProperty, "Show explanation" )
                            in
                            [ Html.button
                                [ E.onClick onClick
                                , A.title title
                                , A.classList [ ( "icon-button", True ), ( "active", isActive ) ]
                                ]
                                [ Html.text "?" ]
                            ]

                        Nothing ->
                            []
                , Html.td [ A.class "center" ] <|
                    case genRandom of
                        Just genMsg ->
                            let
                                -- e.g. there are no derangements of 1 element set
                                noneExist =
                                    Maybe.map (\cnt -> Natural.isZero (cnt derivedInfo.relSize)) countFormula
                                        |> Maybe.withDefault False
                            in
                            [ Html.button
                                [ E.onClick genMsg
                                , A.class "icon-button dice"
                                , A.title <|
                                    if noneExist then
                                        "There are no relations with this property"

                                    else
                                        "Generate random relation with this property"
                                , A.disabled noneExist
                                ]
                                [ Html.text "⚄" ]
                            ]

                        Nothing ->
                            []
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
                , Html.td [ A.class "num" ]
                    [ case countFormula of
                        Just cnt ->
                            Html.text <| Natural.toString <| cnt derivedInfo.relSize

                        Nothing ->
                            Html.span [ A.class "muted", A.title "Unknown" ] [ Html.text "?" ]
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
    Html.table [ A.class "props-table" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Property" ]
                , Html.th [ A.class "center", A.title "Does R have this property?" ] [ Html.text "Holds" ]
                , Html.th [ A.class "center", A.title "Explain why R has (or doesn't have) this property" ] [ Html.text "Explain" ]
                , Html.th [ A.class "center", A.title "Generate random relation with this property" ] [ Html.text "Random" ]
                , Html.th [] [ Html.text "Operations" ]
                , Html.th [ A.class "num", A.title "Number of relations with this property on a set of this size" ] [ Html.text "Count" ]
                , Html.th [] [ Html.text "OEIS" ]
                ]
            ]
        , Html.tbody [] <| List.map row propertyConfigs
        ]


blankLink : String -> String -> Html msg
blankLink href text =
    Html.a [ A.href href, A.target "_blank" ] [ Html.text text ]


yesNo : Bool -> Html msg
yesNo b =
    let
        ( class, label ) =
            if b then
                ( "yes", "✓" )

            else
                ( "no", "✗" )
    in
    Html.span
        [ A.class class ]
        [ Html.text label ]


sizeInputView : Int -> Html Msg
sizeInputView relSize =
    Html.div []
        [ Html.label [ A.class "slider-label" ]
            [ Html.span [ A.class "slider-value" ]
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
    Html.div [ A.class "muted" ]
        [ Html.label [ A.class "slider-label", A.title "Probability of including each pair when generating random relations" ]
            [ Html.span [ A.class "slider-value" ]
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
    Html.div [ A.class "graph-options" ]
        [ Html.div []
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
                    , Html.text "Highlight SCCs"
                    , infoHint "Strongly Connected Components"
                    ]
                ]
            ]
        , Html.div []
            [ radio "Bipartite graph" graphMode BipartiteGraph True
            ]
        , Html.div []
            [ radio "Hasse diagram" graphMode HasseDiagram (Rel.isAcyclic derivedInfo)
            , infoHint "Only available for acyclic relations"
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
        [ Html.button [ E.onClick PreviousRelation, A.title "Previous relation", A.class "icon-button" ] [ Html.text "‹" ]
        , Html.span [ A.class "pager-label" ]
            [ Html.text <|
                "Relation "
                    ++ Natural.toString (Rel.toRelIndex rel)
                    ++ " of "
                    ++ Natural.toString (Count.rel <| Rel.size rel)
            ]
        , Html.button [ E.onClick NextRelation, A.title "Next relation", A.class "icon-button" ] [ Html.text "›" ]
        ]


miscControls : Html Msg
miscControls =
    Html.div [ A.class "control-row" ]
        [ Html.button [ E.onClick UndoHistory, A.title "Undo previous edits" ] [ Html.text "Undo" ]
        , -- TODO think about how to decompose this hodgepodge of controls
          Html.button [ E.onClick MakeEmpty, A.title "Empty relation" ] [ Html.text "Clear" ]
        , Html.button [ E.onClick DoComplement ] [ Html.text "Complement" ]
        , Html.button [ E.onClick DoConverse ] [ Html.text "Converse" ]
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
