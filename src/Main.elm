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
    | DoReflexiveReduction
    | DoSymmetricClosure
    | DoTransitiveClosure
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
    | GenPartialFunction
    | GenFunction
    | GenBijectiveFunction
    | GotRandom Rel
      -- Explanations
    | HideExplanations
    | ExplainRelation
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

        DoReflexiveReduction ->
            updateRel Rel.reflexiveReduction model

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

        GenIrreflexive ->
            generateRel (Rel.genIrreflexiveRelation model.trueProb) model

        GenSymmetric ->
            generateRel (Rel.genSymmetricRelation model.trueProb) model

        GenAntisymmetric ->
            generateRel (Rel.genAntisymmetricRelation model.trueProb) model

        GenAsymmetric ->
            generateRel (Rel.genAsymmetricRelation model.trueProb) model

        GenPartialFunction ->
            generateRel Rel.genPartialFunction model

        GenFunction ->
            generateRel Rel.genFunction model

        GenBijectiveFunction ->
            generateRel Rel.genBijectiveFunction model

        HideExplanations ->
            pure { model | explanation = Nothing }

        ExplainRelation ->
            pure { model | explanation = Just <| Rel.explainRelation model.derivedInfo }

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


readOnlyConfig : Rel.Config Msg
readOnlyConfig =
    { toggle = \_ _ -> NoOp }


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
                    , case model.derivedInfo.acyclic of
                        Ok acyclic ->
                            Html.div []
                                [ Html.text "This relation is acyclic"
                                , Html.div [ A.class "indent" ]
                                    [ Html.text <|
                                        "Topological sort: ["
                                            ++ String.join ", " (List.map String.fromInt <| Rel.topologicalSort acyclic)
                                            ++ "]"
                                    , Html.div [] [ Html.text "Transitive reduction: " ]
                                    , Html.div [] [ Rel.view readOnlyConfig (Rel.transitiveReduction acyclic) Nothing ]
                                    ]
                                ]

                        Err cycle ->
                            Html.div []
                                [ Html.text "This relation is not acyclic"
                                , Html.div [ A.class "indent" ]
                                    [ Html.text <|
                                        "It contains a cycle: ["
                                            ++ String.join ", " (List.map String.fromInt cycle)
                                            ++ "]"
                                    ]
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
    , buttons : List (ButtonConfig msg)
    , genRandom : Maybe msg
    , onHoverExplanation : Maybe msg
    }


type alias ButtonConfig msg =
    { label : String
    , message : msg
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
      , buttons = [ ButtonConfig "Closure" DoReflexiveClosure ]
      , genRandom = Just GenReflexive
      , onHoverExplanation = Just ExplainReflexive
      }
    , { propertyName = "Irreflexive"
      , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation#Irreflexivity"
      , hasProperty = Rel.isIrreflexive
      , buttons = [ ButtonConfig "Reduction" DoReflexiveReduction ]
      , genRandom = Just GenIrreflexive
      , onHoverExplanation = Just ExplainIrreflexive
      }
    , { propertyName = "Symmetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Symmetric_relation"
      , hasProperty = Rel.isSymmetric
      , buttons = [ ButtonConfig "Closure" DoSymmetricClosure ]
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
      , buttons = [ ButtonConfig "Closure" DoTransitiveClosure ]
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainTransitive
      }
    , { propertyName = "Connected"
      , wikiLink = "https://en.wikipedia.org/wiki/Connected_relation"
      , hasProperty = .isConnected
      , buttons = []
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainWhyNotConnected
      }
    , { propertyName = "Acyclic"
      , wikiLink = "https://en.wikipedia.org/wiki/Glossary_of_order_theory#A"
      , hasProperty = Rel.isAcyclic
      , buttons = []
      , genRandom = Nothing
      , onHoverExplanation = Just ExplainWhyNotAcyclic
      }
    , { propertyName = "Partial function"
      , wikiLink = "https://en.wikipedia.org/wiki/Partial_function"
      , hasProperty = .isPartialFunction
      , buttons = []
      , genRandom = Just GenPartialFunction
      , onHoverExplanation = Just ExplainWhyNotPartialFunction
      }
    , { propertyName = "Function"
      , wikiLink = "https://en.wikipedia.org/wiki/Function_(mathematics)"
      , hasProperty = Rel.isFunction
      , buttons = []
      , genRandom = Just GenFunction
      , onHoverExplanation = Just ExplainFunction
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
      , genRandom = Nothing

      -- TODO explain why not Derangement
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Involution"
      , wikiLink = "https://en.wikipedia.org/wiki/Involution_(mathematics)"
      , hasProperty = .isInvolution
      , buttons = []
      , genRandom = Nothing

      -- TODO explain why not involution
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
                , Html.td [] [ yesNo (Maybe.withDefault NoOp onHoverExplanation) hasProp ]
                , Html.td [] <|
                    List.map
                        (\butCfg ->
                            Html.button [ E.onClick butCfg.message, A.disabled hasProp ]
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
