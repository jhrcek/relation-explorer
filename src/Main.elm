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
    }


type alias Explanation =
    { highlight : Set Rel.Pair
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
      }
    , Cmd.none
    )


type Msg
    = SetRelSize Int
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
    | GenFunction
    | GenBijectiveFunction
    | GotRandom Rel
      -- Explanations
    | HideExplanations
    | ExplainWhyNotReflexive
    | ExplainWhyNotIrreflexive
    | ExplainWhyNotSymmetric
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

        ToggleRel i j ->
            pure { model | rel = Rel.toggle i j model.rel }

        DoReflexiveClosure ->
            pure { model | rel = Rel.reflexiveClosure model.rel }

        DoSymmetricClosure ->
            pure { model | rel = Rel.symmetricClosure model.rel }

        DoTransitiveClosure ->
            let
                ( transitiveRel, history ) =
                    Rel.transitiveClosure model.rel

                _ =
                    Debug.log
                        (List.map Rel.showElements (transitiveRel :: history)
                            |> List.reverse
                            |> String.join "\n"
                        )
                        ()
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
                -- TODO make True-bias configurable via a slider
                Rel.genRelation 0.5 (Rel.size model.rel)
            )

        GenReflexive ->
            ( model
            , Random.generate GotRandom <|
                -- TODO make True-bias configurable via a slider
                Rel.genReflexiveRelation 0.5 (Rel.size model.rel)
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
                                , "Whenever relation contains an element (x,y), it also needs to contain (y,x) to be symmetric."

                                -- TODO I'd like the explanation to be more granular, to make it clearer.
                                -- something like: "since we have (1,2), we alson need (2,1) ..."
                                , "The following elements would have to be added to satisfy that condition: " ++ Rel.showPairSet missing
                                ]
                            }
                }

        NoOp ->
            pure model


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
        , sizeInputView model
        , Html.div [ A.id "rel-and-explanation" ]
            [ let
                highlight =
                    case model.explanation of
                        Nothing ->
                            Set.empty

                        Just exp ->
                            exp.highlight
              in
              Rel.view relConfig model.rel highlight
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
        , elementaryPropertiesView model.rel
        , Html.hr [] []
        , operationsView
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
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Assymetric"
      , wikiLink = "https://en.wikipedia.org/wiki/Asymmetric_relation"
      , hasProperty = Rel.isAsymmetric
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Transitive"
      , wikiLink = "https://en.wikipedia.org/wiki/Transitive_relation"
      , hasProperty = Rel.isTransitive
      , closureButton = Just DoTransitiveClosure
      , genRandom = Nothing
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Connected"
      , wikiLink = "https://en.wikipedia.org/wiki/Connected_relation"
      , hasProperty = Rel.isConnected
      , closureButton = Nothing
      , genRandom = Nothing
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Function"
      , wikiLink = "https://en.wikipedia.org/wiki/Function_(mathematics)"
      , hasProperty = Rel.isFunction
      , closureButton = Nothing
      , genRandom = Just GenFunction
      , onHoverExplanation = Nothing
      }
    , { propertyName = "Bijection"
      , wikiLink = "https://en.wikipedia.org/wiki/Bijection"
      , hasProperty = Rel.isBijectiveFunction
      , closureButton = Nothing
      , genRandom = Just GenBijectiveFunction
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
        -- Hiding and showing explanations on hover can cause jumping - for small relations and long explanations.
        -- TODO shuffle the UI arround to avoid this
        Html.span [ E.onMouseEnter onHover, E.onMouseLeave HideExplanations ] [ Html.text "No - ⓘ" ]


sizeInputView : Model -> Html Msg
sizeInputView model =
    let
        relSize =
            Rel.size model.rel
    in
    Html.div []
        [ Html.label []
            [ Html.text <| "|X| = " ++ String.fromInt relSize ++ " "
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


operationsView : Html Msg
operationsView =
    Html.div []
        [ Html.h4 [] [ Html.text "Operations" ]
        , Html.button [ E.onClick MakeEmpty, A.title "Empty relation" ] [ Html.text "∅" ]
        , Html.button [ E.onClick DoComplement ] [ Html.text "Complement" ]
        , Html.button [ E.onClick DoConverse ] [ Html.text "Converse" ]
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

#rel-and-explanation {
    display: flex;
    gap: 20px;
    padding: 20px;

    #explanation {
        max-width: 40%;
    }
}
"""
