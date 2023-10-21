module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Random
import Rel exposing (Rel)


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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initSize =
            4
    in
    ( { rel = Rel.empty initSize }
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
    | GenRel
    | GenReflexive
    | GenFunction
    | GenBijectiveFunction
    | GotRandom Rel


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
        , Rel.view relConfig model.rel
        , Html.text <| "Set of elements: " ++ Rel.showElements model.rel
        , elementaryPropertiesView model.rel
        , Html.hr [] []
        , operationsView
        ]


elementaryPropertiesView : Rel -> Html Msg
elementaryPropertiesView rel =
    let
        row { propertyName, wikiLink, hasProperty, closureButton, genRandom } =
            let
                hasProp =
                    hasProperty rel
            in
            Html.tr []
                [ Html.td [] [ blankLink wikiLink propertyName ]
                , Html.td [] [ Html.text <| yesNo hasProp ]
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
        , Html.tbody [] <|
            List.map row
                [ { propertyName = "Relation"
                  , wikiLink = "https://en.wikipedia.org/wiki/Relation_(mathematics)"
                  , hasProperty = always True
                  , closureButton = Nothing
                  , genRandom = Just GenRel
                  }
                , { propertyName = "Reflexive"
                  , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation"
                  , hasProperty = Rel.isReflexive
                  , closureButton = Just DoReflexiveClosure
                  , genRandom = Just GenReflexive
                  }
                , { propertyName = "Irreflexive"
                  , wikiLink = "https://en.wikipedia.org/wiki/Reflexive_relation#Irreflexivity"
                  , hasProperty = Rel.isIrreflexive
                  , closureButton = Nothing
                  , genRandom = Nothing
                  }
                , { propertyName = "Symmetric"
                  , wikiLink = "https://en.wikipedia.org/wiki/Symmetric_relation"
                  , hasProperty = Rel.isSymmetric
                  , closureButton = Just DoSymmetricClosure
                  , genRandom = Nothing
                  }
                , { propertyName = "Antisymmetric"
                  , wikiLink = "https://en.wikipedia.org/wiki/Antisymmetric_relation"
                  , hasProperty = Rel.isAntisymmetric
                  , closureButton = Nothing
                  , genRandom = Nothing
                  }
                , { propertyName = "Assymetric"
                  , wikiLink = "https://en.wikipedia.org/wiki/Asymmetric_relation"
                  , hasProperty = Rel.isAsymmetric
                  , closureButton = Nothing
                  , genRandom = Nothing
                  }
                , { propertyName = "Transitive"
                  , wikiLink = "https://en.wikipedia.org/wiki/Transitive_relation"
                  , hasProperty = Rel.isTransitive
                  , closureButton = Just DoTransitiveClosure
                  , genRandom = Nothing
                  }
                , { propertyName = "Connected"
                  , wikiLink = "https://en.wikipedia.org/wiki/Connected_relation"
                  , hasProperty = Rel.isConnected
                  , closureButton = Nothing
                  , genRandom = Nothing
                  }
                , { propertyName = "Function"
                  , wikiLink = "https://en.wikipedia.org/wiki/Function_(mathematics)"
                  , hasProperty = Rel.isFunction
                  , closureButton = Nothing
                  , genRandom = Just GenFunction
                  }
                , { propertyName = "Bijection"
                  , wikiLink = "https://en.wikipedia.org/wiki/Bijection"
                  , hasProperty = Rel.isBijectiveFunction
                  , closureButton = Nothing
                  , genRandom = Just GenBijectiveFunction
                  }
                ]
        ]


blankLink : String -> String -> Html msg
blankLink href text =
    Html.a [ A.href href, A.target "_blank" ] [ Html.text text ]


yesNo : Bool -> String
yesNo b =
    if b then
        "Yes"

    else
        "No"


sizeInputView : Model -> Html Msg
sizeInputView model =
    let
        relSize =
            Rel.size model.rel
    in
    Html.div []
        [ Html.label []
            [ Html.text "Relation size"
            , Html.input
                [ A.type_ "range"
                , A.min "1"
                , A.max "10"
                , E.onInput <| SetRelSize << Maybe.withDefault 3 << String.toInt
                , A.value <| String.fromInt relSize
                ]
                []
            ]
        , Html.text <| String.fromInt relSize
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
    margin: 20px;
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
"""
