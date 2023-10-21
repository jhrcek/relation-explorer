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
        , subscriptions = \_ -> Sub.none
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
    | GenRandomRel
    | GenRandomFunction
    | GenRandomBijectiveFunction
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

        GenRandomRel ->
            ( model
            , Random.generate GotRandom <|
                -- TODO make True-bias configurable via a slider
                Rel.genRelation 0.5 (Rel.size model.rel)
            )

        GenRandomFunction ->
            ( model
            , Random.generate GotRandom <|
                Rel.genFunction (Rel.size model.rel)
            )

        GenRandomBijectiveFunction ->
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
        isReflexive =
            Rel.isReflexive rel

        isSymmetric =
            Rel.isSymmetric rel

        isTransitive =
            Rel.isTransitive rel
    in
    -- TODO refactor this UI so that Yes/No, Closure, Random and count are in separate columns
    Html.div []
        [ Html.div []
            [ Html.text "Is "
            , blankLink "https://en.wikipedia.org/wiki/Reflexive_relation" "relation"
            , Html.text ": Yes "
            , Html.button [ E.onClick GenRandomRel, A.title "Generate random relation" ] [ Html.text "⚄" ]
            ]
        , Html.div []
            [ Html.text "Is "
            , blankLink "https://en.wikipedia.org/wiki/Reflexive_relation" "reflexive"
            , Html.text <| ": " ++ yesNo isReflexive ++ " "
            , Html.button [ E.onClick DoReflexiveClosure, A.disabled isReflexive ]
                [ Html.text "Reflexive Closure" ]
            ]
        , Html.div []
            [ Html.text "Is "
            , blankLink "https://en.wikipedia.org/wiki/Reflexive_relation#Irreflexivity" "irreflexive"
            , Html.text <| ": " ++ yesNo (Rel.isIrreflexive rel)
            ]
        , Html.div []
            [ Html.text "Is "
            , blankLink "https://en.wikipedia.org/wiki/Symmetric_relation" "symmetric"
            , Html.text <| ": " ++ yesNo isSymmetric ++ " "
            , Html.button [ E.onClick DoSymmetricClosure, A.disabled isSymmetric ]
                [ Html.text "Symmetric Closure" ]
            ]
        , Html.div []
            [ Html.text "Is "
            , blankLink "https://en.wikipedia.org/wiki/Antisymmetric_relation" "antisymmetric"
            , Html.text <| ": " ++ yesNo (Rel.isAntisymmetric rel)
            ]
        , Html.div []
            [ Html.text "Is "
            , blankLink "https://en.wikipedia.org/wiki/Asymmetric_relation" "assymetric"
            , Html.text <| ": " ++ yesNo (Rel.isAsymmetric rel)
            ]
        , Html.div []
            [ Html.text "Is "
            , blankLink "https://en.wikipedia.org/wiki/Transitive_relation" "transitive"
            , Html.text <| ": " ++ yesNo isTransitive ++ " "
            , Html.button [ E.onClick DoTransitiveClosure, A.disabled isTransitive ]
                [ Html.text "Transitive Closure" ]
            ]
        , Html.div []
            [ Html.text "Is "
            , blankLink "https://en.wikipedia.org/wiki/Connected_relation" "connected"
            , Html.text <| ": " ++ yesNo (Rel.isConnected rel)
            ]
        , Html.div []
            [ Html.text "Is "
            , blankLink "https://en.wikipedia.org/wiki/Function_(mathematics)" "function"
            , Html.text <| ": " ++ yesNo (Rel.isFunction rel) ++ " "
            , Html.button [ E.onClick GenRandomFunction, A.title "Generate random function" ] [ Html.text "⚄" ]
            ]
        , -- TODO nest this under function
          Html.div []
            [ Html.text "Is "
            , blankLink "https://en.wikipedia.org/wiki/Bijection" "bijection"
            , Html.text <| ": " ++ yesNo (Rel.isBijectiveFunction rel) ++ " "
            , Html.button [ E.onClick GenRandomBijectiveFunction, A.title "Generate random bijective function" ] [ Html.text "⚄" ]
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
        , Html.text <| " " ++ String.fromInt relSize
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
