module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Rel exposing (Rel)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { rel : Rel
    }


init : Model
init =
    let
        initSize =
            4
    in
    { rel = Rel.empty initSize
    }


type Msg
    = SetRelSize Int
    | ToggleRel Int Int
    | DoReflexiveClosure
    | DoSymmetricClosure
    | DoTransitiveClosure
    | DoComplement
    | DoConverse
    | MakeEmpty


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetRelSize newSize ->
            let
                safeSize =
                    clamp 1 10 newSize
            in
            { model | rel = Rel.resize safeSize model.rel }

        ToggleRel i j ->
            { model | rel = Rel.toggle i j model.rel }

        DoReflexiveClosure ->
            { model | rel = Rel.reflexiveClosure model.rel }

        DoSymmetricClosure ->
            { model | rel = Rel.symmetricClosure model.rel }

        DoTransitiveClosure ->
            { model | rel = Rel.transitiveClosure model.rel }

        DoComplement ->
            { model | rel = Rel.complement model.rel }

        DoConverse ->
            { model | rel = Rel.converse model.rel }

        MakeEmpty ->
            { model | rel = Rel.empty <| Rel.size model.rel }


relConfig : Rel.Config Msg
relConfig =
    { toggle = ToggleRel }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.node "style" [] [ Html.text style ]
        , sizeInputView model
        , Rel.view relConfig model.rel
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
    Html.div []
        [ Html.div []
            -- TODO add wikipedia link https://en.wikipedia.org/wiki/Reflexive_relation
            [ Html.text <| "Is reflexive: " ++ yesNo isReflexive ++ " "
            , Html.button [ E.onClick DoReflexiveClosure, A.disabled isReflexive ]
                [ Html.text "Reflexive Closure" ]
            ]
        , Html.div []
            -- TODO add wikipedia link https://en.wikipedia.org/wiki/Symmetric_relation
            [ Html.text <| "Is symmetric: " ++ yesNo isSymmetric ++ " "
            , Html.button [ E.onClick DoSymmetricClosure, A.disabled isSymmetric ]
                [ Html.text "Symmetric Closure" ]
            ]
        , Html.div []
            -- TODO add wikipedia link https://en.wikipedia.org/wiki/Antisymmetric_relation
            [ Html.text <| "Is antisymmetric: " ++ yesNo (Rel.isAntisymmetric rel) ]
        , Html.div []
            -- TODO add wikipedia link https://en.wikipedia.org/wiki/Transitive_relation
            [ Html.text <| "Is transitive: " ++ yesNo isTransitive ++ " "
            , Html.button [ E.onClick DoTransitiveClosure, A.disabled isTransitive ]
                [ Html.text "Transitive Closure" ]
            ]
        ]


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
        [ Html.label [] [ Html.text "Relation size " ]
        , Html.input
            [ A.type_ "range"
            , A.min "1"
            , A.max "10"
            , E.onInput <| SetRelSize << Maybe.withDefault 3 << String.toInt
            , A.value <| String.fromInt relSize
            ]
            []
        , Html.text <| " " ++ String.fromInt relSize
        ]


operationsView : Html Msg
operationsView =
    Html.div []
        [ Html.h4 [] [ Html.text "Operations" ]
        , Html.div [] [ Html.button [ E.onClick DoComplement ] [ Html.text "Complement" ] ]
        , Html.div [] [ Html.button [ E.onClick DoConverse ] [ Html.text "Converse" ] ]
        , Html.div [] [ Html.button [ E.onClick MakeEmpty ] [ Html.text "Empty" ] ]
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
"""
