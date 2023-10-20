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
    | NoOp


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

        NoOp ->
            model


relConfig : Rel.Config Msg
relConfig =
    { toggle = ToggleRel }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.node "style" [] [ Html.text style ]
        , viewSizeInput model
        , Rel.view relConfig model.rel
        , viewRelProperties model.rel
        ]


viewRelProperties : Rel -> Html Msg
viewRelProperties rel =
    Html.div []
        [ Html.div []
            -- TODO add wikipedia link https://en.wikipedia.org/wiki/Reflexive_relation
            [ Html.text <| "Is reflexive: " ++ yesNo (Rel.isReflexive rel) ]
        , Html.div []
            -- TODO add wikipedia link https://en.wikipedia.org/wiki/Symmetric_relation
            [ Html.text <| "Is symmetric: " ++ yesNo (Rel.isSymmetric rel) ]
        , Html.div []
            -- TODO add wikipedia link https://en.wikipedia.org/wiki/Antisymmetric_relation
            [ Html.text <| "Is antisymmetric: " ++ yesNo (Rel.isAntisymmetric rel) ]
        , Html.div []
            -- TODO add wikipedia link https://en.wikipedia.org/wiki/Transitive_relation
            [ Html.text <| "Is transitive: " ++ yesNo (Rel.isTransitive rel) ]
        ]


yesNo : Bool -> String
yesNo b =
    if b then
        "Yes"

    else
        "No"


viewSizeInput : Model -> Html Msg
viewSizeInput model =
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
