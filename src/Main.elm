module Main exposing (main)

import Browser exposing (element)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Lib.Input as Input exposing (Input)
import Lib.Output exposing (Output)
import Lib.Solver as Solver exposing (Solver, Solvers)
import Solvers.Day01 as Day01


solvers : Solvers
solvers =
    Solver.fromList
        [ Day01.partOne
        , Day01.partTwo
        ]


main : Program () Model Msg
main =
    element
        { init = always init
        , update = \a b -> ( update a b, Cmd.none )
        , subscriptions = always Sub.none
        , view = view
        }


type alias Model =
    { input : Input
    , output : Output
    , solver : Maybe ( String, Solver )
    }


type Msg
    = OnInput Input
    | OnSolverSelect String
    | Solve


init : ( Model, Cmd msg )
init =
    ( { output = Ok ""
      , input = Input.fromString ""
      , solver = Nothing
      }
    , Cmd.none
    )


update :
    Msg
    -> Model
    -> Model
update msg model =
    case msg of
        OnInput input ->
            { model | input = input }

        OnSolverSelect key ->
            { model | solver = Solver.get key solvers |> Maybe.map (\a -> ( key, a )) }

        Solve ->
            case model.solver of
                Nothing ->
                    { model | output = Err "Select a solver to run" }

                Just ( _, solver ) ->
                    { model | output = Solver.run solver model.input }


view :
    Model
    -> Html Msg
view model =
    Html.div [ Attr.class "Wrapper" ]
        [ Html.h1 [] [ Html.text "Advent Of Code 2019" ]
        , Html.p []
            [ Html.text "These are the "
            , Html.a [ Attr.href "https://adventofcode.com/2019" ] [ Html.text "Advent Of Code 2019" ]
            , Html.text " solutions from "
            , Html.a [ Attr.href "https://github.com/everlaat" ] [ Html.text "Elvin Verlaat" ]
            , Html.text "."
            ]
        , Html.p []
            [ Html.text "Check out "
            , Html.a [ Attr.href "https://github.com/everlaat/advent-of-code-2019" ] [ Html.text "the source" ]
            , Html.text " at Github. "
            ]
        , Html.div [ Attr.class "Wrapper_Input" ]
            [ Html.textarea
                [ Event.onInput (OnInput << Input.fromString)
                , Attr.placeholder "Input"
                ]
                []
            ]
        , Html.div [ Attr.class "Wrapper_Solvers" ]
            [ Html.select [ Event.onInput OnSolverSelect ] <|
                (Html.option [] [ Html.text "" ] :: Solver.map (viewSolverOption model.solver) solvers)
            , Html.button [ Event.onClick Solve ]
                [ Html.text "run"
                ]
            ]
        , Html.div [ Attr.class "Wrapper_Output" ]
            [ case model.output of
                Err a ->
                    Html.div [ Attr.class "Output_Error" ]
                        [ Html.pre [] [ Html.text a ]
                        ]

                Ok "" ->
                    Html.text ""

                Ok a ->
                    Html.div [ Attr.class "Output_Success" ]
                        [ Html.pre [] [ Html.text a ]
                        ]
            ]
        ]


viewSolverOption :
    Maybe ( String, Solver )
    -> String
    -> Solver
    -> Html msg
viewSolverOption maybeSolver label _ =
    let
        isSelected =
            Maybe.map (\( a, _ ) -> a == label) maybeSolver
                |> Maybe.withDefault False
    in
    Html.option [ Attr.value label, Attr.selected isSelected ]
        [ Html.text label
        ]
