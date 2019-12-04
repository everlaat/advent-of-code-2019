module Solvers.Day03 exposing (partOne, partTwo, tests)

import Dict exposing (Dict)
import Expect
import Lib.Input as Input exposing (Input)
import Lib.Output exposing (Output)
import Lib.Solver exposing (Solver, make)
import Test exposing (Test, describe, test)


partOne : ( String, Solver )
partOne =
    ( "Day 03, Part One"
    , make runPartOne
    )


partTwo : ( String, Solver )
partTwo =
    ( "Day 03, Part Two"
    , make runPartTwo
    )


tests : List Test
tests =
    [ describe "Part One" <|
        List.map
            (\( a, b ) ->
                test ("distance should be " ++ b)
                    (\_ ->
                        Expect.equal
                            (runPartOne (Input.fromString a))
                            (Ok b)
                    )
            )
            [ ( """R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83""", "159" )
            , ( """R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""", "135" )
            ]
    , describe "Part Two" <|
        List.map
            (\( a, b ) ->
                test ("steps should be " ++ b)
                    (\_ ->
                        Expect.equal
                            (runPartTwo (Input.fromString a))
                            (Ok b)
                    )
            )
            [ ( """R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83""", "610" )
            , ( """R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""", "410" )
            ]
    ]


type Wire
    = Wire (Dict Coordinate Distance)


type alias Coordinate =
    ( Int, Int )


type alias Distance =
    Int


type Operation
    = Up Int
    | Right Int
    | Left Int
    | Down Int


runPartOne : Input -> Output
runPartOne input =
    case Input.toString input |> String.lines of
        [ a, b ] ->
            Result.map2
                (\wa wb ->
                    getClosestCrossingManhattanDistance wa wb
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault "?"
                )
                (parseWire a)
                (parseWire b)

        _ ->
            Err "Given input isn't valid."


runPartTwo : Input -> Output
runPartTwo input =
    case Input.toString input |> String.lines of
        [ a, b ] ->
            Result.map2
                (\wa wb ->
                    getClosestCrossingFewestSteps wa wb
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault "?"
                )
                (parseWire a)
                (parseWire b)

        _ ->
            Err "Given input isn't valid."


getClosestCrossingManhattanDistance :
    Wire
    -> Wire
    -> Maybe Int
getClosestCrossingManhattanDistance (Wire wireA) (Wire wireB) =
    Dict.intersect wireA wireB
        |> Dict.toList
        |> List.map
            (Tuple.first
                >> manhattanDistance ( 0, 0 )
            )
        |> List.sort
        |> List.head


getClosestCrossingFewestSteps :
    Wire
    -> Wire
    -> Maybe Int
getClosestCrossingFewestSteps (Wire wireA) (Wire wireB) =
    Dict.intersect wireA wireB
        |> Dict.toList
        |> List.filterMap
            (\( key, a ) ->
                Dict.get key wireB
                    |> Maybe.map ((+) a)
            )
        |> List.sort
        |> List.head


manhattanDistance :
    Coordinate
    -> Coordinate
    -> Int
manhattanDistance ( a, b ) ( c, d ) =
    abs a + abs b + abs c + abs d


stringToOperation :
    String
    -> Result String Operation
stringToOperation a =
    let
        partDirection =
            String.left 1 a |> String.toUpper

        partDistance =
            String.dropLeft 1 a |> String.toInt
    in
    case ( partDirection, partDistance ) of
        ( "U", Just distance ) ->
            Ok <| Up distance

        ( "R", Just distance ) ->
            Ok <| Right distance

        ( "D", Just distance ) ->
            Ok <| Down distance

        ( "L", Just distance ) ->
            Ok <| Left distance

        _ ->
            Err <| "'" ++ a ++ "' is not a valid operation"


parseWire :
    String
    -> Result String Wire
parseWire =
    String.split ","
        >> List.map stringToOperation
        >> List.foldl
            (\rOperation rOperations ->
                Result.map2
                    (\a b -> a :: b)
                    rOperation
                    rOperations
            )
            (Ok [])
        >> Result.map
            (List.reverse
                >> List.foldl
                    operateWire
                    ( Wire Dict.empty, ( 0, 0 ), 0 )
            )
        >> Result.map
            (\( a, _, _ ) ->
                a
            )


operateWire :
    Operation
    -> ( Wire, Coordinate, Distance )
    -> ( Wire, Coordinate, Distance )
operateWire operation ( Wire dictWire, startPosition, startDistance ) =
    List.repeat (operationToDistance operation) 0
        |> List.foldl
            (\_ ( dict, lastPosition, lastDistance ) ->
                let
                    newPosition =
                        calcNewPosition operation lastPosition

                    newDistance =
                        lastDistance + 1
                in
                ( Dict.insert newPosition newDistance dict, newPosition, newDistance )
            )
            ( Dict.empty, startPosition, startDistance )
        |> (\( newPath, lastPosition, lastDistance ) ->
                ( Wire (Dict.union newPath dictWire), lastPosition, lastDistance )
           )


operationToDistance :
    Operation
    -> Int
operationToDistance operation =
    case operation of
        Up distance ->
            distance

        Right distance ->
            distance

        Down distance ->
            distance

        Left distance ->
            distance


calcNewPosition :
    Operation
    -> Coordinate
    -> Coordinate
calcNewPosition operation ( x, y ) =
    case operation of
        Up _ ->
            ( x, y + 1 )

        Right _ ->
            ( x + 1, y )

        Down _ ->
            ( x, y - 1 )

        Left _ ->
            ( x - 1, y )
