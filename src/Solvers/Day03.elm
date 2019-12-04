module Solvers.Day03 exposing (partOne, partTwo, tests)

import Expect
import Lib.Input as Input exposing (Input)
import Lib.Output exposing (Output)
import Lib.Solver exposing (Solver, make)
import Set exposing (Set)
import Test exposing (Test, describe, test)


partOne : ( String, Solver )
partOne =
    ( "Day 03, Part One"
    , make runPartOne
    )


partTwo : ( String, Solver )
partTwo =
    ( "Day 03, Part Two"
    , make runPartOne
    )


tests : List Test
tests =
    [ describe "Part One" []
    ]


type Wire
    = Wire (Set Coordinate)


type alias Coordinate =
    ( Int, Int )


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


getClosestCrossingManhattanDistance :
    Wire
    -> Wire
    -> Maybe Int
getClosestCrossingManhattanDistance (Wire wireA) (Wire wireB) =
    Set.intersect wireA wireB
        |> Set.map (manhattanDistance ( 0, 0 ))
        |> Set.toList
        |> List.sort
        |> List.head


manhattanDistance : Coordinate -> Coordinate -> Int
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
                    ( Wire Set.empty, ( 0, 0 ) )
            )
        >> Result.map
            Tuple.first


operateWire :
    Operation
    -> ( Wire, Coordinate )
    -> ( Wire, Coordinate )
operateWire operation ( Wire setWire, startPosition ) =
    List.repeat (operationToDistance operation) 0
        |> List.foldl
            (\_ ( set, lastPosition ) ->
                let
                    newPosition =
                        calcNewPosition operation lastPosition
                in
                ( Set.insert newPosition set, newPosition )
            )
            ( Set.empty, startPosition )
        |> (\( newPath, lastPosition ) ->
                ( Wire (Set.union newPath setWire), lastPosition )
           )


operationToDistance : Operation -> Int
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


calcNewPosition : Operation -> Coordinate -> Coordinate
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
