module Solvers.Day02 exposing (partOne, partTwo, tests)

import Array exposing (Array)
import Expect
import Lib.Input as Input exposing (Input)
import Lib.Output exposing (Output)
import Lib.Solver exposing (Solver, make)
import Test exposing (Test, describe, test)


partOne : ( String, Solver )
partOne =
    ( "Day 02, Part One"
    , make runInput
    )


partTwo : ( String, Solver )
partTwo =
    ( "Day 02, Part Two"
    , make findInput
    )


tests : List Test
tests =
    [ describe "Part One" <|
        List.map
            (\( input, output ) ->
                test input
                    (\_ ->
                        Expect.equal (runInput <| Input.fromString input)
                            output
                    )
            )
            [ ( "1,9,10,3,2,3,11,0,99,30,40,50", Ok "3500,9,10,70,2,3,11,0,99,30,40,50" )
            , ( "1,0,0,0,99", Ok "2,0,0,0,99" )
            , ( "2,3,0,3,99", Ok "2,3,0,6,99" )
            , ( "2,4,4,5,99,0", Ok "2,4,4,5,99,9801" )
            , ( "1,1,1,4,99,5,6,0,99", Ok "30,1,1,4,2,5,6,0,99" )
            , ( "3,0,0,0,99", Err "unknown operation code 3" )
            , ( "1,0,10,11,99", Err "Error calculating value" )
            ]
    ]


type IntCodeProgram
    = IntCodeProgram (List IntCode)


type Memory
    = Memory (Array Int)


type IntCode
    = IntCode Int


type Pointer
    = Pointer Int


runInput : Input -> Output
runInput =
    inputToIntCodeProgram
        >> Result.andThen runIntCodeProgram
        >> Result.map intCodeProgramToString


findInput : Input -> Output
findInput =
    inputToIntCodeProgram
        >> Result.andThen (find 19690720)
        >> Result.map (\( noun, verb ) -> String.fromInt (100 * noun + verb))


runIntCodeProgram :
    IntCodeProgram
    -> Result String IntCodeProgram
runIntCodeProgram =
    intCodeProgramToMemory
        >> run pointerStart
        >> Result.map memoryToIntCodeProgram


find :
    Int
    -> IntCodeProgram
    -> Result String ( Int, Int )
find query =
    intCodeProgramToMemory
        >> find_ ( 0, 0 ) query


find_ : ( Int, Int ) -> Int -> Memory -> Result String ( Int, Int )
find_ ( noun, verb ) query ((Memory m) as memory) =
    (Array.set 1 noun m
        |> Array.set 2 verb
        |> Memory
    )
        |> run (Pointer 0)
        |> Result.andThen
            (\(Memory result) ->
                if Array.get 0 result == Just query then
                    Ok ( noun, verb )

                else if noun == 99 && verb == 99 then
                    Err "out of bounds"

                else if noun == 99 then
                    find_ ( 0, verb + 1 ) query memory

                else
                    find_ ( noun + 1, verb ) query memory
            )


run :
    Pointer
    -> Memory
    -> Result String Memory
run ((Pointer p) as pointer) ((Memory m) as memory) =
    case Array.get p m of
        Just 1 ->
            runOperator (+) pointer memory
                |> Result.andThen (run (pointerNextOperation pointer))

        Just 2 ->
            runOperator (*) pointer memory
                |> Result.andThen (run (pointerNextOperation pointer))

        Just 99 ->
            Ok memory

        Just x ->
            Err ("unknown operation code " ++ String.fromInt x)

        Nothing ->
            Err ("Pointer out of bounds " ++ String.fromInt p)


runOperator :
    (Int -> Int -> Int)
    -> Pointer
    -> Memory
    -> Result String Memory
runOperator f (Pointer p) (Memory m) =
    Maybe.map3
        (\a b c ->
            calcOperator f (Pointer a) (Pointer b) (Memory m)
                |> Maybe.map (\value -> Memory <| Array.set c value m)
        )
        (Array.get (p + 1) m)
        (Array.get (p + 2) m)
        (Array.get (p + 3) m)
        |> Maybe.map
            (\m_a ->
                case m_a of
                    Just a ->
                        Ok a

                    Nothing ->
                        Err "Error calculating value"
            )
        |> Maybe.withDefault (Err "Unable to read out all positions")


pointerStart : Pointer
pointerStart =
    Pointer 0


pointerNextOperation : Pointer -> Pointer
pointerNextOperation (Pointer a) =
    Pointer (a + 4)


intCodeProgramToMemory :
    IntCodeProgram
    -> Memory
intCodeProgramToMemory (IntCodeProgram listOfIntCodes) =
    List.map (\(IntCode a) -> a) listOfIntCodes
        |> Array.fromList
        |> Memory


memoryToIntCodeProgram : Memory -> IntCodeProgram
memoryToIntCodeProgram (Memory memory) =
    Array.toList memory
        |> List.map IntCode
        |> IntCodeProgram


calcOperator :
    (Int -> Int -> Int)
    -> Pointer
    -> Pointer
    -> Memory
    -> Maybe Int
calcOperator f (Pointer a) (Pointer b) (Memory m) =
    Maybe.map2 f
        (Array.get a m)
        (Array.get b m)


intCodeProgramToString :
    IntCodeProgram
    -> String
intCodeProgramToString (IntCodeProgram listOfIntCode) =
    List.map (\(IntCode int) -> String.fromInt int) listOfIntCode
        |> String.join ","


intCodeFromString :
    String
    -> Result String IntCode
intCodeFromString a =
    String.toInt a
        |> Maybe.map IntCode
        |> Result.fromMaybe ("Can't convert " ++ a ++ " in to a Int")


inputToIntCodeProgram :
    Input
    -> Result String IntCodeProgram
inputToIntCodeProgram =
    Input.toString
        >> String.split ","
        >> List.map (intCodeFromString << String.trim)
        >> List.foldl
            (\resultIntCode result ->
                case ( resultIntCode, result ) of
                    ( _, Err err ) ->
                        Err err

                    ( Ok intCode, Ok r ) ->
                        Ok <| intCode :: r

                    ( Err err, _ ) ->
                        Err err
            )
            (Ok [])
        >> Result.map (IntCodeProgram << List.reverse)
