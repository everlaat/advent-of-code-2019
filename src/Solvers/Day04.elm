module Solvers.Day04 exposing (partOne, partTwo, tests)

import Expect
import Lib.Input as Input exposing (Input)
import Lib.Output exposing (Output)
import Lib.Solver exposing (Solver, make)
import Test exposing (Test, describe, test)


partOne : ( String, Solver )
partOne =
    ( "Day 04, Part One"
    , make runPartOne
    )


partTwo : ( String, Solver )
partTwo =
    ( "Day 04, Part Two"
    , make runPartOne
    )


tests : List Test
tests =
    [ describe "Part One"
        [ test "111111 meets these criteria (double 11, never decreases)."
            (\_ ->
                Expect.equal (validatePassword (Password [ 1, 1, 1, 1, 1, 1 ])) True
            )
        , test "223450 does not meet these criteria (decreasing pair of digits 50)."
            (\_ ->
                Expect.equal (validatePassword (Password [ 2, 2, 3, 4, 5, 0 ])) False
            )
        , test "123789 does not meet these criteria (no double)."
            (\_ ->
                Expect.equal (validatePassword (Password [ 1, 2, 3, 7, 8, 9 ])) False
            )
        ]
    ]


runPartOne : Input -> Output
runPartOne input =
    case String.split "-" <| Input.toString input of
        [ start, end ] ->
            Maybe.map2
                (\intStart intEnd ->
                    List.range intStart intEnd
                        |> List.map intToPassword
                        |> List.filter validatePassword
                        |> List.length
                        |> String.fromInt
                )
                (String.toInt start)
                (String.toInt end)
                |> Result.fromMaybe "couldn't convert start and/or end in to ints"

        _ ->
            Err "Input is invalid."


type Password
    = Password (List Int)


validatePassword : Password -> Bool
validatePassword password =
    List.map (\f -> f password)
        [ passwordLengthIsSix
        , hasTwoAdjacentDigits
        , neverDecreses
        ]
        |> List.all ((==) True)


passwordLengthIsSix : Password -> Bool
passwordLengthIsSix (Password digits) =
    List.length digits == 6


hasTwoAdjacentDigits : Password -> Bool
hasTwoAdjacentDigits (Password digits) =
    List.foldl
        (\a ( mPrevious, result ) ->
            Maybe.map
                (\b ->
                    if not result && a == b then
                        ( Just a, True )

                    else
                        ( Just a, result )
                )
                mPrevious
                |> Maybe.withDefault ( Just a, result )
        )
        ( Nothing, False )
        digits
        |> Tuple.second


neverDecreses : Password -> Bool
neverDecreses (Password digits) =
    List.foldl
        (\a ( mPrevious, result ) ->
            Maybe.map
                (\b ->
                    if result && a >= b then
                        ( Just a, True )

                    else
                        ( Just a, False )
                )
                mPrevious
                |> Maybe.withDefault ( Just a, result )
        )
        ( Nothing, True )
        digits
        |> Tuple.second


intToPassword : Int -> Password
intToPassword int =
    String.fromInt int
        |> String.split ""
        |> List.filterMap String.toInt
        |> Password


passwordToString : Password -> String
passwordToString (Password digits) =
    String.join "" <| List.map String.fromInt digits
