module Solvers.Day04 exposing (partOne, partTwo, tests)

import Dict
import Expect
import Lib.Input as Input exposing (Input)
import Lib.Output exposing (Output)
import Lib.Solver exposing (Solver, make)
import Test exposing (Test, describe, test)


partOne : ( String, Solver )
partOne =
    ( "Day 04, Part One"
    , make (runInput validatePasswordPartOne)
    )


partTwo : ( String, Solver )
partTwo =
    ( "Day 04, Part Two"
    , make (runInput validatePasswordPartTwo)
    )


tests : List Test
tests =
    [ describe "Part One"
        [ test "111111 meets these criteria (double 11, never decreases)."
            (\_ ->
                Expect.equal (validatePasswordPartOne (Password [ 1, 1, 1, 1, 1, 1 ])) True
            )
        , test "223450 does not meet these criteria (decreasing pair of digits 50)."
            (\_ ->
                Expect.equal (validatePasswordPartOne (Password [ 2, 2, 3, 4, 5, 0 ])) False
            )
        , test "123789 does not meet these criteria (no double)."
            (\_ ->
                Expect.equal (validatePasswordPartOne (Password [ 1, 2, 3, 7, 8, 9 ])) False
            )
        ]
    , describe "Part Two"
        [ test "112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long."
            (\_ ->
                Expect.equal (validatePasswordPartTwo (Password [ 1, 1, 2, 2, 3, 3 ])) True
            )
        , test "123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444)"
            (\_ ->
                Expect.equal (validatePasswordPartTwo (Password [ 1, 2, 3, 4, 4, 4 ])) False
            )
        , test "111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22)."
            (\_ ->
                Expect.equal (validatePasswordPartTwo (Password [ 1, 1, 1, 1, 2, 2 ])) True
            )
        ]
    ]


runInput : (Password -> Bool) -> Input -> Output
runInput validator input =
    case String.split "-" <| Input.toString input of
        [ start, end ] ->
            Maybe.map2
                (\intStart intEnd ->
                    List.range intStart intEnd
                        |> List.map intToPassword
                        |> List.filter validator
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


validatePasswordPartOne : Password -> Bool
validatePasswordPartOne password =
    passwordLengthIsSix password && neverDecreses password && hasTwoAdjacentDigits password


validatePasswordPartTwo : Password -> Bool
validatePasswordPartTwo password =
    passwordLengthIsSix password && neverDecreses password && hasTwoAdjacentDigitsThatAreNotPartOfLargerGroup password


passwordLengthIsSix : Password -> Bool
passwordLengthIsSix (Password digits) =
    List.length digits == 6


hasTwoAdjacentDigits : Password -> Bool
hasTwoAdjacentDigits (Password digits) =
    List.foldl
        (\a dict ->
            Dict.get a dict
                |> Maybe.map (\count -> Dict.insert a (count + 1) dict)
                |> Maybe.withDefault (Dict.insert a 1 dict)
        )
        Dict.empty
        digits
        |> (Dict.values
                >> List.filter (\a -> a == 1)
                >> List.length
                >> (/=) 6
           )


hasTwoAdjacentDigitsThatAreNotPartOfLargerGroup : Password -> Bool
hasTwoAdjacentDigitsThatAreNotPartOfLargerGroup (Password digits) =
    List.foldl
        (\a dict ->
            Dict.get a dict
                |> Maybe.map (\count -> Dict.insert a (count + 1) dict)
                |> Maybe.withDefault (Dict.insert a 1 dict)
        )
        Dict.empty
        digits
        |> (Dict.values >> List.member 2)


neverDecreses : Password -> Bool
neverDecreses (Password digits) =
    List.foldl
        (\a ( previous, result ) ->
            if result && a >= previous then
                ( a, True )

            else
                ( a, False )
        )
        ( 0, True )
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
