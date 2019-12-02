module Solvers.Day01 exposing (partOne, partTwo, tests)

import Expect
import Lib.Input as Input exposing (Input)
import Lib.Solver exposing (Solver, make)
import Test exposing (Test, describe, test)


partOne : ( String, Solver )
partOne =
    ( "Day 01, Part One"
    , make
        (inputToMassList
            >> Result.map
                (fuelRequired
                    >> fuelToString
                )
        )
    )


partTwo : ( String, Solver )
partTwo =
    ( "Day 01, Part Two"
    , make
        (inputToMassList
            >> Result.map
                (fuelRequiredIncludingAddedFuelMass
                    >> fuelToString
                )
        )
    )


tests : List Test
tests =
    [ describe "Part One"
        [ test "For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2" <|
            \() -> Expect.equal (calcFuel (Mass 12)) (Fuel 2)
        , test "For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2." <|
            \() -> Expect.equal (calcFuel (Mass 12)) (Fuel 2)
        , test "For a mass of 1969, the fuel required is 654" <|
            \() -> Expect.equal (calcFuel (Mass 1969)) (Fuel 654)
        , test "For a mass of 100756, the fuel required is 33583." <|
            \() -> Expect.equal (calcFuel (Mass 100756)) (Fuel 33583)
        ]
    , describe "Part Two"
        [ test "A module of mass 14 requires 2 fuel. This fuel requires no further fuel (2 divided by 3 and rounded down is 0, which would call for a negative fuel), so the total fuel required is still just 2." <|
            \() -> Expect.equal (calcFuelInclusive (Mass 12)) (Fuel 2)
        , test "At first, a module of mass 1969 requires 654 fuel. Then, this fuel requires 216 more fuel (654 / 3 - 2). 216 then requires 70 more fuel, which requires 21 fuel, which requires 5 fuel, which requires no further fuel. So, the total fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966" <|
            \() -> Expect.equal (calcFuelInclusive (Mass 1969)) (Fuel 966)
        , test "The fuel required by a module of mass 100756 and its fuel is: 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346" <|
            \() -> Expect.equal (calcFuelInclusive (Mass 100756)) (Fuel 50346)
        ]
    ]


type Mass
    = Mass Int


type Fuel
    = Fuel Int


fuelRequired :
    List Mass
    -> Fuel
fuelRequired =
    List.map calcFuel
        >> sumFuel


fuelRequiredIncludingAddedFuelMass :
    List Mass
    -> Fuel
fuelRequiredIncludingAddedFuelMass =
    List.map calcFuelInclusive
        >> sumFuel


calcFuel :
    Mass
    -> Fuel
calcFuel (Mass mass) =
    Fuel (max 0 ((mass // 3) - 2))


calcMassOfFuel :
    Fuel
    -> Mass
calcMassOfFuel (Fuel a) =
    Mass (a * 1)


calcFuelInclusive :
    Mass
    -> Fuel
calcFuelInclusive mass =
    case calcFuel mass of
        Fuel 0 ->
            Fuel 0

        fuel ->
            sumFuel
                [ fuel
                , calcMassOfFuel fuel
                    |> calcFuelInclusive
                ]


sumFuel :
    List Fuel
    -> Fuel
sumFuel =
    List.map (\(Fuel a) -> a)
        >> List.sum
        >> Fuel


fuelToString : Fuel -> String
fuelToString (Fuel a) =
    "Fuel: " ++ String.fromInt a


massFromString :
    String
    -> Result String Mass
massFromString a =
    String.toInt a
        |> Maybe.map Mass
        |> Result.fromMaybe ("Can't convert " ++ a ++ " in to a Int")


inputToMassList :
    Input
    -> Result String (List Mass)
inputToMassList =
    Input.toString
        >> String.lines
        >> List.map massFromString
        >> List.foldl
            (\resultMass result ->
                case ( resultMass, result ) of
                    ( _, Err err ) ->
                        Err err

                    ( Ok mass, Ok r ) ->
                        Ok <| mass :: r

                    ( Err err, _ ) ->
                        Err err
            )
            (Ok [])
