module Solutions.Day01 exposing (tests)

import Expect
import Test exposing (Test, describe, test)


tests : List Test
tests =
    [ describe "Part One"
        [ test "For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2" <|
            \() -> Expect.equal (fuelRequiredForMass (Mass 12)) (Fuel 2)
        , test "For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2." <|
            \() -> Expect.equal (fuelRequiredForMass (Mass 12)) (Fuel 2)
        , test "For a mass of 1969, the fuel required is 654" <|
            \() -> Expect.equal (fuelRequiredForMass (Mass 1969)) (Fuel 654)
        , test "For a mass of 100756, the fuel required is 33583." <|
            \() -> Expect.equal (fuelRequiredForMass (Mass 100756)) (Fuel 33583)
        , test "What is the sum of the fuel requirements for all (input) of the modules on your spacecraft?" <|
            \() ->
                Expect.equal (fuelRequired input)
                    -- Solution Part One
                    (Debug.log "Solution Day01 - part one" <| Fuel 3471229)
        ]
    , describe "Part Two"
        [ test "A module of mass 14 requires 2 fuel. This fuel requires no further fuel (2 divided by 3 and rounded down is 0, which would call for a negative fuel), so the total fuel required is still just 2." <|
            \() -> Expect.equal (fuelRequiredForMassIncludingAddedFuelMass (Mass 12)) (Fuel 2)
        , test "At first, a module of mass 1969 requires 654 fuel. Then, this fuel requires 216 more fuel (654 / 3 - 2). 216 then requires 70 more fuel, which requires 21 fuel, which requires 5 fuel, which requires no further fuel. So, the total fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966" <|
            \() -> Expect.equal (fuelRequiredForMassIncludingAddedFuelMass (Mass 1969)) (Fuel 966)
        , test "The fuel required by a module of mass 100756 and its fuel is: 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346" <|
            \() -> Expect.equal (fuelRequiredForMassIncludingAddedFuelMass (Mass 100756)) (Fuel 50346)
        , test "What is the sum of the fuel requirements for all of the modules on your spacecraft when also taking into account the mass of the added fuel?" <|
            \() ->
                Expect.equal (fuelRequiredIncludingAddedFuelMass input)
                    -- Solution Part Two
                    (Debug.log "Solution Day01 - part two" <| Fuel 5203967)
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
    List.map fuelRequiredForMass
        >> sumFuel


fuelRequiredIncludingAddedFuelMass :
    List Mass
    -> Fuel
fuelRequiredIncludingAddedFuelMass =
    List.map fuelRequiredForMassIncludingAddedFuelMass
        >> sumFuel


fuelRequiredForMass :
    Mass
    -> Fuel
fuelRequiredForMass (Mass mass) =
    Fuel (max 0 ((mass // 3) - 2))


fuelToMass :
    Fuel
    -> Mass
fuelToMass (Fuel a) =
    Mass (a * 1)


fuelRequiredForMassIncludingAddedFuelMass :
    Mass
    -> Fuel
fuelRequiredForMassIncludingAddedFuelMass mass =
    case fuelRequiredForMass mass of
        Fuel 0 ->
            Fuel 0

        required ->
            sumFuel
                [ required
                , fuelToMass required
                    |> fuelRequiredForMassIncludingAddedFuelMass
                ]


sumFuel :
    List Fuel
    -> Fuel
sumFuel =
    List.map (\(Fuel a) -> a)
        >> List.sum
        >> Fuel


massFromString :
    String
    -> Mass
massFromString =
    String.toInt
        >> Maybe.withDefault 0
        >> Mass


input : List Mass
input =
    List.map (massFromString << String.trim) <|
        String.lines
            """129192
58561
57267
95382
84995
127372
93598
97264
138550
79327
135661
139468
108860
149642
72123
128333
69002
98450
86267
70171
101333
79822
142539
142743
51371
111381
62073
72210
125168
135952
131060
121842
88234
146774
136571
126719
50644
75696
51195
77171
118052
83691
133779
149814
64847
110697
92695
59453
139517
129487
79271
97896
146987
149822
71866
90797
104732
54997
50139
134115
133017
144979
89428
124750
91833
57252
67195
121624
102706
138245
127700
124098
110382
121557
103613
133576
122801
112306
120203
134696
76129
84576
80854
147237
71025
127513
143631
125090
115698
57979
84880
120177
147389
88380
114688
56355
126265
58220
63523
130179"""
