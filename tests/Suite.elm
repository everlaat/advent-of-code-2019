module Suite exposing (suite)

import Solutions.Day01 as Day01
import Test exposing (Test, describe)


suite : Test
suite =
    describe "Advent Of Code 2019"
        [ describe "Day 01" Day01.tests
        ]
