module Suite exposing (suite)

import Solvers.Day01 as Day01
import Solvers.Day02 as Day02
import Test exposing (Test, describe)


suite : Test
suite =
    describe "Advent Of Code 2019"
        [ describe "Day 01" Day01.tests
        , describe "Day 02" Day02.tests
        ]
