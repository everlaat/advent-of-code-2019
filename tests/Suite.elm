module Suite exposing (suite)

import Solvers.Day01 as Day01
import Solvers.Day02 as Day02
import Solvers.Day03 as Day03
import Solvers.Day04 as Day04
import Test exposing (Test, describe)


suite : Test
suite =
    describe "Advent Of Code 2019"
        [ describe "Day 01" Day01.tests
        , describe "Day 02" Day02.tests
        , describe "Day 03" Day03.tests
        , describe "Day 04" Day04.tests
        ]
