module Lib.Solver exposing (Solver, Solvers, fromList, get, make, map, run)

import Dict as Dict exposing (fromList, get, map)
import Lib.Input exposing (Input)
import Lib.Output exposing (Output)


type Solver
    = Solver (Input -> Output)


make : (Input -> Output) -> Solver
make =
    Solver


run : Solver -> Input -> Output
run (Solver f) input =
    f input


type Solvers
    = Solvers (Dict.Dict String Solver)


fromList :
    List ( String, Solver )
    -> Solvers
fromList =
    Dict.fromList
        >> Solvers


get :
    String
    -> Solvers
    -> Maybe Solver
get key (Solvers dict) =
    Dict.get key dict


map :
    (String -> Solver -> a)
    -> Solvers
    -> List a
map f (Solvers dict) =
    Dict.toList dict
        |> List.map (\( a, b ) -> f a b)
