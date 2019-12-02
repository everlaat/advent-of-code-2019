module Lib.Input exposing (Input, fromString, toString)


type Input
    = Input String


fromString :
    String
    -> Input
fromString =
    Input


toString :
    Input
    -> String
toString (Input a) =
    a
